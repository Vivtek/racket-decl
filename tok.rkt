#lang racket
(require racket/generator)

(provide
 match-bareword
 match-quote
 match-bracket
 match-sigil
 match-space
 match-plus

 closing-bracket
 
 make-token-skimmer
 make-tag-skimmer
 make-text-skimmer
 make-para-skimmer
 make-block-skimmer
 make-textplus-skimmer
)

;;
;; 2020-10-17
;; Bareword matcher
;;

(define (match-bareword line)
   (let ([probe (regexp-match #px"^([[:alnum:]_](:*[[:alnum:]!@#$%\\^&*=\\-+~_.,;\\|\\(\\)\\[\\]\\?<>{}])*)" line)])
      (if (not probe)
        #f
        (let* ([bareword  (list-ref probe 1)]
               [probe     (regexp-match #px"^(.*)\\p{P}+<<[[:alnum:]\\-<]*$" bareword)] ; One slight weirdness that is best handled in a separate stage:
               [bareword2 (if (not probe)                                              ; a sigil of the form ...<<EOF will match the first regex and be counted as a single bareword.
                             bareword                                                  ; So let's cleave it off if it's there and just keep the pre-sigil part.
                             (list-ref probe 1)
                          )
               ])
           (list (string-length bareword2) bareword2)
        )
      )
   )
)

;;
;; 2020-10-17
;; Quote matcher
;;

(define (match-quote line)
   (let ([probe (match-quote-single line)])
      (if probe probe (match-quote-double line)))
)

(define (match-quote-single line)
   (let ([probe (regexp-match #px"^('(?:\\\\.|[^'])*')" line)])
      (if (not probe)
        #f
        (let ([quote (list-ref probe 1)])
           (list (string-length quote) quote)
        )
      )
   )
)
(define (match-quote-double line)
   (let ([probe (regexp-match #px"^(\"(?:\\\\.|[^\"])*\")" line)])
      (if (not probe)
        #f
        (let ([quote (list-ref probe 1)])
           (list (string-length quote) quote)
        )
      )
   )
)

;;
;; 2020-10-17
;; Bracket matcher
;;

(define (match-bracket line)
  (let* ([len     (string-length line)]
         [bracket (if (> len 1) (string (string-ref line 0)) "")]
         [rest    (if (> len 1) (substring  line 1) "")]
         [closer  (closing-bracket bracket)])
     (if (not closer) #f
       (begin
        (let again ()
         (let ([probe (regexp-obliterate #px"('(?:\\\\.|[^'])*')" rest)])
           (if probe
             (begin
               (set! rest probe)
               (again)
             )
             rest
           )
          )   
          (let ([probe (regexp-obliterate #px"(\"(?:\\\\.|[^\"])*\")" rest)])
           (if probe
             (begin
               (set! rest probe)
               (again)
             )
             rest
           )
          )   
          (let ([probe (regexp-obliterate (bracket-regexp bracket) rest)])
           (if probe
             (begin
               (set! rest probe)
               (again)
             )
             rest
           )
          )   
        )
        ;(println (string-append "here with: '" rest "'"))
        (let ([probe (regexp-match (closer-regexp bracket) rest)])
          (if (not probe) #f
            (let ([len (+ (string-length (list-ref probe 1)) 2)])
              (list len (substring line 0 len))
            )
          )
        )
       )
     )
  )
)

(define (regexp-obliterate re string)
  (let* ([probe (regexp-match re string)]
         [mstr  (if probe (list-ref probe 1) #f)]
         [rre   (if mstr  (regexp (regexp-quote mstr)) #f)]
         [rep   (if mstr  (build-string (string-length mstr) (lambda (x) #\space)) #f)])
    (if probe
      (regexp-replace rre string rep)
      #f
    )
  )
)

(define (closing-bracket bracket)
  (cond
    [(equal? bracket "{") "}"]
    [(equal? bracket "(") ")"]
    [(equal? bracket "<") ">"]
    [(equal? bracket "[") "]"]
    [else #f]
  )
)

(define (bracket-regexp bracket)
  (cond
    [(equal? bracket "{") #px"(\\{(?:\\\\.|[^{}])*\\})"]
    [(equal? bracket "(") #px"(\\((?:\\\\.|[^\\(\\)])*\\))"]
    [(equal? bracket "<") #px"(<(?:\\\\.|[^<>])*>)"]
    [(equal? bracket "[") #px"(\\[(?:\\\\.|[^\\[\\]])*\\])"]
    [else #f]
  )
)
(define (closer-regexp bracket)
  (cond
    [(equal? bracket "{") #px"^(.*)\\}"]
    [(equal? bracket "(") #px"^(.*)\\)"]
    [(equal? bracket "<") #px"^(.*)>"]
    [(equal? bracket "[") #px"^(.*)\\]"]
    [else #f]
  )
)

;;
;; 2020-10-17
;; Sigil matcher
;;

(define (match-sigil line)
   (let ([probe (regexp-match #px"^([^[:alnum:][:space:]]+)([[:alnum:]\\-]*)" line)])
      (if (not probe)
        #f
        (let* ([sigil  (list-ref probe 1)]
               [tag    (list-ref probe 2)]
               [sigil2 (if (regexp-match #rx"<<$" sigil) (string-append sigil tag) sigil)])
           (list (string-length sigil2) sigil2)
        )
      )
   )
)

;
; 2020-10-17
; Space matcher
;
(define (match-space line)
   (let ([probe (regexp-match #px"^([[:space:]]+)" line)])
      (if (not probe)
        #f
        (let* ([space  (list-ref probe 1)])
           (list (string-length space) space)
        )
      )
   )
)

;
; 2020-10-21
; Plus matcher
;
(define (match-plus line)
   (let ([probe (regexp-match #px"^(\\+)[^\\s]" line)])
      (if (not probe)
        #f
        (list 1 "+")
      )
   )
)

;
; 2020-10-17
; First-line tokenizer
;

(define (make-tag-skimmer line-stream)      (make-token-skimmer line-stream 'tag))
(define (make-text-skimmer line-stream)     (make-token-skimmer line-stream 'text))
(define (make-para-skimmer line-stream)     (make-token-skimmer line-stream 'para))
(define (make-block-skimmer line-stream)    (make-token-skimmer line-stream 'block))
(define (make-textplus-skimmer line-stream) (make-token-skimmer line-stream 'textplus))

(define (make-token-skimmer line-stream mode)
  (letrec ([stop-on-blank   #f]
           [check-barewords #t]
           [check-quotes    #t]
           [check-brackets  #t]
           [check-sigils    #t]
           [check-plus      #f]
           
           [qindent 0]
           [qindmin 0]
           [lastlno 0]
           [blanks  0]
           [closer  #f]
           [blanks-before 0]
           [plus-quoted #f]
           [plus-indent 0]

           [starting-quote #f]
           [quoting    #f]
           [glom       #f]
           [glom-until #f]
           [first-line #t]

           [line-continues #f]
           [line-continued #f]

           [indentation-stack '()]
           [pop-frame (lambda (correction)
                         (yield (vector 'end (- lastlno blanks-before correction) qindmin 0 ""))
                         (set! qindmin (car indentation-stack))
                         (set! indentation-stack (cdr indentation-stack))
                         (set! closer #f)
                         (set! first-line #t)
                         (when (and (null? indentation-stack)
                                    (eq?   mode 'textplus))
                           (set! check-barewords #f)
                           (set! check-quotes    #f)
                           (set! check-brackets  #f)
                           (set! check-plus      #t)
                           (set! plus-quoted     #f)
                           (set! first-line      #t)
                           (set! starting-quote  #f)
                         )
                      )]
           [pop-frames-to-indent (lambda (indent)
                                   (unless (or (null? indentation-stack) (> indent qindmin))
                                     (pop-frame (if (eq? indent -1) 0 1))
                                     (when (< indent qindmin) (pop-frames-to-indent indent))
                                   )
                                 )]
        )
    (cond
       [(eq? mode 'text)
          (set! quoting #t)
          (set! glom #t)]
       [(eq? mode 'para)
          (set! stop-on-blank #t)]
       [(eq? mode 'block)
          (set! check-barewords #f)
          (set! check-quotes    #f)
          (set! check-brackets  #f)
          (set! stop-on-blank   #t)]
       [(eq? mode 'textplus)
          (set! check-barewords #f)
          (set! check-quotes    #f)
          (set! check-brackets  #f)
          (set! check-plus      #t)
          (set! stop-on-blank   #t)]
    )
    (generator ()
      (let next-line ()
        (let ([line (line-stream)])
          (unless line
             (pop-frames-to-indent -1)
             (let forever ()
               (yield #f)
               (forever))
          )
          (set! line-continued line-continues)
          (set! line-continues #f)
          (let* ([type   (vector-ref line 0)]
                 [lno    (vector-ref line 1)]
                 [indent (vector-ref line 2)]
                 [len    (vector-ref line 3)]
                 [text   (vector-ref line 4)]
                 [first    #t]
                 [non-name #f]
                 [textrest #f]
                 [push-frame (lambda (indmin)
                                (set! indentation-stack (cons qindmin indentation-stack))
                                (set! qindmin indmin)
                             )]
                 [push-frame-on-plus  (lambda (indent)
                                          (pop-frames-to-indent indent)
                                          (push-frame (+ indent 1))
                                          (yield (vector 'start lno indent 0 ""))
                                      )]
                 [push-frame-if-first (lambda (indent)
                                         (when first
                                            (set! first #f)
                                            (unless plus-quoted
                                               (pop-frames-to-indent indent)
                                               (push-frame (+ indent 1))
                                               (yield (vector 'start lno indent 0 "")))
                                         )
                                      )]
                )
            (when line-continued
              (set! first #f)
              (set! non-name #t)
            )
            (when (eq? type 'blank)
               (if (and stop-on-blank (not quoting) (not plus-quoted))
                   (pop-frame 1)
                   (set! blanks (+ blanks 1)))
               (set! first-line #t)
               (next-line))
            (when (and (eq? type 'line) (eq? mode 'para))
               (when first-line
                  (yield (vector 'tstart lno 0 0 ""))
                  (push-frame 0)
               )
               (set! first-line #f)
               (yield (vector 'text lno indent len text))
               (next-line))
            (when (and plus-quoted (< indent plus-indent))
               (pop-frames-to-indent indent)
            )
            (set! blanks-before blanks)
            (set! blanks 0)
            (set! lastlno lno)
            (let next-token ()
              (let ([probe (match-space text)])
                 (when probe
                     (let ([tlen (list-ref probe 0)]
                           [ttxt (list-ref probe 1)])
                       (set! indent (+ indent tlen))
                       (set! len    (- len    tlen))
                       (set! text   (substring text tlen))
                       (next-token)
                     )))
              (when textrest
                 (yield (vector 'text lno indent len text))
                 (next-line))
              (when starting-quote
                 (push-frame indent)
                 (yield (vector 'qstart lno indent 0 ""))
                 (set! quoting #t)
                 (set! starting-quote #f))
              (when (or quoting
                        (eq? mode 'text)
                        (eq? mode 'para))
                 (if glom
                   (begin
                      (when glom-until
                         (when (equal? text glom-until)
                            (set! glom #f)
                            (set! glom-until #f)
                            (pop-frame 1)
                            (next-line)
                         )
                      )
                      ;(let ([qtext (string-append (build-string indent (lambda (x) #\space)) text)])
                         (for ([i blanks-before])
                           (yield (vector 'text (- (- lno i) 1) 0 0 ""))
                         )
                         (yield (vector 'text lno indent len text))
                         (next-line)
                      ;)
                   )
                   (if (< indent qindmin)
                     (begin
                        (let ([closer-buffered closer])
                          (pop-frames-to-indent indent)
                          (set! quoting #f)
                          (when closer-buffered
                            (when (equal? closer-buffered text)
                              (yield (vector 'closer lno indent len text))
                              (next-line)
                            )
                          )
                        )
                     )
                     (begin
                        (for ([i blanks-before])
                           (yield (vector 'text (- (- lno i) 1) 0 0 ""))
                        )
                        (yield (vector 'text lno (- indent qindmin) len text))
                        (next-line)
                     )
                 )))
              (let ([probe (match-plus text)])
                 (when (and probe check-plus (not line-continued))
                     (let ([tlen (list-ref probe 0)]
                           [ttxt (list-ref probe 1)])
                       (push-frame-on-plus indent)
                       (yield (vector 'plus lno indent tlen ttxt))
                       (set! indent (+ indent tlen))
                       (set! len    (- len    tlen))
                       (set! text   (substring text tlen))
                       (set! check-barewords #t) ; Simulate mode 'tag until the stack pops off
                       (set! check-quotes    #t)
                       (set! check-brackets  #t)
                       (set! check-plus      #f)
                       (set! plus-quoted     #t)
                       (set! plus-indent     indent)
                       (next-token)
                     )))
              (let ([probe (match-bareword text)])
                 (when (and probe check-barewords)
                     (let ([tlen (list-ref probe 0)]
                           [ttxt (list-ref probe 1)]
                           [type (cond
                                   [first          'tag]
                                   [(not non-name) 'name]
                                   [else           'word]
                                 )])
                       (unless line-continued (push-frame-if-first indent))
                       (yield (vector type lno indent tlen ttxt))
                       (set! indent (+ indent tlen))
                       (set! len    (- len    tlen))
                       (set! text   (substring text tlen))
                       ;(set! first #f)
                       (next-token)
                     )))
              (let ([probe (match-bracket text)])
                 (when (and probe check-brackets)
                     (let ([tlen (list-ref probe 0)]
                           [ttxt (list-ref probe 1)])
                       (unless line-continued (push-frame-if-first indent))
                       (yield (vector 'bracket lno indent tlen ttxt))
                       (set! indent (+ indent tlen))
                       (set! len    (- len    tlen))
                       (set! text   (substring text tlen))
                       ;(set! first #f)
                       (set! non-name #t)
                       (next-token)
                     )))
              (let ([probe (match-quote text)])
                 (when (and probe check-quotes)
                     (let ([tlen (list-ref probe 0)]
                           [ttxt (list-ref probe 1)])
                       (unless line-continued (push-frame-if-first indent))
                       (yield (vector 'quote lno indent tlen ttxt))
                       (set! indent (+ indent tlen))
                       (set! len    (- len    tlen))
                       (set! text   (substring text tlen))
                       ;(set! first #f)
                       (set! non-name #t)
                       (next-token)
                     )))
              (let ([probe (match-sigil text)])
                 (when (and probe check-sigils)
                     (let* ([tlen (list-ref probe 0)]
                            [ttxt (list-ref probe 1)]
                            [line-cont (equal? ttxt "|")]
                            [type (cond
                                  [line-cont 'cont]
                                  [else      'sigil])])
                       (unless line-continued (push-frame-if-first indent))
                       (yield (vector type lno indent tlen ttxt))
                       (set! indent (+ indent tlen))
                       (set! len    (- len    tlen))
                       (set! text   (substring text tlen))
                       ;(set! first #f)
                       (set! non-name #t)
                       (set! textrest line-cont)
                       (set! closer (closing-bracket ttxt))
                       
                       (let ([probe (regexp-match #px"<<(.*)$" ttxt)])
                         (when probe
                            (set! len 0) ; Ignore anything else on the line
                            (set! glom #t)
                            (let ([mode (list-ref probe 1)])
                               (cond
                                  [(equal? mode "")        (set! glom-until "EOF")]
                                  [(not (equal? mode "<")) (set! glom-until mode)]
                               )
                            )
                         )
                       )

                       (unless line-cont
                         (set! starting-quote #t)
                         (set! blanks-before 0)
                         (when (eq? len 0) (next-line))
                       )
                       (when line-cont
                         (set! line-continues #t)
                       )
                       (next-token)
                     )))
             ; We have a line, but stuff is on it that is disabled - this means it's a text line in a text mode.
             ; So we handle this just as though we were in 'para mode.
             (when (> len 0)
               (when first-line
                  (yield (vector 'tstart lno 0 0 ""))
                  (push-frame 0)
               )
               (set! first-line #f)
               (yield (vector 'text lno indent len text))
               (next-line))
            )
          )
          (next-line)
        )
      )
    )
  )
)

