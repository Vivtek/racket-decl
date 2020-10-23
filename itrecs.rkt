#lang racket
(require racket/generator)

(provide make-reader
         make-file-reader
         make-string-reader
         
         itrecs-load)

;;
;; 2020-10-14
;; Line reader for a file or string
;;

(define (make-string-reader input) (make-reader (open-input-string input)))
(define (make-file-reader   input) (make-reader input))

(define (make-reader file)
   (unless (input-port? file)
           (set! file (open-input-file file)))
   (port-count-lines! file)
   (generator ()
      (let next ()
         (let* ((line_no (get-line-number file))
                (line    (read-line file)))
            (if (string? line)
               (yield (prepare-line line_no line))
               (begin
                  (close-input-port file)
                  (let forever () (yield #f) (forever))
               )
            )
         )
         (next)
      )
   )
)

(define (prepare-line line_no line)
   (let* ([il     (regexp-match #px"(\\s*)(.*)" line)]
          [rawind (string-length (list-ref il 1))]
          [text   (string-trim (list-ref il 2))]
          [tlen   (string-length text)]
          [indent (if (> tlen 0) rawind 0)]
          [type   (if (> tlen 0) 'line 'blank)])
      (vector type line_no indent tlen text)
   )
)

(define (get-line-number port)
   (let-values ([(lno col pos) (port-next-location port)])
      lno
   )
   ;(call-with-values
   ;   (lambda () (port-next-location port))
   ;   (lambda (lno col pos) lno)
   ;)
)

;;
;; 2020-10-16
;; List loader for any itrecs generator
;;

(define (itrecs-load it)
   (for/list ([rec (in-producer it #f)])
      rec)
)

