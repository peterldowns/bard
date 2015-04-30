(load "word.scm")

; Helper for printing poems
(define (print-poem poem)
  (let loop ((poem poem)
             (depth -1))
    (let ((indent (make-list (max depth 0) "\t")))
      (for-each display indent))
    (for-each
      (lambda (item)
        (if (list? item)
          (loop item (+ depth 1))
          (begin
            (display (if (word-record? item) (word-str item) item))
            (display " "))))
      poem)
    (newline)))
