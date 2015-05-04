(load "interpreter.scm")

; Example
(define load-result (load-words "vocabulary/scrape.scm"))
(define test-vocabulary (car load-result))
(define test-pos-table (cdr load-result))
(define test-interpreter (make-interpreter test-vocabulary random-choice))
(define test-word (match-word 'a (number-syllables 2)
                                 (rhymes-with "clicker")))
(define test-line (match-line 'b "more-literals"
                                 test-word
                                 (match-word (rhymes-with "corruption"))))
(define test-constraints (poem
                           (match-line 'a
                                       '(syllables 7)
                                       (match-word 'a (has-antonym "hungry"))
                                       "literal"
                                       (match-word 'x (has-synonym "indolent")))
                           (match-line test-word)
                           (match-line '(syllables 6) (match-word (any-word)))
                           (match-line '(syllables 8) (match-word (any-word))
                                                      (match-word (any-word))
                                                      (match-word (any-word)))
                           (match-line 'a)
                           (match-line (match-word 'x) (match-word 'a))
                           test-line))
(define (test)
  (let ((test-poem (test-interpreter test-constraints)))
    test-poem))
