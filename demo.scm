(load "interpreter.scm")

; Example
(define load-result (load-words "vocabulary/scrape.scm"))
(define test-vocabulary (car load-result))
(define test-pos-table (cdr load-result))
(define test-interpreter (make-interpreter test-vocabulary dummy-grader))
(define test-word (match-word 'a (number-syllables 2)
                                 (rhymes-with "clicker")))
(define test-line (match-line 'b "more-literals"
                                 test-word
                                 (match-word (rhymes-with "corruption"))))
(define test-constraints (poem
                           (match-line 'a
                                       ;'(syllables 10)
                                       (match-word 'y (has-synonym "learning"))
                                       (match-word 'x (rhymes-with "investing")))
                           (match-line test-word)
                           (match-line '(syllables 6)
                                       (match-word (any-word)))
                           (match-line '(syllables 10)
                                       (match-word (any-word))
                                       (match-word (any-word)))
                           (match-line 'a)
                           (match-line (match-word 'x)
                                       (match-word 'a))
                           test-line))
(define (test)
  (let ((test-poem (test-interpreter test-constraints)))
    test-poem))
