(load "interpreter.scm")

; Example
(define load-result (load-words "vocabulary/scrape.scm"))
(define vocab (car load-result))
(define pos-table (cdr load-result))
(define interpreter (make-interpreter vocab random-choice))

(define test-haiku (poem
                    (match-line 'a
                                '(syllables 5)
                                (match-word (any-word))
                                (match-word (any-word)))
                    (match-line 'b
                                '(syllables 7)
                                (match-word (rhymes-with "allay"))
                                (match-word (any-word)))
                    (match-line 'c
                                '(syllables 5)
                                (match-word (any-word))
                                (match-word (any-word)))))
#|
(print-poem (interpreter test-haiku))
; sauciness bayoneted
; crochet honoraria
; suffused overtook
|#

(define test-poem (poem
                    (match-line 'a
                                ;'(syllables 10)
                                (match-word 'y (has-synonym "learning"))
                                (match-word 'x (rhymes-with "investing")))
                    (match-line '(syllables 6)
                                (match-word (any-word)))
                    (match-line '(syllables 10)
                                (match-word (any-word))
                                (match-word (any-word)))
                    (match-line 'a)
                    (match-line (match-word 'x)
                                (match-word 'a))))
#|
(print-poem (interpreter test-poem))
; unlearning vesting
; mariposas
; dirigibles mariposas
; unlearning vesting
; vesting enunciates
|#
