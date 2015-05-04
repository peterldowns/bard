;;; A grader is responsible for ordering a list of possible word records from
;;; most-desirable to least-desirable. These are a few possible
;;; implementations, although the interface is simple so other implementations
;;; are easy to write.

; Simply return the words in the order that they are in the dictionary (which
; isn't necessarily alphabetical, based on the way our dictionary file works.)
(define (dummy-grader possibilities)
  possibilities)

; Return words in a random order.
(define (random-choice possibilities)
  (define (shuffle lst)
    (sort lst (lambda (x y)
                (equal? 0 (random 2)))))
  (shuffle possibilities))

; Return words in order from most syllables first to fewest syllables last.
(define (most-syllables possibilities)
  (quick-sort possibilities (compare-syllables >)))
; Return words in order from least syllables first to most syllables last.
(define (least-syllables possibilities)
  (quick-sort possibilities (compare-syllables <)))
(define (compare-syllables fn)
  (lambda (word1 word2)
    (let ((syllables1 (word-num-syllables word1))
          (syllables2 (word-num-syllables word2)))
      (fn syllables1 syllables2))))
