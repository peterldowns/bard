(define (universal-grader possibilities) possibilities)

(define (random-choice possibilities)
  (define (shuffle lst)
    (sort lst
	  (lambda (x y)
	    (equal? 0 (random 2)))))
  (shuffle possibilities))

(define (most-syllables possibilities)
  (define (compare-syllables word1 word2)
   (let ((syllables1 (word-num-syllables word1))
	 (syllables2 (word-num-syllables word2)))
     (> syllables1 syllables2)))
  (quick-sort possibilities compare-syllables))
