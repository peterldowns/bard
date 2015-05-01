(define (universal-grader possibilities)
  possibilities)
(define (first-pick possibilities)
  (if (null? possibilities)
    #f
    (car possibilities)))

(define (random-choice possibilities)
  (if (null? possibilities)
    #f
    (let* ((count (length possibilities))
           (index (random count))
           (item (list-ref possibilities index)))
      item)))

(define (most-syllables possibilities)
  (if (null? possibilities)
    #f
    (let loop ((longest #f)
               (possibilities possibilities))
      (let ((test (car possibilities))
            (remaining (cdr possibilities)))
        (if (null? remaining)
          longest
          (loop
            (if (or (not longest)
                    (> (word-num-syllables test)
                       (word-num-syllables longest)))
              test
              longest)
            remaining))))))
