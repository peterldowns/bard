(load "word.scm")

(define (get-word vocabulary thing)
  (cond ((word-record? thing) thing)
        (else
          (hash-table/get
            vocabulary
            (if (symbol? thing)
                thing
                (symbol thing))
            word-dne))))

(define (fetch-words vocabulary constraints)
  (let ((results '())
        (constraint-fn (join-constraints constraints)))
    (define (filter-fn k w)
      (if (constraint-fn vocabulary w)
        (set! results (cons w results))))
    (hash-table/for-each vocabulary filter-fn)
    results))
(define (join-constraints constraints)
  (lambda test-args
    (every (lambda (constraint) (apply constraint test-args)) constraints)))

(define (line-value-syllables line vocabulary)
  (define (word-syllables thing)
    (let ((word (get-word vocabulary thing)))
      (if (word-record? word)
        (word-num-syllables word)
        (begin
          (pp "Unknown number of syllables for word:")
          (pp word)
          0))))
  (reduce + 0 (map word-syllables line)))

(define (syllables-of line-constraint)
  (let* ((metadata (match-metadata line-constraint))
         (syllables-data (and metadata (assq 'syllables metadata)))
         (syllables-count (and syllables-data (cadr syllables-data))))
    syllables-count))


