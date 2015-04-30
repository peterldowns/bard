; Constraints
(load "constraints.scm")
(load "word.scm")
(load "pp.scm")
(load "matching.scm")


(define (make-interpreter vocabulary grader)
  (define lines-table (make-strong-eq-hash-table))
  (define words-table (make-strong-eq-hash-table))
  (define (interpreter cell)
    (cond ((poem? cell)
           (map interpreter (contents cell)))
          ((match-line? cell)
           (let* ((name (name-of cell))
                  (existing-value (and name
                                       (hash-table/get lines-table name #f)))
                  (contents (contents cell)))
             (or existing-value
                 (let ((value (map interpreter contents)))
                   (hash-table/put! lines-table name value)
                   value))))
          ((match-word? cell)
           (let* ((name (name-of cell))
                  (existing-value (and name
                                       (hash-table/get words-table name #f)))
                  (constraints (constraints-of cell)))
             (or existing-value
                 (let ((value (grader (fetch-words vocabulary constraints))))
                   (hash-table/put! words-table name value)
                   value))))
          ; Strings and symbols are both literals.
          ((string? cell)
           (symbol cell))
          ((symbol? cell)
           cell)
          (else
            (list 'unrecognized cell))))
  interpreter)

(define (join constraints)
  (lambda test-args
    (every (lambda (constraint) (apply constraint test-args)) constraints)))
(define (fetch-words vocabulary constraints)
  (let ((results '())
        (constraint-fn (join constraints)))
    (define (filter-fn k w)
      (if (constraint-fn vocabulary w)
        (append! results (list w))))
    (hash-table/for-each vocabulary filter-fn)
    results))
(define (test-grader possibilities)
  (if (null? possibilities)
    'no-match-found
    (car possibilities)))


; Example
;(define test-vocabulary (load-words "vocabulary/wordsScraped.txt"))
(define test-vocabulary (load-words "dict.txt"))
(define test-interpreter (make-interpreter test-vocabulary test-grader))
(define test-word (match-word 'a (number-syllables 2) (rhymes-with "sag")))
(define test-line (match-line 'b "more-literals" test-word (match-word (rhymes-with "blue"))))
(define test-constraints (poem
                           test-line
                           (match-line 'a
                                       (match-word 'a (has-antonym 'foo))
                                       "literal"
                                       (match-word 'x (rhymes-with "cow"))
                                       (match-word 'x))))
(define test-poem (test-interpreter test-constraints))
