; Constraints
(load "word.scm")
(load "constraints.scm")
(load "grader.scm")
(load "matching.scm")
(load "pp.scm")

(define (amb succeed . things) (amb-of things))
(define (amb-of succeed things)
  (let loop ((alts things))
    (if (null? alts)
      (fail)
      (succeed (lambda () (loop cdr alts))
               (car alts)))))


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
        (set! results (cons w results))))
    (hash-table/for-each vocabulary filter-fn)
    results))

; Example
;(define test-vocabulary (load-words "vocabulary/wordsScraped.txt"))
(define test-vocabulary (load-words "vocabulary/scrape.scm"))
(define test-interpreter (make-interpreter test-vocabulary random-choice))
(define test-word (match-word 'a (number-syllables 2) (rhymes-with "clicker")))
(define test-line (match-line 'b "more-literals"
                                 test-word
                                 (match-word (rhymes-with "corruption"))))
(define test-constraints (poem
                           (match-line 'a
                                       (match-word 'a (has-antonym "hungry"))
                                       "literal"
                                       (match-word 'x (has-synonym "indolent")))
                           (match-line test-word)
                           (match-line 'a)
                           (match-line (match-word 'x))
                           test-line
                           ))
(define test-poem (test-interpreter test-constraints))
