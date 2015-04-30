; Constraints
(load "constraints.scm")
(load "word.scm")

; Functions for expressing the poem as a nested list of constraints.
(define (tagged-list tag)
  (lambda elements (cons tag elements)))
(define (tagged-list? tag)
  (lambda (thing)
    (and (list? thing)
         (symbol? (car thing))
         (eq? tag (car thing)))))

(define poem (tagged-list 'poem))
(define poem? (tagged-list? 'poem))
(define line (tagged-list 'line))
(define line? (tagged-list? 'line))
(define word (tagged-list 'word))
(define word? (tagged-list? 'word))

(define (contents thing)
  (cond ((poem? thing)
         (cdr thing))
        ((or (line? thing) (word? thing))
         (if (name-of thing)
           (cddr thing)
           (cdr thing)))
        (else (error "Unrecognized type of thing:" thing))))

; Definition of the interpreter.
(define (name-of cell)
  (if (and (pair? cell)
           (> (length cell) 1))
    (let ((name (cadr cell)))
      ; Not all cells have names
      (if (symbol? name)
        name
        #f))
    #f))

(define (constraints-of cell)
  (if (pair? cell)
    (if (name-of cell)
      (cddr cell)
      (cdr cell))
    (error "Not a cell:" cell)))

(define (make-interpreter vocabulary grader)
  (define lines-table (make-strong-eq-hash-table))
  (define words-table (make-strong-eq-hash-table))
  (define (interpreter cell)
    (cond ((poem? cell)
           (map interpreter (contents cell)))
          ((line? cell)
           (let* ((name (name-of cell))
                  (existing-value (and name
                                       (hash-table/get lines-table name #f)))
                  (contents (contents cell)))
             (or existing-value
                 (let ((value (map interpreter contents)))
                   (hash-table/put! lines-table name value)
                   value))))
          ((word? cell)
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
  (let ((results '()))
    (define (filter-fn _ w)
      (if ((join constraints) vocabulary w)
        (set! results (cons w results))))
    (hash-table/for-each vocabulary filter-fn)
    results))
(define (test-grader possibilities)
  (if (null? possibilities)
    #f
    (car possibilities)))


; Helper for printing poems
(define (print-poem poem)
  (let loop ((poem poem)
             (depth -1))
    (let ((indent (make-list (max depth 0) "\t")))
      (for-each display indent))
    (for-each (lambda (item)
                (if (list? item)
                  (loop item (+ depth 1))
                  (begin (display
                           (if (word-record? item) (word-str item) item))
                         (display " "))))
              poem)
    (newline)))


; Example
(define test-vocabulary (load-words "vocabulary/wordsScraped.txt"))
(define test-vocabulary (load-words "dict.txt"))
(define test-interpreter (make-interpreter test-vocabulary test-grader))
(define test-word (word 'a (number-syllables 2) (rhymes-with "cow")))
(define test-line (line 'b "more-literals" test-word (word (rhymes-with "blue"))))
(define test-constraints (poem
                           (line 'a (word 'a (has-antonym 'foo))
                                 "literal"
                                 (word (rhymes-with "cow")))))
(define test-poem (test-interpreter test-constraints))

(newline)(newline)
(print-poem test-poem)
