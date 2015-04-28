; Functions for expressing the poem as a nested list of constraints.
(define (poem . args)
  (cons 'poem args))
(define (poem? cell)
  (and (pair? cell)
       (eq? 'poem (car cell))))
(define (poem-contents poem)
  (cdr poem))

(define (line . args)
  (cons 'line args))
(define (line? cell)
  (and (pair? cell)
       (eq? 'line (car cell))))
(define (line-contents line)
  (if (name-of line)
    (cddr line)
    (cdr line)))

(define (word . args)
  (cons 'word args))
(define (word? cell)
  (and (pair? cell)
       (eq? 'word (car cell))))

;
; Definition of the interpreter.
(define (name-of cell)
  (if (> (length cell) 1)
    (let ((name (cadr cell)))
      (if (symbol? name)
        name
        #f))
    #f))

(define (make-interpreter vocabulary grader)
  (define lines '())
  (define words '())
  (define (interpreter cell)
    (cond ((poem? cell) (map interpreter (poem-contents cell)))
          ((line? cell) (map interpreter (line-contents cell)))
          ((word? cell) #f)
          ((string? cell) cell)
          ((symbol? cell) (cons 'symbol cell))
          (else
            (list 'unrecognized cell))))
  interpreter)

;
; Constraint functions, which based on some interpreter state will either
; accept or reject a possibility.
(define (rhymes-with symbol)
  (lambda (state possibility)
    #t))

(define (has-tags . tags)
  (lambda (state possibility)
    #t))

(define test-interpreter (make-interpreter '() '()))
(define test-constraints (poem
                           (line 'a (word 'a (has-tags 'noun))
                                    "literal"
                                    (word (rhymes-with 'a)))
                           (line 'b "more literals"
                                    (word (rhymes-with 'a)))
                           (line (word (has-tags 'adverb))
                                 (word "we")
                                 (word (has-tags 'verb 'past-tense)))))
(define example (test-interpreter test-constraints))

; Helper for printing poems
(define (print-poem poem)
  (let loop ((poem poem)
             (depth -1))
    (let ((indent (make-list (max depth 0) "\t")))
      (for-each display indent))
    (for-each (lambda (item)
                (if (pair? item)
                  (loop item (+ depth 1))
                  (begin (display item)
                         (display " "))))
              poem)
    (newline)))
