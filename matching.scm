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
(define match-line (tagged-list 'line))
(define match-line? (tagged-list? 'line))
(define match-word (tagged-list 'word))
(define match-word? (tagged-list? 'word))

(define (contents match-cell)
  (cond ((poem? match-cell)
         (cdr match-cell))
        ((or (match-line? match-cell) (match-word? match-cell))
         (if (name-of match-cell)
           (cddr match-cell)
           (cdr match-cell)))
        (else (error "Unrecognized type of match-cell:" match-cell))))

(define (match-metadata match-cell)
  (if (pair? match-cell)
    (filter pair? match-cell)))

(define (name-of match-cell)
  (if (and (pair? match-cell)
           (> (length match-cell) 1))
    (let ((name (cadr match-cell)))
      ; Not all match-cells have names
      (if (symbol? name)
        name
        (let ((data-cell (assq 'name (match-metadata match-cell))))
          (if data-cell
            (cadr data-cell)
            #f))))

    #f))

(define (constraints-of match-cell)
  (if (poem? match-cell)
    (cdr match-cell)
    (filter
      (cond ((match-line? match-cell)
             (lambda (t) (or (string? t) (match-word? t))))
            ((match-word? match-cell)
             procedure?)
            (else (error "Unrecognized type of match-cell" match-cell)))
      match-cell)))
