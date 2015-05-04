; Functions for expressing the poem as a nested list of constraints. These
; tagged lists have three optional componenents: names, metadata, and
; constraints.
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

; Metadata are expressed as tuples. The results of this function will be
; an alist (or an empty list if there is no metadata).
(define (match-metadata match-cell)
  (if (pair? match-cell)
    (filter (lambda (x)
              (and (pair? x)
                   (= 2 (length x))))
            match-cell)
    (error "Unrecognized type of match-cell" match-cell)))

; Names are expressed in metadata as a ('name <value>) cell, or by a symbol
; present as the second item in the tagged list (first after the name.) For
; example, both of the following cells have the same name:
;   (match-word 'a ...)
;   (match-word ... '(name a) ...)
; If the name does not exist, the function returns #f.
(define (match-name match-cell)
  (if (pair? match-cell)
    (if (> (length match-cell) 1)
      (let ((name (cadr match-cell)))
        (if (symbol? name)
          name
          (let ((data-cell (assq 'name (match-metadata match-cell))))
            (if data-cell
              (cadr data-cell)
              #f))))
      #f)
    (error "Unrecognized type of match-cell" match-cell)))

; Constraints are expressed as functions (that may match multiple objects),
; tagged lists, or literals, depending on the type of match-cell in which the
; constraints are present.
(define (match-constraints match-cell)
  (if (poem? match-cell)
    (cdr match-cell)
    (filter
      (cond ((match-line? match-cell)
             (lambda (t) (or (string? t) (match-word? t))))
            ((match-word? match-cell)
             procedure?)
            (else (error "Unrecognized type of match-cell" match-cell)))
      match-cell)))
