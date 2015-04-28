
;; (poem)
(define (tagged-list? thing tag)
  (and (list? thing)
       (symbol? (car thing))
       (eq? tag (car thing))))

(define (poem? poem) (tagged-list? poem 'poem))
(define (line? line) (tagged-list? line 'line))
(define (word? word) (tagged-list? word 'word))

(define (word-type word) (cadr word))
(define (word-constraints (caddr word)))

(define (bard:get-variable name)
  (assq name bard:environment))

(define (bard:set-variable! name value)
  (set! bard:environment (cons (list name value) bard:environment)))

(define (bard:reset-environment)
  (set! bard:environment '()))

(define (generate-poem poem-template)
  (if (poem? poem-template)
    (begin
      (bard:reset-environment)
      (map generate-line (cdr poem-template)))
    '()))

(define (generate-line line-template)
  (if (line? line-template)
    (map generate-word (cdr line-template))
    '()))

(define (generate-word word-template)
  (cond ((symbol? word-template) word-template)
        ((word? word-template)
         (let ((constraints (generate-constraints (word-constraints word-template))))
           (select-word (word-type word-template) constraints)))
        (else 'NULL))

  (define (select-word type constraints)
    (let ((word (fetch-word type)))
      (let (constraint-values
             (map (lambda (p) (p word)) constraints))
        (if (every (lambda (v) (eq? v #t)) constraint-values)
          word
          (select-word type constraints)))))

  (define (rhyming-pattern name)
    (lambda (word)
      (let ((value (bard:get-variable name)))
        (if value
          ((ryhmes-with value) word)
          (begin
            (bard:set-variable! name word)
            #t)))))
