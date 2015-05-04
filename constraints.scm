(load "word.scm")
; Constraint functions return functions of the following type:
; (lambda (vocabulary word-record)
;   ...
;   #t or #f),
; which return #t if the given word-record is considered to match the
; constraint or #f if it does not.

; TODO(peter): pass in line and word lookup tables? Strings -> literal words,
; symbols -> lookup in table.
; TODO(peter): line constraints?


(define (get-word vocabulary thing)
  (cond ((word-record? thing) thing)
        (else
          (hash-table/get
            vocabulary
            (if (symbol? thing)
                thing
                (symbol thing))
            word-dne))))

(define (number-syllables n)
  (lambda (vocabulary thing)
    (let ((word (get-word vocabulary thing)))
      (= n (word-num-syllables word)))))

(define (any-word)
  (lambda (vocabulary test-thing)
    (let ((test (get-word vocabulary test-thing)))
      (not (eq? test word-dne)))))

(define (has-synonym base-thing)
  (lambda (vocabulary test-thing)
    (let ((requirement (get-word vocabulary base-thing))
          (test (get-word vocabulary test-thing)))
      (memq (word-sym requirement) (word-synonyms test)))))


(define (has-antonym base)
  (lambda (vocabulary test)
    (let ((word (get-word vocabulary base))
          (antonym (get-word vocabulary test)))
      (memq (word-sym antonym) (word-antonyms word)))))


(define (rhymes-with base)
  (lambda (vocabulary test)
    (let ((word (get-word vocabulary base))
          (possibility (get-word vocabulary test)))
      (memq (word-sym possibility) (word-rhymes word)))))

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
