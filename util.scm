(load "word.scm")

; Given a vocabulary, get a word from it that is most likely to represent the
; argument passed. This allows for string literals, symbols, and word records
; to all be used to represent words in poems.
(define (get-word vocabulary thing)
  (cond ((word-record? thing) thing)
        (else
          (hash-table/get
            vocabulary
            (if (symbol? thing)
                thing
                (symbol thing))
            word-dne))))

; Given a vocabulary and a list of constraints, filter the vocabulary and return
; only the word records that match every constraint.
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
    (every (lambda (constraint)
             (apply constraint test-args))
           constraints)))

; Given a list of words of the kind being built by the interpreter as it
; solves each line, return the number of syllables in the line.
(define (line-syllables line vocabulary)
  (define (word-syllables thing)
    (let ((word (get-word vocabulary thing)))
      (if (eq? word word-dne)
        (begin
          (pp "Unrecognized word:")
          (pp thing)
          0)
        (word-num-syllables word))))
  (reduce + 0 (map word-syllables line)))

; Given a constraint, return the data associated with the 'syllables metadata
; key. This is used by line constraints to express how many syllables they must
; have.
(define (constraint-syllables constraint)
  (let* ((metadata (match-metadata constraint))
         (syllables-data (and metadata (assq 'syllables metadata)))
         (syllables-count (and syllables-data (cadr syllables-data))))
    syllables-count))
