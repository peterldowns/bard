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


(define (rhymes-with other)
  (lambda (vocabulary test)
    (define (get-word word-thing)
      (cond ((word-record? word-thing) word-thing)
            (else
              (hash-table/get
                vocabulary
                (if (symbol? word-thing)
                    word-thing
                    (symbol word-thing))
                word-dne))))
    (let* ((a (get-word other))
           (b (get-word test))
           (rhymes (get-rhymes a)))
      (member (word-str b) rhymes))))
(define (get-rhymes word)
  (let ((port (open-output-string)))
    (begin
      (run-synchronous-subprocess "rhyme"
                                  (list (word-str word))
                                  'output
                                  port)
      (let ((result (filter (lambda (other)
                              (not (string=? other (word-str word))))
                            (split-output (get-output-string port)))))
        (close-port port)
        result))))
(define (split-output output)
  (define (split-iter result current)
    (let ((index (string-find-next-char current #\SPACE)))
      (if (and index (> index 0))
	  (let ((word (format-word (substring current 0 index)))
		(rest (substring current (+ 1 index)
				 (string-length current))))
	    (split-iter (cons word result) rest))
	  (reverse result))))
  (split-iter '() output))
(define (format-word word)
  (let ((index (string-find-next-char word #\()))
    (if index
	(substring word 0 index)
	word)))
