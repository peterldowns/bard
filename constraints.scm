(load "word.scm")
; Constraint functions return functions of the following type:
; (lambda (vocabulary word-record)
;   ...
;   #t or #f),
; which return #t if the given word-record is considered to match the
; constraint or #f if it does not.


(define (number-syllables n)
  (lambda (vocabulary word)
    (= (word-num-syllables word) n)))


(define (has-synonym synonym)
  (lambda (vocabulary word)
    (if (memq synonym (word-synonyms word))
	#t
	#f)))


(define (has-antonym antonym)
  (lambda (vocabulary word)
    (if (memq antonym (word-antonyms word))
	#t
	#f)))


(define (format-word word)
  (let ((index (string-find-next-char word #\()))
    (if index
	(substring word 0 index)
	word)))


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
      (pp 'opening-port)
      (run-synchronous-subprocess "rhyme"
                                  (list (word-str word))
                                  'output
                                  port)
      (let ((result (filter (lambda (other)
                              (not (string=? other (word-str word))))
                            (split-output (get-output-string port)))))
        (pp 'closing-port)
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
