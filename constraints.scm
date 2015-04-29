(define (number-syllables n)
  (lambda (word)
    (= (word-syllables word) n)))

(define (has-synonym synonym)
  (lambda (word)
    (if (memq synonym (word-synonyms word))
	#t
	#f)))

(define (has-antonym antonym)
  (lambda (word)
    (if (memq antonym (word-antonyms word))
	#t
	#f)))

(define (format-word word)
  (let ((index (string-find-next-char word #\()))
    (if index
	(substring word 0 index)
	word)))

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

(define (get-rhymes word)
  (let ((port (open-output-string)))
    (begin
      (run-synchronous-subprocess "rhyme"
				  (list word)
				  'output
				  port)
      (filter (lambda (other) (not (string=? other word)))
	      (split-output (get-output-string port))))))

(define (rhymes-with other)
  (lambda (word)
    (let ((rhymes (get-rhymes word)))
      (let ((member? (member other rhymes)))
	(if member?
	    #t
	    #f)))))
