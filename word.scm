; Words are records of the following type:
(define-record-type word
	(make-word
    sym
    type
    num-syllables
    synonyms
    antonyms
    rhymes)
	word-record?
  (sym word-sym)
  (type word-type)
  (num-syllables word-num-syllables)
  (synonyms word-synonyms)
  (antonyms word-antonyms)
  (rhymes word-rhymes))
(define (word-str w)
  (string (word-sym w)))

(define nouns (make-equal-hash-table))
(define verbs (make-equal-hash-table))
(define adverbs (make-equal-hash-table))
(define adjectives (make-equal-hash-table))
(define pronouns (make-equal-hash-table))
(define plural-pronouns (make-equal-hash-table))
(define articles (make-equal-hash-table))

(define (noun? pos)
  (eqv? 'noun pos))
(define (verb? pos)
  (eqv? 'verb pos))
(define (adverb? pos)
  (eqv? 'adverb pos))
(define (adjective? pos)
  (eqv? 'adjective pos))
(define (pronoun? pos)
  (eqv? 'pronoun pos))
(define (plural? pos)
  (eqv? 'plural pos))
(define (article? pos)
  (eqv? 'article? pos))

(define (addWord dictionary new-word)
  (hash-map/put dictionary (word-sym new-word) word))

(define (updateTables word)
  (let ((pos (word-pos word)))
    (for-each (lambda (x) 
                  (cond ((noun? x) (addWord nouns word))
                        ((verb? x) (addWord verbs word))
                        ((adjective? x) (addWord adjectives word))
                        ((pronoun? x) (addWord pronouns word))
                        ((plural? x) (addWord plural word))
                        ((article? x) (addWord articles word)))) pos)))

(define (load-file-contents file-name)
  (define (read-file file contents)
    (let ((next-line (read file)))
      (if (eof-object? next-line)
        contents
        (read-file file (cons next-line contents)))))
  (call-with-input-file
    file-name
    (lambda (f) (read-file f '()))))

(define (load-words file-name)
  (let ((table (make-strong-eq-hash-table))
        (file-contents (load-file-contents file-name)))
    (for-each (lambda (line)
                (if (= 6 (length line))
                  (let ((word (apply make-word line)))
                    (hash-table/put! table (word-sym word) word))
                    (updateTables word)
                  ; @Dang: some definitions are broken. Please fix!
                  (begin
                    (display "Ignoring definition: ")
                    (display line)
                    (newline)
                    'ignore-broken-content)))
              file-contents)
    table))

(define word-dne (make-word 'word-dne 'dne 0 '() '() '()))

