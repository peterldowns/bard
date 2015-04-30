; Words are records of the following type:
(define-record-type word
	(make-word
    sym
    types
    num-syllables
    synonyms
    antonyms
    rhymes)
	word-record?
  (sym word-sym)
  (types word-types)
  (num-syllables word-num-syllables)
  (synonyms word-synonyms)
  (antonyms word-antonyms)
  (rhymes word-rhymes))
(define (word-str w) (string (word-sym w)))


(define (load-words file-name)
  (let ((vocabulary-table (make-strong-eq-hash-table))
        (pos-table (make-pos-table))
        (file-contents (load-file-contents file-name)))
    (for-each (lambda (line)
                (if (= 6 (length line))
                  (let ((word (apply make-word line)))
                    (add-word vocabulary-table pos-table word))
                  ; @Dang: some definitions are broken. Please fix!
                  (begin
                    (display "Ignoring definition: ")
                    (display line)
                    (newline)
                    'ignore-broken-content)))
              file-contents)
    (cons vocabulary-table pos-table)))

(define (make-pos-table)
  (let ((pos-table (make-equal-hash-table)))
    (for-each (lambda (pos) (hash-table/put! pos-table pos (make-equal-hash-table)))
              '(noun verb adverb adjective pronoun plural article))
    pos-table))

(define (add-word vocabulary pos-table new-word)
  (let ((key (word-sym new-word)))
    (hash-table/put! vocabulary key new-word)
    (for-each (lambda (pos)
                (let ((pos-index (hash-table/get pos-table pos #f)))
                  (if pos-index (hash-table/put! pos-index key new-word))))
              (word-types new-word))))

(define (load-file-contents file-name)
  (define (read-file file contents)
    (let ((next-line (read file)))
      (if (eof-object? next-line)
        contents
        (read-file file (cons next-line contents)))))
  (call-with-input-file
    file-name
    (lambda (f) (read-file f '()))))


(define word-dne (make-word 'word-dne 'dne 0 '() '() '()))
