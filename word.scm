(define-record-type word
	(make-word
    sym
    type
    num-syllables
    synonyms
    antonyms)
	word?
  (sym word-sym)
  (type word-type)
  (num-syllables word-num-syllables)
  (synonyms word-synonyms)
  (antonyms word-antonyms))

(define (load-file file-name)
  (define (read-file file contents)
    (let ((next-line (read file)))
      (if (eof-object? next-line)
        contents
        (begin
          (read-file file
                     (cons next-line contents))))))
  (let ((f (open-input-file file-name)))
    (read-file f '())))

(define (load-words file-name)
  (let ((table (make-strong-eq-hash-table))
        (file-contents (load-file file-name)))
    (for-each (lambda (line)
                (if (= 5 (length line))
                  (let ((word (apply make-word line)))
                    (hash-table/put! table (word-sym word) word))
                  'ignore-broken-content))
              file-contents)
    table))



;;Sample Usage
;(define x (make-word "sorrow" 2
;		     (list "sad" "melancholy")
;		     (list  "happy" "gay")))
;(word? x)
;;#t
;
;(word-spelling x)
;;"sorrow"
;
;(word-syllables x)
;;2
;
;(word-synonyms x)
;;("sad" "melancholy")
;
;(word-antonyms x)
;;("happy" "gay")
