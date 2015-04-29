(define-record-type word
	(make-word spelling syllables synonyms antonyms)
	word?
	(spelling word-spelling)
	(syllables word-syllables)
	(synonyms word-synonyms)
	(antonyms word-antonyms))

;Sample Usage
(define x (make-word "sorrow" 2
		     (list "sad" "melancholy")
		     (list  "happy" "gay")))
(word? x)
;#t

(word-spelling x)
;"sorrow"

(word-syllables x)
;2

(word-synonyms x)
;("sad" "melancholy")

(word-antonyms x)
;("happy" "gay")




