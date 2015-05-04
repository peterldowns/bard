; Constraints
(load "word.scm")
(load "constraints.scm")
(load "grader.scm")
(load "matching.scm")
(load "pp.scm")

(define (line-value-syllables line vocabulary)
  (define (word-syllables thing)
    (let ((word (get-word vocabulary thing)))
      (if (word-record? word)
        (word-num-syllables word)
        (begin
          (pp "Unknown number of syllables for word:")
          (pp word)
          0))))
  (pp 'line)
  (pp line)
  (let ((words (map word-syllables line)))
    (pp 'word-syllables)
    (pp words)
    (reduce + 0 words)))

(define (syllables-of line-constraint)
  (let* ((metadata (match-metadata line-constraint))
         (syllables-data (and metadata (assq 'syllables metadata)))
         (syllables-count (and syllables-data (cadr syllables-data))))
    syllables-count))

(define (make-interpreter vocabulary grader)
  (define (interpreter poem)
    (let loop ((result '())
               (lines (contents poem))
               (lines-alist '())
               (words-alist '()))
      (if (null? lines)
        result
        (let ((next-line (car lines))
              (remaining-lines (cdr lines)))
          (parse-line next-line
                      (lambda (line-value new-lines-alist new-words-alist)
                        (loop (append result (list line-value))
                              remaining-lines
                              new-lines-alist
                              new-words-alist))
                      (lambda ()
                        (pp "Failed to parse a line!")
                        (error "Unsolveable constraints" next-line))
                      lines-alist
                      words-alist)))))
  (define (parse-line line-constraint succeed fail lines-alist words-alist)
    (let* ((name (name-of line-constraint))
           (syllables (syllables-of line-constraint))
           (words (constraints-of line-constraint))
           (existing-match (and name (assq name lines-alist)))
           (existing-value (and existing-match (cdr existing-match))))
      (pp "Line words")
      (pp words)
      (define (line-success line-value new-fail-fn new-lines-alist new-words-alist)
        (pp "New line value:")
        (pp line-value)
        (if (or (not syllables)
                (= syllables (line-value-syllables line-value vocabulary)))
            (succeed line-value
                     (if name
                       ; Update the line association list if this is a named
                       ; line.
                       (cons (cons name line-value) new-lines-alist)
                       new-lines-alist)
                     new-words-alist)
            (begin
              (pp "Syllable count was not correct:")
              (display "Required count: ")
              (display syllables)
              (newline)
              (display "Calculated: ")
              (display (line-value-syllables line-value vocabulary))
              (newline)
              (new-fail-fn))))
      (if existing-value
        (succeed existing-value lines-alist words-alist)
        (parse-words-in-line words line-success fail lines-alist words-alist))))

  (define (parse-words-in-line words succeed fail lines-alist words-alist)
    (define (impl result words succeed fail lines-alist words-alist)
      (if (null? words)
        (succeed result fail lines-alist words-alist)
        (let ((next-word (car words))
              (rest-of-words (cdr words)))
          (pp 'match-word)
          (pp next-word)
          (if (match-word? next-word)
            (parse-word next-word
                        ; Succeed should take a new (fail) fn, which chooses a new
                        ; word! This is the trick!
                        (lambda (next-word-value new-fail-fn new-lines-alist new-words-alist)
                          (let ((new-result (append result (list next-word-value))))
                            (impl new-result
                                  rest-of-words
                                  succeed
                                  ; This is the tricky part -- if the next call
                                  ; to impl fails, it will call the fail
                                  ; function gotten from this call!
                                  new-fail-fn
                                  new-lines-alist
                                  new-words-alist)))
                        fail
                        lines-alist
                        words-alist)
            (let ((new-result
                    (append result (list (cond
                                           ((string? next-word) (symbol next-word))
                                           ((symbol? next-word) next-word)
                                           (else (cons 'unrecognized next-word)))))))
              (impl new-result rest-of-words succeed fail lines-alist words-alist))))))
    (impl '() words succeed fail lines-alist words-alist))
  (define (parse-word word-constraint succeed fail lines-alist words-alist)
    (let* ((name (name-of word-constraint))
           (constraints (contents word-constraint))
           (existing-match (and name (assq name words-alist)))
           (existing-value (and existing-match (cdr existing-match))))
      (define (new-succeed value new-fail-fn)
        (succeed value
                 new-fail-fn
                 lines-alist
                 (if name
                     (cons (cons name value) words-alist)
                     words-alist)))
      (if existing-value
        (succeed existing-value fail lines-alist words-alist)
        (parse-word-constraints constraints
                                new-succeed
                                ; If the word constraints can't be met, fail?
                                ; Or, maybe succeed with a placeholder value?
                                (lambda ()
                                  (pp "Succeeding even though word failed to parse!")
                                  (pp constraints)
                                  (new-succeed #f fail))))))
  (define (parse-word-constraints constraints succeed fail)
    (let loop ((possibilities (grader (fetch-words vocabulary constraints))))
      (if (null? possibilities)
        (fail)
        (let ((next-value (car possibilities))
              (remaining-values (cdr possibilities)))
          (succeed next-value
                   (lambda () (loop remaining-values)))))))

  interpreter)

; Example
(define load-result (load-words "vocabulary/scrape.scm"))
(define test-vocabulary (car load-result))
(define test-pos-table (cdr load-result))
(define test-interpreter (make-interpreter test-vocabulary universal-grader))
(define test-word (match-word 'a (number-syllables 2)
                                 (rhymes-with "clicker")))
(define test-line (match-line 'b "more-literals"
                                 test-word
                                 (match-word (rhymes-with "corruption"))))
(define test-constraints (poem
                           (match-line '(name a)
                                       '(syllables 7)
                                       (match-word 'a (has-antonym "hungry"))
                                       "literal"
                                       (match-word 'x (has-synonym "indolent")))
                           (match-line test-word)
                           (match-line '(syllables 6) (match-word (any-word)))
                           (match-line '(syllables 8) (match-word (any-word))
                                                      (match-word (any-word)))
                           (match-line 'a)
                           (match-line (match-word 'x) (match-word 'a))
                           test-line))
(define test-poem (test-interpreter test-constraints))



#|
(define (choose-word state test-fn succeed exit)
  (define (loop state)
    (if (null? state)
      (exit)
      (let ((word (car state))
            (new-state (cdr state)))
        (test-fn word
                 succeed
                 (lambda ()
                   (loop new-state))))))
  (loop state))

(define (test-choice choice succeed fail)
  (pp "testing choice")
  (pp choice)
  (if (> choice 4)
    (begin
      (pp "-- Succeeded, calling succeed")
      (succeed choice))
    (begin
      (pp "-- Failed, calling fail!")
      (fail))))

(define (exit-fn)
  (pp 'failed)
  'failed)

(define (succeed-fn thing)
  (pp 'succeed!)
  (cons 'succeed thing))
|#
