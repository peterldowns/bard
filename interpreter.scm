(load "word.scm")
(load "constraints.scm")
(load "grader.scm")
(load "matching.scm")
(load "pp.scm")
(load "util.scm")

(define (make-interpreter vocabulary grader)
  ; The interpreter is the outer-layer of interpretation. It loops over
  ; the given poem (list of line constraints) and solves them one by one.,
  ; while keeping track of state for named lines and words\.
  (define (interpreter poem)
    (let loop ((result '())
               (input-lines (match-constraints poem))
               (lines-alist '())
               (words-alist '()))
      (if (null? input-lines)
        result
        (let ((next-line (car input-lines))
              (remaining-lines (cdr input-lines)))
          ; One by one, each input line of the poem is parsed. Every parsing
          ; function accepts two continuation functions, one to call if the
          ; parsing function could come up with a valid result (the success
          ; continuation), and one to call if it could not (the failure
          ; continuation). These are simply functions that close over parser
          ; state, not scheme-level continuations, but the idea is analogous.
          (define (line-success-fn line-value new-lines-alist new-words-alist)
            ; On success, continue parsing.
            (loop (append result (list line-value))
                  remaining-lines
                  new-lines-alist
                  new-words-alist))
          (define (line-failure-fn)
            ; On failure, give up.
            (pp "Failed to parse a line!")
            (error "Unsolveable constraints" next-line))
          (parse-line-constraint next-line
                                 line-success-fn
                                 line-failure-fn
                                 lines-alist
                                 words-alist)))))

  (define (parse-line-constraint line-constraint
                                 succeed
                                 fail
                                 lines-alist
                                 words-alist)
    (let* ((name (match-name line-constraint))
           (syllables (constraint-syllables line-constraint))
           (words (match-constraints line-constraint))
           (existing-match (and name (assq name lines-alist)))
           (existing-value (and existing-match (cdr existing-match))))
      ; If this constraint has already been solved, continue by using the
      ; existing value. Otherwise, solve the constraint, store the result for
      ; later lookup if necessary, and continue with the newly solved value.
      (define (line-success-fn line-value
                               new-fail-fn
                               new-lines-alist
                               new-words-alist)
        ; Upon solving for a line, check that if there is a syllable
        ; constraint on the line it is matched. If that constraint is not
        ; matched, then call the failure function of this constraint, which
        ; will then go back and choose new values for the words.
        (if (or (not syllables)
                (= syllables (line-syllables line-value vocabulary)))
          (succeed line-value
                   (if name
                     ; Update the line association list if this is a named
                     ; line.
                     (cons (cons name line-value) new-lines-alist)
                     new-lines-alist)
                   new-words-alist)
          (new-fail-fn)))
      (if existing-value
        (succeed existing-value lines-alist words-alist)
        (parse-words-in-line words
                             line-success-fn
                             fail
                             lines-alist
                             words-alist))))

  (define (parse-words-in-line words
                               succeed
                               fail
                               lines-alist
                               words-alist)
    (define (loop result words succeed fail lines-alist words-alist)
      (if (null? words)
        ; If there are no more words, then succeed with the current result.
        (succeed result fail lines-alist words-alist)
        ; Otherwise, grab the next word and parse it.
        (let ((next-word (car words))
              (rest-of-words (cdr words)))
          (define (word-success-fn next-word-value
                                   new-fail-fn
                                   new-lines-alist
                                   new-words-alist)
            (loop (append result (list next-word-value))
                  rest-of-words
                  succeed
                  ; This is the tricky part -- if the next call to loop
                  ; fails, it will call the fail function gotten from this
                  ; call!
                  new-fail-fn
                  new-lines-alist
                  new-words-alist))
          ; If the next word is a match-word constraint cell, then parse it.
          ; Otherwise, it should be a literal (string or symbol); parse it and
          ; then continue looping.
          (if (match-word? next-word)
            (parse-word next-word word-success-fn fail lines-alist words-alist)
            (let ((new-result
                    (append
                      result
                      (list (cond
                              ((string? next-word) (symbol next-word))
                              ((symbol? next-word) next-word)
                              (else (error "Unrecognized word" next-word)))))))
              (loop new-result
                    rest-of-words
                    succeed
                    fail
                    lines-alist
                    words-alist))))))
    (loop '() words succeed fail lines-alist words-alist))

  (define (parse-word word-constraint
                      succeed
                      fail
                      lines-alist
                      words-alist)
    (let* ((name (match-name word-constraint))
           (constraints (match-constraints word-constraint))
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
        (parse-word-constraints constraints new-succeed fail))))

  (define (parse-word-constraints constraints succeed fail)
    (let loop ((possibilities (grader (fetch-words vocabulary constraints))))
      (if (null? possibilities)
        (fail)
        (let ((next-value (car possibilities))
              (remaining-values (cdr possibilities)))
          (succeed next-value
                   (lambda () (loop remaining-values)))))))

  interpreter)

; Future improvements:
; * wordnet (meanings)
; * not-rhyming (pattern 'a can't rhyme with 'b)
; * Pass constraints between lines, not just between words in a line.
