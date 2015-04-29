(define (load-words file-name string-lookup-table)
  (let* ((f (open-input-file file-name))
         (data (read f)))
    data))

(define f (open-input-file "vocabulary/wordsScraped.txt"))

 ;;TODO: Do things with words instead of printing
(define (readWords)
  (define (print-line line)
    (display (car line))
    (display "\n")
    (let ((new-line (read f)))
      (if new-line
	  (print-line new-line)
	  #f)))
  (print-line (read f)))

