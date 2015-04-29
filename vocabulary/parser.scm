(define f (open-input-file "wordsScrapped.txt"))
 
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
 
(readWords)