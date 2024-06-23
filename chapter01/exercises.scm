;;; 1.8
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            ;; s is 1st element of los
            (cdr los)
            ;; keep discarding elements that are not s from los
            ;; if s is not in los or s is the last in los, return ()
            (remove-first s (cdr los))))))

(remove-first 5 '(5 6 7))
;; => (6, 7)
(remove-first 5 '(6 7 5))
;; => ()
(remove-first 1 '(6 7 5))
;; => ()


;;; 1.9
;;; usage: (remove s los)
;;;        remove all occurrences of s in los
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            ;; if s is 1st element of los, pop it and check cdr of los
            (remove s (cdr los))
            ;; else just keep moving 1st elements to another list
            (cons (car los) (remove s (cdr los)))))))

(remove 1 '(1 2 2 1 3))
;; => (2 2 3)
(remove 1 '())
;; => ()
(remove 1 '(2 3 4 5 6))
;; => (2 3 4 5 6)
