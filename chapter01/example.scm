(use-modules (ice-9 format))

;;; 1.1.1
;;; in-S? : N -> Bool
;;; usage: (in-S? n) = #t if n is in S, #f otherwise
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

(in-S? 6)
;; => #t
(in-S? 14)
;; => #f

;;; 1.2.1
;;; list-length : List -> Int
;;; usage: (list-length l) = the length of l
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

(list-length '(1 (a b) c))
;; => 3
(list-length '())
;; => 0


;;; 1.2.2
;;; nth-element: List x int -> SchemeVal
;;; usage: (nth-element lst n) = the n-th element of lst
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)       ; empty list is not allowed
        (if (zero? n)
            (car lst)                ; idx = 0, return the 1st element
            ;; else remove the head and shift idx to left by 1 (dec 1)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    ;; eopl:error procedure is not available in Guile I presume
    ;; so I used Guile's native exception
    (error 'nth-element
           (format #f "List too short by ~s elements.~%" (+ n 1)))))

(nth-element '(a b c d e) 3)
;; => d
(nth-element '(a b c d e) 8)


;;; 1.2.3
;;; remove-first : Sym × Listof(Sym) -> Listof(Sym)
;;; usage: (remove-first s los) returns a list with
;;;        the same elements arranged in the same
;;;        order as los, except that the first
;;;        occurrence of the symbol s is removed.
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            ;; s is 1st element of los
            (cdr los)
            ;; keep moving elements that are not s from los to a new lst by cons
            (cons (car los) (remove-first s (cdr los)))))))

(remove-first 5 '(5 6 7))
;; => (6, 7)
(remove-first 5 '(7 6 5))
;; => (7, 6)
(remove-first 5 '(1 2 3 4))
;; => (1 2 3 4)
