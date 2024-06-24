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


;;; 1.11
;;; the recursion guranteed to halt because (cdr slist) shorter than slist

;;; 1.12
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         ;; inline subst-in-s-exp
         (if (symbol? (car slist))
             (if (eqv? old (car slist)) new (car slist))
             (subst new old (car slist)))
         (subst new old (cdr slist)))
        )))

(subst 'a 'b '((b c) (b () d)))
;; => ((a c) (a () d))

;;; 1.13
;;; I don't know how to make (lambda (slst)) a separate fn
(define subst-v1
  (lambda (new old slst)
    (map
     (lambda (slst)
       (if (null? slst)
           ;; if empty lst then no replacement
           '()
           ;; if slst (S-exp) is Symbol, replace as necessary
           (cons
            (if (symbol? (car slst))
                (if (eqv? old (car slst)) new (car slst))
                (subst new old (car slst)))
            (subst new old (cdr slst)))))
     slst)))

(subst-v1 'a 'b '((b c) (b () d)))
;; => ((a c) (a () d))

;;; FIXME: the following code does not work
;;; because when mapping substx, it only accepts lists
(define subst-v2
  (lambda (new old slst)
    (map substx new old slst)))

(define (substx new old slst)
  (cons
   (if (null? slst)
      '()
      (if (symbol? (car slst))
          (if (eqv? old (car slst)) new (car slst))
          (substx new old (car slst))))
   (substx new old (cdr slst))))

(subst-v2 'a 'b '())
(subst-v2 'a 'b '((b c) (b () d)))
