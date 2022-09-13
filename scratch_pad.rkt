#lang racket

;(list 1 2 3 4)
;(map sqrt (list 1 2 3 4))
;(length (list 1 2 3 4))

(define my-map
  (lambda (f xs)
    (if (null? xs)
    null
    (cons (f (car xs))
          (my-map f (cdr xs))))))

;(my-map sqrt (list 1 2 3 4))

;----------------------------------

(define my-filter
  (lambda (p? xs)
    (if (null? xs)
        null
        (if (p? (car xs))
            (cons (car xs)
                  (my-filter p? (cdr xs)))
            (my-filter p? (cdr xs))))))

;(my-filter even? (list 1 2 3 4))

;----------------------------------
; factor out repeated code with let

(define my-filter2
  (lambda (p? xs)
    (if (null? xs)
        null
        (let ((ys (my-filter2 p? (cdr xs))))
        (if (p? (car xs))
            (cons (car xs)
                  ys)
            ys)))))

;(my-filter2 even? (list 1 2 3 4))

;----------------------------------
; utility function

(define maybe-cons
  (lambda (did-pass head tail)
    (if did-pass (cons head tail) tail)))

(define my-filter3
  (lambda (p? xs)
    (if (null? xs)
        null
        (maybe-cons (p? (car xs))
                (car xs)
                (my-filter3 p? (cdr xs))))))

;----------------------------------
; does not work just right

(define add-ten
  (lambda (x)
    (+ x 10)))


(define maybe-cadr
  (lambda (f xs)
    (if (> 1 (length xs))
        (car xs)
        (cons (f (car xs))
              (cadr xs)))))

;(define map-skip
;  (lambda (f xs)
;    (if (null? xs)
;        null
;        (if (> 1 (length xs))
;            (cons (f (car xs))
;                  (map-skip (cdr xs)))
;            (cons (f (car xs)) (cadr xs))
;                   (map-skip f (cdr (cdr xs)))))))

(define frist-two-elements
  (lambda (f xs)
    (cons (f (car xs)) (cadr xs))))

(define map-skip
  (lambda (f xs)
    (if (null? xs)
        null
        (if (< (length xs) 2)
            (cons (car xs) (map-skip f (cdr xs)))
            (cons (frist-two-elements f xs) (map-skip f (cdr (cdr xs))))))))


;(map-skip add-ten (list 1 2 3 4 5 6))
;(map-skip (lambda(x)(+ 1000 x)) '(1 2 3 4 5 6 7))
;(map-skip add-ten '(1 2 3 4 5 6 7))


;----------------------------------
; working

(define map-skip-helper1
  (lambda (f xs n)
    (if (null? xs)
        null
        (if (even? n)
            (cons (f (car xs))
                  (map-skip-helper1 f (cdr xs) (+ n 1)))
            (cons (car xs)
                  (map-skip-helper1 f (cdr xs) (+ n 1)))))))

(define map-skip1
  (lambda (f xs)
    (map-skip-helper1 f xs 0)))

(map-skip1 (lambda(x)(+ 1000 x)) '(1 2 3 4 5 6 7))
(map-skip1 add-ten '(1 2 3 4 5 6 7))

;----------------------------------
; working and code cleaned up a litle

(define map-skip-helper2
  (lambda (f xs n)
    (if (null? xs)
        null
        (let ((y (map-skip-helper2 f (cdr xs) (+ n 1))))
        (if (even? n)
            (cons (f (car xs))
                  y)
            (cons (car xs)
                  y))))))

(define map-skip2
  (lambda (f xs)
    (map-skip-helper2 f xs 0)))

(display "----------------------------\n")
(map-skip2 (lambda(x)(+ 1000 x)) '(1 2 3 4 5 6 7))
(map-skip2 add-ten '(1 2 3 4 5 6 7))

;----------------------------------
;

;(define next-letter
;  (lambda (num vals)
;    (if (= num 0)
;        (car vals)
;        (next-letter (- num 1) vals))))

(define next-letter
  (lambda (num vals)
    (if (> num (length vals))
        #f
        (if (= num 0)
            (car vals)
            (next-letter (- num 1)
                         (cdr vals))))))

(define scatter-gather
  (lambda (ind vals)
    (if (null? ind)
        null
        (cons (next-letter (car ind) vals)
              (scatter-gather (cdr ind) vals)))))

(display "----------------------------\n")
(scatter-gather '(0 1 4 1 1 7 2) '(a b c d e))

;----------------------------------
;

(define cross-map-helper
  (lambda (f x ys)
    (if (null? ys)
        null
        (cons (f x (car ys))
              (cross-map-helper f x (cdr ys))))))

(define cross-map
  (lambda (f xs ys)
    (if (null? xs)
        null
        (cons (cross-map-helper f (car xs) ys)
              (cross-map f (cdr xs) ys)))))

(display "----------------------------\n")
(cross-map - '(100 200 300) '(4 3 2 1))

;----------------------------------
; 

(define pass
  (lambda (p? xs)
    (if (null? xs)
    null
    (if (p? (car xs))
        (cons (car xs)
              (pass p? (cdr xs)))
        (pass p? (cdr xs))))))

(define not-pass
  (lambda (p? xs)
    (if (null? xs)
    null
    (if (not (p? (car xs)))
        (cons (car xs)
              (not-pass p? (cdr xs)))
        (not-pass p? (cdr xs))))))

(define tear
  (lambda (p? xs)
    (list (pass p? xs)
          (not-pass p? xs))))

(display "----------------------------\n")
(tear number? '(a b c 1 2 3 d e f))

;----------------------------------
; contains a bug

; case 1 - xs is null
; case 2 - ys is null
; case 3 - nither is null

;(define sub-list
;  (lambda (l n)
;    (if (= n 0)
;        (cdr l)
;        (sub-list (cdr l) (- n 1)))))

(define sub-list
  (lambda (l n)
    (if (null? l)
        null
        (if (= n 0)
            (cdr l)
            (sub-list (cdr l) (- n 1))))))

(define next-elements
  (lambda (l n)
    (if (= n 0)
        null
        (cons (car l)
              (next-elements (cdr l) (- n 1))))))

(define foo-helper
  (lambda (xs ys n)
    (cond  [(null? xs) (cons ys
                             null)]
           [(null? ys) (cons xs
                             null)]
           [(odd? n)   (cons (next-elements xs n)
                             (foo-helper (sub-list xs n) ys (+ n 1)))]
           [else (cons (next-elements ys n)
                       (foo-helper xs (sub-list ys n) (+ n 1)))] ) ))

(define foo
  (lambda (xs ys)
    (foo-helper xs ys 1)))

(display "----------------------------\n")
(foo '(a b c d e f g) '(aa bb cc dd ee ff gg))
(foo '(a b c d e f g) '())
(foo '() '(aa bb cc dd ee ff gg))

;----------------------------------
; 

;(define tr
;  (lambda (l)
;    (if (null? l)
;        null
;        ())))

;----------------------------------
; WORKING
(define sub-list2
  (lambda (f x ys)
    (if (null? ys)
        null
        (cons (f x (car ys))
                (sub-list2 f x (cdr ys))))))

(define cross-map2
  (lambda (f xs ys)
    (if (null? xs)
        null
        (append (sub-list2 f (car xs) ys)
                (cross-map2 f (cdr xs) ys)))))

(display "----------------------------\n")
(cross-map2 - '(100 200 300) '(4 3 2 1))

;----------------------------------
; WORKING

(define scatter-gather-helper2
  (lambda (x ys)
    (cond [(= x 0) (car ys)]
          [(null? ys) #f]
          [else (scatter-gather-helper2 (- x 1) (cdr ys))])))

(define scatter-gather2
  (lambda (xs ys)
    (if (null? xs)
        null
        (cons (scatter-gather-helper2 (car xs) ys)
              (scatter-gather2 (cdr xs) ys)))))

(display "----------------------------\n")
(scatter-gather2 '(0 1 4 1 1 7 2) '(a b c d e))

;----------------------------------
; WORKING

;(define tear2
;  (lambda (p xs)
;    (if (null? xs)
;        null
;        (list (tear-sub2 p (car xs))
;              (tear-sub2 p )))))