#lang scheme

;; SICP - CHAPTER 2

;; Preliminary - useful procedures:

(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (cube x) (* x x x))

(define (cbimp x y)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cbrt x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (cbimp x guess))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
	guess
	(cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt b n)
  (define (fast-expt-iter b counter a)
    (cond ((= counter 0) a)
	  ((even? counter) (fast-expt-iter (square b) (/ counter 2) a))
	  (else (fast-expt-iter b (- counter 1) (* a b)))))
  (fast-expt-iter b n 1))

(define (divides? a b)
  (= (remainder b a) 0))

(define (inc x) (+ x 1))

;; Fermat test for primes:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define nil (list))


;; SEC 2.1.1 Example: Arithmetic Operations for Rational Numbers

; Ex 2.1

;(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; SEC 2.1.2 Abstraction Barriers

;; Ex 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment x)
  (make-point
   (average (x-point (start-segment x))
            (x-point (end-segment x)))
   (average (y-point (start-segment x))
            (y-point (end-segment x)))))

;; Ex 2.3

(define (make-rect l w)
  (let ((a (make-point 0 0))
        (b (make-point l 0))
        (c (make-point 0 w)))
    (cons (make-segment a b)
          (make-segment a c))))

(define (rect-length x) (car (cdr (car x))))
(define (rect-width x) (cdr (cdr (cdr x))))

(define (rect-perimeter x)
  (* 2
     (+ (rect-length x)
        (rect-width x))))

(define (rect-area x)
  (* (rect-length x)
     (rect-width x)))

;; Now, a different representation of rectangles in a plane:

(define (make-rect2 l w origin)
  (cons origin (make-point l w)))

(define (rect-length2 x) (car (cdr x)))
(define (rect-width2 x) (cdr (cdr x)))

;; Using this alternate representation, rect-area and rect-perimeter
;; still work as long as you substitute rect-width2 and rect-length2.

;; And for kicks ...

(define (seg-length x)
  (let ((a (abs (- (x-point (end-segment x)) (x-point (start-segment x)))))
        (b (abs (- (y-point (end-segment x)) (y-point (start-segment x))))))
    (cond ((= 0 a) b)
          ((= 0 b) a)
          (else (sqrt (+ (square a) (square b)))))))

;; SEC 2.1.3 What is Meant by Data?

;; Ex 2.4

; In exercise book.

;; Ex 2.5

;; Procedure which takes a number and returns the highest power of 2
;; which factors the number.
(define (find-2-power x)
  (define (find-2-power-iter power)
    (if (not (even? (/ x (fast-expt 2 power))))
        power
        (find-2-power-iter (+ 1 power))))
  (find-2-power-iter 0))

;; Ditto for powers of 3.
(define (find-3-power x)
  (define (find-3-power-iter power)
    (if (not (divides? 3 (/ x (fast-expt 3 power))))
        power
        (find-3-power-iter (+ 1 power))))
  (find-3-power-iter 0))

;; Putting it together to make the pair constructors and selectors
(define (exp-cons x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(define (exp-car x)
  (find-2-power x))

(define (exp-cdr x)
  (find-3-power x))

;; Ex 2.6

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; SEC 2.1.4 Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;(define (mul-interval x y)
;  (let ((p1 (* (lower-bound x) (lower-bound y)))
;        (p2 (* (lower-bound x) (upper-bound y)))
;        (p3 (* (upper-bound x) (lower-bound y)))
;        (p4 (* (upper-bound x) (upper-bound y))))
;    (make-interval (min p1 p2 p3 p4)
;                   (max p1 p2 p3 p4))))

;; Ex 2.7

(define (make-interval a b)
  (cons a b))
(define (upper-bound x)
  (cdr x))
(define (lower-bound x)
  (car x))

;; Ex 2.8

;; The difference between two intervals X = [a, b] and Y = [c, d]
;; will be an interval ranging from the difference between the lower bound
;; of Y and upper bound of X (or c - b), to the difference between
;; the upper bound of Y and the lower bound of X (or d - a). Thus:

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Ex 2.9

;; "Show that the width of the sum (or difference) of two intervals
;; is a function only of the widths of the intervals being added (or subtracted)."

;Well, let interval A = [3, 19] and interval B = [4, 10].
;Then width(A) = 8 and width(B) = 3. If we add A and B,
;we get interval A+B = [7,29]. Width(A+B) = 11, which is
;obviously equal to width(A) + width(B). Likewise if we subtract
;A from B, we get interval B-A = [-15,7], with width(B-A) = 11 = width(B+A).

;Multiplication and division are a different story, however.
;This time let A = [8,16] (width 4) and B = [5,9] (width 2).
;Multiplying these gives interval AB = [40,144], with width 104.
;This does not appear to be a function of 4 and 2. If we let
;B = [7,11], we get AB = [56,176] with width 120. With the same
;argument widths, we have a totally different product width.
;So product width is not a function of argument widths.

;Likewise for quotient width. With our first set of intervals
;B/A = [(7/16),(11/8)], with width of (15/32). In the second case,
;B/A = [(5/16),(9/8)], with width of (13/32).

;; Ex 2.10

(define (spans-zero? x)
  (and (< 0 (upper-bound x))
       (> 0 (lower-bound x))))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Denominator must not span 0." y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Ex 2.11

(define (mul-interval x y)
  (let ((xlo (lower-bound x))
        (xup (upper-bound x))
        (ylo (lower-bound y))
        (yup (upper-bound x)))
    (cond ((and (> xlo 0)
               (> xup 0)
               (> ylo 0)
               (> yup 0))
           ; [+,+] * [+,+]
           (make-interval (* xlo ylo) (* xup yup)))
          ((and (> xlo 0)
               (> xup 0)
               (< ylo 0)
               (> yup 0))
           ; [+,+] * [-,+]
           (make-interval (* xup ylo) (* xup yup)))
          ((and (> xlo 0)
               (> xup 0)
               (< ylo 0)
               (< yup 0))
           ; [+,+] * [-,-]
           (make-interval (* xup ylo) (* xlo yup)))
          ((and (< xlo 0)
               (> xup 0)
               (> ylo 0)
               (> yup 0))
           ; [-,+] * [+,+]
           (make-interval (* xlo yup) (* xup yup)))
          ((and (< xlo 0)
               (> xup 0)
               (< ylo 0)
               (< yup 0))
           ; [-,+] * [-,-]
           (make-interval (* xup ylo) (* xlo ylo)))
          ((and (< xlo 0)
               (< xup 0)
               (> ylo 0)
               (> yup 0))
           ; [-,-] * [+,+]
           (make-interval (* xlo yup) (* xup ylo)))
          ((and (< xlo 0)
               (< xup 0)
               (< ylo 0)
               (> yup 0))
           ; [-,-] * [-,+]
           (make-interval (* xlo yup) (* xlo ylo)))
          ((and (< xlo 0)
               (< xup 0)
               (< ylo 0)
               (< yup 0))
           ; [-,-] * [-,-]
           (make-interval (* xup yup) (* xlo ylo)))
          ((and (< xlo 0)
               (> xup 0)
               (< ylo 0)
               (> yup 0))
           ; [-,+] * [-,+]
           (let ((p1 (* xlo ylo))
                 (p2 (* xlo yup))
                 (p3 (* xup xlo))
                 (p4 (* xup yup)))
             (make-interval (min p1 p2 p3 p4)
                            (max p1 p2 p3 p4)))))))

;; Ex 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (/ (* c p) 100.0))
                 (+ c (/ (* c p) 100.0))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

;; Ex 2.13

(define (int-prod-tolerance x y)
  (let ((p (percent x))
        (q (percent y)))
    (/ (* 10000 (+ p q))
       (+ 10000 (* p q)))))

;; Ex 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Skipping 2.14 - 2.16 for now.

;; 2.2 Hierarchical Data and the Closure Property

;; SEC 2.2.1 Representing Sequences

;; Ex 2.17

; Return a list containing only the last element of a given (nonempty) list.
(define (last-pair list1)
  (if (null? (cdr list1))
      (list (car list1))
      (last-pair (cdr list1))))

;; Ex 2.18

; A procedure that takes a list as argument
; and returns a list of the same elements in reverse order.
(define (reverse list1)
  (define (reverse-iter inl outl)
    (if (null? inl)
        outl
        (reverse-iter (cdr inl) (cons (car inl) outl))))
  (reverse-iter list1 (list)))

;; Ex 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define au-coins (list 200 100 50 20 10 5))

(define (count-change amount)
  (cc amount au-coins))
(define (cc amount coin-values)
  (define (first-denomination a)
    (car a))
  (define (except-first-denomination a)
    (cdr a))
  (define (no-more? a)
    (null? a))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; Ex 2.20

; Simple predicate to test whether two integers have matching parity.
(define (same-par? a b)
  (if (even? a)
      (even? b)
      (not (even? b))))

; Main procedure to return a list of all arguments
; that match parity of first argument, x.
(define (same-parity x . y)
  (define (iter a b)
    (cond ((null? a)
           b)
          ((same-par? x (car a))
           (iter (cdr a) (append b (list (car a)))))
          (else
           (iter (cdr a) b))))
  (iter y (list x)))

;; Ex 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

;; Ex 2.22

; Louis's problem is one I myself have experienced - quite recently.
; The problem is that because we operate on the 'car' of each pair in the
; list, the answer list is built from the first item to the last. (A recursive
; procedure of course avoids this by burrowing down to the last item before
; it starts to actually cons anything to the answer list.)

; If, as in the second part of this problem, you reverse the arguments to cons,
; you end up with a bunch of nested lists, eg ((((1) . 2) . 3) . 4) instead of
; (1 2 3 4). This is because instead of consing an integer to the front of a list,
; you're consing a list to the front of an integer, which just creates a pair.
; This is where 'append' comes in handy.

;; Ex 2.23

(define (my-for-each proc items)
  (define (iter list)
    (cond ((null? list) #f)
          (else
           (proc (car list))
           (iter (cdr list)))))
  (iter items))