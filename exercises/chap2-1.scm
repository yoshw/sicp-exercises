#lang scheme

(require "util.scm")

;; SICP - CHAPTER 2

;; 2.1 Introduction to Data Abstraction

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
