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
;    (display (* a b))
;    (display " ")
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