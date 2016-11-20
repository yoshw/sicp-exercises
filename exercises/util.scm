#lang scheme

;; useful procedures
(provide square
         cube
         abs
         average
         cbrt
         even?
         divides?
         fast-expt
         fast-prime?
         inc
         deriv
         gcd
         nil)

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (average x y)
  (/ (+ x y) 2))

(define (cbrt x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
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
