#lang scheme

;; SICP - CHAPTER 1

;; SEC 1.1.6 Conditional Expressions and Predicates

;; Ex 1.3
(define (square x) (* x x))

(define (sum-of-bigger-squares x y z)
  (cond ((and (<= x y) (<= x z)) (+ (square y) (square z)))
	((and (<= y z) (<= y x)) (+ (square x) (square z)))
	((and (<= z x) (<= z y)) (+ (square x) (square y)))))



;; SEC 1.1.7 Example: Square Roots by Newton's Method

;; Ex 1.7
;; When we use 'new-if' instead of if, the program hangs. This is because 'new-if' is a standard procedure,
;; meaning Scheme tries to evaluate all its arguments before evaluating the body.
;; Because sqrt-iter calls itself, new-if evaluates arguments forever, never evaluating
;; the cond in its body.

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

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
    (new-if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;; Ex 1.8
;; Making a procedure to solve cube roots
;; using Newton's method of successive approximations.

(define (cube x) (* x (square x)))

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



;; SEC 1.2.2 Tree Recursion

;; Ex 1.11
;; Write recursive and iterative procedures to compute
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3), n >= 3.

(define (f-rec n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	((= n 2) 2)
	(else (+ (f-rec (- n 1))
		 (* 2 (f-rec (- n 2)))
		 (* 3 (f-rec (- n 3)))))))

(define (f-iter n)
  (define (f-iterator a b c count)
    (if (= count 0)
	c
	(f-iterator (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (f-iterator 2 1 0 n))

;; Ex 1.12
;; Write a recursive procedure to compute the values of Pascal's Triangle.

(define (pascal-row n)
  (pascal-row-element n 0))

(define (pascal-row-element n x)
  (display (pascal-element n x))
  (display " ")
  (if (= n x)
      #f
      (pascal-row-element n (+ x 1))))

(define (pascal-element n x)
  (cond ((= x 0) 1)
	((= x n) 1)
	(else (+ (pascal-element (- n 1) x)
		 (pascal-element (- n 1) (- x 1))))))

;; SEC 1.2.4 Exponentiation

;; Ex 1.16
;; Design a procedure that evolves an interative exponentiation process
;; that uses successive squaring and uses a logarithmic number of steps.

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt b n)
  (define (fast-expt-iter b counter a)
    (display (* a b))
    (display " ")
    (cond ((= counter 0) a)
	  ((even? counter) (fast-expt-iter (square b) (/ counter 2) a))
	  (else (fast-expt-iter b (- counter 1) (* a b)))))
  (fast-expt-iter b n 1))

;; Ex 1.17

(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))

;; Ex 1.18

(define (faster-* a b)
  (fast-*-iter a b 0))

(define (fast-*-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-*-iter (double a) (halve b) sum))
        (else (fast-*-iter a (- b 1) (+ sum a)))))