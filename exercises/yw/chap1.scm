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
    (new-if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;; Ex 1.8
;; Making a procedure to solve cube roots
;; using Newton's method of successive approximations.

(define (square x) (* x x))
(define (cube x) (* x (square x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

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