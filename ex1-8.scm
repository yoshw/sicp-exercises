; SICP EX1.8:
; Making a procedure to solve cube roots
; using Newton's method of successive approximations.

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