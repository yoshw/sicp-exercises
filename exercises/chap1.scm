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
;    (display (* a b))
;    (display " ")
    (cond ((= counter 0) a)
	  ((even? counter) (fast-expt-iter (square b) (/ counter 2) a))
	  (else (fast-expt-iter b (- counter 1) (* a b)))))
  (fast-expt-iter b n 1))

;; Ex 1.17

(define (doub x)
  (* x 2))
(define (halve x)
  (/ x 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (doub (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))

;; Ex 1.18

(define (faster-* a b)
  (fast-*-iter a b 0))

(define (fast-*-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-*-iter (doub a) (halve b) sum))
        (else (fast-*-iter a (- b 1) (+ sum a)))))

;; SEC 1.2.6 Example: Testing for Primality

;; Ex 1.21
;; Use the following smallest-divisor procedure to find
;; the smallest divisor of 199 [it's 199], 1999 [1999], and 19999 [7].

(define (smallest-divisor n)
  (find-divisor n 2))

;;(define (find-divisor n test-divisor)
;;  (cond ((> (square test-divisor) n) n)
;;        ((divides? test-divisor n) test-divisor)
;;        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

;; Ex 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

;;(define (start-prime-test n start-time)
;;  (if (prime? n)
;;      (report-prime (- (current-inexact-milliseconds) start-time))
;;      '()))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((even? a) (search-for-primes (+ a 1) b))
        ((< (- b a) 1)
         (newline)
         (display "All done."))
        (else
         (timed-prime-test a)
         (search-for-primes (+ a 2) b))))

;; Ex 1.23

;; Defining next to improve efficiency of smallest-divisor test
(define (next a)
  (if (= a 2)
      3
      (+ a 2)))

;; Redefining smallest-divisor to take advantage of 'next' procedure.
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;; Results:

;; 3 smallest primes > 1000 = 
;; 1009 (in 0.008056640625)
;; 1013 (in 0.008056640625)
;; 1019 (in 0.0087890625)

;; 3 smallest primes > 10000 = 
;; 10007 (in 0.02197265625)
;; 10009 (in 0.02294921875)
;; 10037 (in 0.02197265625)

;; 3 smallest primes > 100000 = 
;; 100003 (in 0.06884765625)
;; 100019 (in 0.06787109375)
;; 100043 (in 0.06787109375)

;; 3 smallest primes > 1000000 = 
;; 1000003 (in 0.2021484375)
;; 1000033 (in 0.197021484375)
;; 1000037 (in 0.197021484375)

;; This represents a speed increase factor of
;; approximately 1.44 in the first batch of tests,
;; 1.617 for the second batch, 1.658 for the third,
;; and almost 1.8 for the last batch.

;; While the new version of smallest-divisor is about 1.6
;; times faster than the old, we would have expected it to
;; twice as fast. How to account for this lower-than-expected
;; efficiency? Perhaps because (next test-divisor) takes one
;; step more than (+ test-divisor 1)? ASK SIMON

;; Exercise 1.24
;; Rewrite timed-prime-test to take advantage of fast-prime.

;;This is straightforward - we just change 'prime?' to 'fast-prime?'
;;(which I have to define first).

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

;; Now redefine start-prime-test ...

(define (start-prime-test n start-time)
  (if (fast-prime? n 50)
      (report-prime (- (current-inexact-milliseconds) start-time))
      '()))

;; As fast-prime has (log n) growth, we would expect the test times near 1,000,000
;; to take (log 1000000)/(log 1000) = 2 times as long as the test times near 1000.

;; Results:

;; 3 smallest primes > 1000 = 
;; 1009 (in 0.48388671875)
;; 1013 (in 0.5)
;; 1019 (in 0.52978515625)
;; (AVG = 0.5045572917)

;; 3 smallest primes > 10000 = 
;; 10007 (in 0.6259765625)
;; 10009 (in 0.60693359375)
;; 10037 (in 0.627197265625)
;; (AVG = 0.6200358073)

;; 3 smallest primes > 100000 = 
;; 100003 (in 0.694091796875)
;; 100019 (in 0.780029296875)
;; 100043 (in 0.7119140625)
;; (AVG = 0.7286783854)

;; 3 smallest primes > 1000000 = 
;; 1000003 (in 0.826904296875)
;; 1000033 (in 0.798095703125)
;; 1000037 (in 0.85302734375)
;; (AVG = 0.8260091146)

;; Average of tests near 1000000 / average of tests near 1000
;; = 0.8260091146 / 0.5045572917
;; =~ 1.637

;; This result departs significantly from our hypothesis of 2. Why
;; might the program be more efficient at higher figures than at
;; lower ones? ... I'm not sure! ASK SIMON!

;; Ex 1.27

(define (carms-fool n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (carm-test a)
    (cond ((= a n) true)
          ((try-it a) (carm-test (+ a 1)))
          (else false)))
  (carm-test 1))

;; (carms-fool 561) returns #t!
;; (carms-fool 1105) returns #t!
;; (carms-fool 1729) returns #t!
;; (carms-fool 2465) returns #t!
;; (carms-fool 2821) returns #t!
;; (carms-fool 6601) returns #t!

;; Ex 1.28

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (= (remainder (square (expmod2 base (/ exp 2) m))
                           m)
                1)
             0
             (remainder (square (expmod2 base (/ exp 2) m))
                        m)))
        (else
         (remainder (* base (expmod2 base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (carm-proof-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Successfully got this test to return false for
;; all the Carmichael numbers listed above -- but not
;; without some trying! Had to run "try-it" several thousand
;; times to get a negative result! But eventually got it for all of em.

;; SEC 1.3.1 Procedures as Arguments

;; Ex 1.29

(define (simpsons-int f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-sum f a b n k)
    (define (next a)
      (+ a h))
    (cond ((> k n) 0)
          ((or (= k 0) (= k n))
           (+ (f a)
              (simpsons-sum f (next a) b n (+ k 1))))
          ((even? k)
           (+ (* 2 (f a))
              (simpsons-sum f (next a) b n (+ k 1))))
          (else
           (+ (* 4 (f a))
              (simpsons-sum f (next a) b n (+ k 1))))))
  (* (simpsons-sum f a b n 0)
     (/ h 3)))

;; Ex 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Ex 1.31

;; a)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial a)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (product identity 1 next a))

(define (pi-approx limit)
  (define (next a) (+ a 2))
  (if (even? limit)
      (* (* 2 (- limit 1))
         (/ (product square 2.0 next (- limit 1))
            (product square 3.0 next limit)))
      (* (* 2 limit)
         (/ (product square 2.0 next (- limit 1))
            (product square 3.0 next limit)))))

;; b)

(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a)
         (product2 term (next a) next b))))

(define (factorial2 a)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (product2 identity 1 next a))

;; Ex 1.32

;; a)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (a-sum term a next b)
  (accumulate + 0 term a next b))

(define (a-product term a next b)
  (accumulate * 1 term a next b))

;; Now we can easily use a-sum to define sum-cubes, or a-product to define factorial.

;; b)

(define (accumulate2 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate2 combiner null-value term (next a) next b))))

;; Then, eg:

(define (a-product2 term a next b)
  (accumulate2 * 1 term a next b))

;; Ex 1.33

(define (filtered-accumulate filt? combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filt? a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

;; So then ...

(define (inc x) (+ x 1))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; SEC 1.3.2 Constructing Procedures Using lambda

;; Ex 1.34

(define (f g)
  (g 2))
;; Calling (f f) evaluates to (f 2) which returns
;; an error because 2 is not a procedure.


;; SEC 1.3.3 Procedures as General Methods

;; Ex 1.35

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (golden-ratio n)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) n)) ;; ... Success!

;; Ex 1.36

(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; First, without average damping:

(define (x2x-is-k n)
  (verbose-fixed-point (lambda (x) (/ (log 1000) (log x))) n))

;; Then, WITH average damping:

(define (damp-x2x-is-k n)
  (verbose-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) n))

;; Without damping: 34 steps to get within tolerance.
;; With damping: 10 steps to get within tolerance.
;; We have a pretty clear winner!

;; Ex 1.37 (k-term finite continued fractions)

;; a)

(define (cont-frac n d k)
  (define (cont-frac-count i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-count (+ i 1))))))
  (cont-frac-count 1))

;; 1/(golden ratio) = golden ratio - 1 = 2/(1 + (sqrt 5)) = 0.6180339888 or so.
;; My (cont-frac) function is accurate to 4 decimal places when k = 11 or greater.

;; b) (this time, in iterative)

(define (cont-frac2 n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
        result
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))

;; Ex 1.38 (approximations to e)

(define (approx-e k)
  (define (d i)
    (if (= 0 (remainder (+ i 1) 3))
        (* 2 (/ (+ i 1) 3))
        1))
  (+ 2
     (cont-frac (lambda (i) 1.0)
                 d
                 k)))

;; Ex 1.39 (continued frac. approximation to tan)

(define (tan-cf x k)
    (define (n i)
    (if (= 1 i)
        x
        (- (square x))))
     (cont-frac n
                (lambda (i) (- (* 2 i) 1))
                k))

;; SEC 1.3.4 Procedures as Returned Values

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; Ex 1.40 (cubic)

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Which allows us to define:

(define (cubic-zero a b c)
  (newtons-method (cubic a b c) 1))

;; Ex 1.41 (double)

(define (double f)
  (lambda (x)
    (f (f x))))

;; Using this procedure, (((double (double double)) inc) 5) returns 21.
;; As it should.

;; Ex 1.42 (compose)

(define (compose f g)
  (lambda (x)
    (f (g x))))

;; Ex 1.43 (repeated function)

(define (repeated f n)
  (define (repeat-iter a b i)
    (if (= i n)
        (lambda (x) (a x))
        (repeat-iter (compose a b) b (+ i 1))))
  (repeat-iter f f 1))

;; Ex 1.44 (smoothed function)

(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3)))

;; To get the n-fold smoothed function f(x), I think
;; you just run (((repeated smooth n) f) x).

;; Ex 1.45

(define (average-damp f)
  (lambda (x) (average x (f x))))

; Require 1 average-damp:

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cbrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; Require 2 average-damps:
(define (4thrt x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y)))))
               1.0))
(define (5thrt x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y (cube y))))))
               1.0))
(define (6thrt x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* (square y) (cube y))))))
               1.0))
(define (7thrt x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (square (cube y))))))
               1.0))

; Require 3 average-damps:
(define (8thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (* y (square (cube y))))))))
               1.0))
(define (9thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (* (square y) (square (cube y))))))))
               1.0))
(define (10thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (cube (cube y)))))))
               1.0))
(define (11thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (* y (cube (cube y))))))))
               1.0))
(define (12thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (* (square y) (cube (cube y))))))))
               1.0))
(define (13thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (* (cube y) (cube (cube y))))))))
               1.0))
(define (14thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (* y (cube y) (cube (cube y))))))))
               1.0))
(define (15thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp (lambda (y) (/ x (* (square y) (cube y) (cube (cube y))))))))
               1.0))

; Require 4 average-damps:
(define (16thrt x)
  (fixed-point (average-damp
                (average-damp
                 (average-damp
                  (average-damp
                   (lambda (y) (/ x (* (cube y) (cube y) (cube (cube y)))))))))
               1.0))

; SO, THEN:

(define (2pow n)
  (define (2pow-iter i p)
    (if (> p n)
        i
        (2pow-iter (+ 1 i) (* 2 p))))
  (2pow-iter 0 2))

(define (nth-rt n x)
  (fixed-point
   ((repeated average-damp (2pow n))
    (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0))

;; Ex 1.46 (iterative improvement)

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))
    (iter x)))

(define (sqrt3 x)
  (let ((good-enough-sqrt? (lambda (guess) (< (abs (- (square guess) x)) 0.0001)))
        (improve-sqrt (lambda (guess) (average guess (/ x guess)))))
    ((iterative-improve good-enough-sqrt? improve-sqrt) 1)))

(define (new-fixed-point f init)
  (let ((good-enough-fp? (lambda (guess) (< (abs (- (f guess) guess)) 0.00001)))
        (improve-fp (lambda (guess) (f guess))))
    ((iterative-improve good-enough-fp? improve-fp) init)))

;; THUS ENDS CHAPTER 1!