(define (square x) (* x x))

(define (sum-of-bigger-squares x y z)
  (cond (and (<= x y) (<= x z)) (+ (square y) (square z))
	(and (<= y z) (<= y x)) (+ (square x) (square z))
	(and (<= z x) (<= z y)) (+ (square x) (square y))))