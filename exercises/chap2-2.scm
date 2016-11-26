#lang scheme

(require "util.scm")

;; SICP - CHAPTER 2

;; 2.2 Hierarchical Data and the Closure Property

;; SEC 2.2.1 Representing Sequences

;; Ex 2.17

; Return a list containing only the last element of a given (nonempty) list.
(define (last-pair lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-pair (cdr lst))))

;; Ex 2.18

; A procedure that takes a list as argument
; and returns a list of the same elements in reverse order.
(define (reverse lst)
  (define (iter items acc)
    (if (null? items)
        acc
        (iter (cdr items) (cons (car items) acc))))
  (iter lst '()))

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
(define (same-parity x . rest)
  (cons x (filter (lambda (y) (same-par? x y)) rest)))

(define (filter p lst)
  (cond ((null? lst)   '())
        ((p (car lst)) (cons (car lst) (filter p (cdr lst))))
        (else          (filter p (cdr lst)))))

;; Ex 2.21

(define (square-list items)
  (if (null? items)
      '()
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
  (cond ((null? items) true)
        (else (proc (car items))
              (my-for-each proc (cdr items)))
        ))

;; Ex 2.24
;; (1 (2 (3 4)))

;; Ex 2.25

(define (cadaddr x) (car (cdr (car (cdr (cdr x))))))
(define seven1 (cadaddr (list 1 3 (list 5 7) 9)))

(define (caar x) (car (car x)))
(define seven2 (caar (list (list 7))))

(define (cadadadadadadr x) (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x)))))))))))))
(define seven3 (cadadadadadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))

;; Ex 2.26
;; (1 2 3 4 5 6)
;; ((1 2 3) 4 5 6)
;; ((1 2 3) (4 5 6))

;; Ex 2.27
(define (leaf? t)
  (not (pair? t)))

(define (deep-reverse x)
  (define (iter lst acc)
    (cond ((null? lst)       acc)
          ((leaf? (car lst)) (iter (cdr lst) (cons (car lst) acc)))
          (else              (iter (cdr lst)
                                   (cons (deep-reverse (car lst)) acc)))
          ))
  (iter x '()))

;; Ex 2.28
(define (fringe t)
  (cond ((null? t)       '())
        ((leaf? (car t)) (cons (car t) (fringe (cdr t))))
        (else            (append (fringe (car t)) (fringe (cdr t))))
        ))

;; Ex 2.28
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

;; a
(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))

(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))

;; b
(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (branch-weight b)
  (let ((s (branch-structure b)))
    (if (pair? s)
        (total-weight s)
        s)
    )
  )

;; c
(define (balanced-mobile? m)
  (let ((lb (left-branch m))
        (rb (right-branch m))
        (lbl (branch-length (left-branch m)))
        (rbl (branch-length (right-branch m))))
    (and (= (* lbl (branch-weight lb))
            (* rbl (branch-weight rb)))
         (balanced-branch? lb)
         (balanced-branch? rb)
         )
    )
  )

(define (balanced-branch? b)
  (let ((s (branch-structure b)))
    (if (pair? s)
        (balanced-mobile? s)
        true)
    )
  )

;; d - only the selectors would need to change

;; Ex 2.30
(define (square-tree t)
  (cond ((null? t)       '())
        ((leaf? (car t)) (cons (square (car t))
                               (square-tree (cdr t))))
        (else            (cons (square-tree (car t))
                               (square-tree (cdr t))))
        )
  )

(define (square-tree2 t)
  (map (lambda (subt)
         (if (pair? subt)
             (square-tree2 subt)
             (square subt)))
       t))

;; Ex 2.31
(define (tree-map f t)
  (map (lambda (subt)
         (if (pair? subt)
             (tree-map f subt)
             (f subt)))
       t))

(define (square-tree3 t) (tree-map square t))

;; Ex 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (t) (cons (car s) t)) rest)))))
