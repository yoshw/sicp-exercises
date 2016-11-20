#lang scheme

(require "util.scm")

;; SICP - CHAPTER 2

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
