#lang racket
(require redex
         "common.rkt"
         "extend-lookup.rkt")

(define-extended-language Assignments Lambda
  (e ::= .... n + (void) (set! x e))
  (n ::= natural))

(define-extended-language ImperativeCalculus Assignments
  (e ::= .... (letrec ((x e) ...) e)))




(define-extended-language IC-s ImperativeCalculus
  (E ::= hole (v ... E e ...) (set! x E))
  (σ ::= ((x v) ...))
  (v ::= n + (void) (lambda (x ...) e)))

#;(define s->βs
  (reduction-relation
   Assignments-s
   #:domain (e σ)
   (--> [(in-hole E x) σ]
        [(in-hole E (lookup σ x)) σ])
   (--> [(in-hole E (set! x v)) σ]
        [(in-hole E (void)) (extend σ (x) (v))])
   (--> [(in-hole E (+ n_1 n_2)) σ]
        [(in-hole E ,(+ (term n_1) (term n_2))) σ])
   (--> [(in-hole E ((lambda (x ..._n) e) v ..._n)) σ]
        [(in-hole E e) (extend σ (x_new ...) (v ...))]
        (where (x_new ...) ,(variables-not-in (term σ) (term (x ...)))))))
