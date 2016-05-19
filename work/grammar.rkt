#lang racket
(require redex)
(provide L)

(define-language L 
  ;; Definition statements
  (D (define χ e)
     (define (χ ((χ τ) ...) e)))
  ;; expressions
  (e (e_1 e_2)
     (e)
     (λ ((χ τ) ...) e)
     χ
     n
     m
     b
     l
     (if0 e e e)
     (+ e e))
  ;; expression w/ contract
  (m (contract-out contract e))
  ;; contract
  (contract (-> pred pred))
  ;; types
  (τ (-> τ τ ...)
     (Listof τ)
     num
     bool)
  (pred integer?
        any/c
        (-> pred pred)
        τ
        (not pred)
        (and pred pred)
        (or pred pred)
        (pred2 e)
        (λ ((χ τ) ...) e))
  (pred2 <=/c
         >=/c
         </c
         >/c
         =/c)
  ;; environments
  (Γ ((χ τ) ... Γ)
     ·)
  ;; racket value representations
  (l (cons e l)
        empty)
  (n number)
  (b boolean)
  (n+ (side-condition (name n+ n) (> (term n+) 0)))
  ;; other variables
  (χ variable-not-otherwise-mentioned)
  ;; error -- used to prevent invalid forms
  (err error))










#|

        int?
        pos?
        zero?
        >
        <
        >=
        <=
        =
Predicates

num?
int?
real?
pos?
zero?

bool?
>
<
<=
>=
=?

true?
not
or
and

  ;; applications
  #;(E (E e) 
       (v E) 
       (+ E e) 
       (+ v E) 
       (if0 E e e) 
       hole)

|#
            