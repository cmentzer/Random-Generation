#lang racket

(provide
 ;; language 
 TLambda-tc

 ;; (extend Î“ (x t) ...) add (x t) to Î“ so that x is found before other x-s
 extend

 ;; (lookup Î“ x) retrieves x's type from Î“
 lookup)

(require redex)

;; -----------------------------------------------------------------------------
(define-language TLambda-tc
  (e ::= n + x (lambda ((x_!_ t) ...) e) (e e ...))
  (n ::= natural)
  (t ::= int (t ... -> t))
  (Î“ ::= ((x t) ...))
  (x ::= variable-not-otherwise-mentioned))

(define tlambda? (redex-match? TLambda-tc e))

;; -----------------------------------------------------------------------------
;; (extend Î“ (x t) ...) add (x t) to Î“ so that x is found before other x-s

(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))

(define-metafunction TLambda-tc
  extend : Î“ (x any) ... -> any
  [(extend ((x_Î“ any_Î“) ...) (x any) ...) ((x any) ...(x_Î“ any_Î“) ...)])

;; -----------------------------------------------------------------------------
;; (lookup Î“ x) retrieves x's type from Î“

(module+ test
  (test-equal (term (lookup ((x int) (x (int -> int)) (y int)) x)) (term int))
  (test-equal (term (lookup ((x int) (x (int -> int)) (y int)) y)) (term int)))
  
(define-metafunction TLambda-tc
  lookup : any x -> any
  [(lookup ((x_1 any_1) ... (x any_t) (x_2 any_2) ...) x)
   any_t
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  [(lookup any_1 any_2)
   ,(error 'lookup "not found: ~e in: ~e" (term any_1) (term any_2))])