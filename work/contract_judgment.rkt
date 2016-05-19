#lang racket

(require redex
         "grammar.rkt"
         "pred_judgment.rkt")

(provide (all-defined-out))

;; this judgment form uses the pred judgment form to create contracts
;; of the form (-> predicate predicate) with appropriate types. 
(define-judgment-form L
  #:mode (contract I I I I O)
  #:contract (contract Γ τ n n contract)
  
  [(pred Γ τ n_1 pred_1)
   (pred Γ τ n_2 pred_2)
   -------------------
   (contract Γ τ n_1 n_2 (-> pred_1 pred_2))])