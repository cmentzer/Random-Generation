#lang racket

(require redex
         "grammar.rkt"
         "lib.rkt"
         "gen_judgment.rkt")

(provide (all-defined-out))


;; this judgment form generates predicates to be used in contracts. 
(define-judgment-form L
  #:mode (pred I I I O)
  #:contract (pred Γ τ n pred)
  
  ;; default predicate for any types
  [-------------------
   (pred Γ τ 0 any/c)]
  
  ;; predicates for functions
  [(pred Γ τ_1 0 pred_1)
   (pred Γ τ_2 0 pred_2)
   -------------------
   (pred Γ (-> τ_1 τ_2) 0 (-> pred_1 pred_2))]
  
  ;; default predicates for lists
  [-------------------
   (pred Γ (Listof τ) 0 (Listof τ))]
   
  ;; default predicates for numbers
  [-------------------
   (pred Γ num 0 ,(random-number-predicate))]
  
  ;; allow user-defined predicates? 
  ;; uncommenting this code means that any lambda expression with
  ;; result type boolean can be used as a predicate in a contract.
  #;[(gen Γ (-> τ bool) 0 e)
   -------------------
   (pred Γ τ 0 e)]
  
  ;; n+ parameter specifies how many layers of "and" and "or"
  ;; operations the predicate will have. 
  [(pred Γ τ ,(- (term n+) 1) pred_1)
   (pred Γ τ ,(- (term n+) 1) pred_2)
   -------------------
   (pred Γ τ n+ (and pred_1 pred_2))]
  
  [(pred Γ τ ,(- (term n+) 1) pred_1)
   (pred Γ τ ,(- (term n+) 1) pred_2)
   -------------------
   (pred Γ τ n+ (or pred_1 pred_2))])