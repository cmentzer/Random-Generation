#lang racket
(require redex
         "grammar.rkt"
         "lib.rkt")

(provide (all-defined-out))

;; define the behavior of type inside the language
(define-judgment-form L
  #:mode (type I I O)
  #:contract (type Γ e τ)
  
  ;; function application
  [(type Γ e_1 (-> τ_1 τ_2)) 
   (type Γ e_2 τ_1)
   -------------------------
   (type Γ (e_1 e_2) τ_2)]
  
  ;; 0 argument function application
  [(type Γ e_1 (-> τ_1))
   ------------------------
   (type Γ (e_1) τ_1)]
  
  ;; A 0 argument lambda has the type (-> τ)
  ;; where τ is the type of its body
  [(type Γ e_1 τ_1)
   -------------------------
   (type Γ (λ () e_1) (-> τ_1))]
  
  ;; A many argument lambda has the type (-> τ_1 τ_2 ... τ_n)
  ;; where τ_1 - τ_(n-1) are the types of the arguments 
  ;; and τ_n is the type of the body
  [(type ((χ_1 τ_1) (χ_2 τ_2) ... Γ) e_3 τ_3)
   -----------------------------------
   (type Γ (λ ((χ_1 τ_1) (χ_2 τ_2) ...) e_3) (-> τ_1 τ_2 ... τ_3))]
  
  ;; A variable with a type attached to it (but not in environment)!
  ;; has its attached type
  [---------------------
   (type ((χ τ) Γ) χ τ)]
  
  [(type Γ e_1 num)
   (type Γ e_2 num)
   --------------------
   (type Γ (+ e_1 e_2) num)]
  
  [(type Γ e_1 τ_1)
   --------------------
   (type Γ (mon pred e_1) τ_1)]
  
  ;; numbers have type num
  [--------------------
   (type Γ number num)]
  
  ;; booleans have type bool
  [--------------------
   (type Γ boolean bool)]
  
  ;; lists have list type
  [--------------------
   (type Γ empty list)]
  
  [(type Γ e τ)
   --------------------
   (type Γ (cons e l) (Listof τ))]
  
  ;; A variable in the environment has the type attached to it
  [(where τ (lookup Γ χ))
   ----------------------
   (type Γ χ τ)])

