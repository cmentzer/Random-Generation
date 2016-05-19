#lang racket
(require redex)

(define-language L 
  ;; applications
  #;(E (E e) 
       (v E) 
       (+ E e) 
       (+ v E) 
       (if0 E e e) 
       hole)
  ;; expressions
  (e (side-condition
      (e_1 e_2)
      (fun? (term e_1)))
     (λ ((χ τ) ...) e)
     χ
     n
     (if0 e e e)
     (+ e e))
  ;; values
  (v ;(λ ((x : τ) ...) e)
   n)
  ;; types
  (τ (-> τ τ ...)
     num)
  ;; environments
  (Γ ((χ τ) ... Γ)
     ·)
  ;; racket value representations
  (n number)
  ;; other variables
  (χ variable-not-otherwise-mentioned))


;; define the behavior of type inside the language
(define-judgment-form L
  #:mode (type I I O)
  #:contract (type Γ e τ)
  
  ;; function application
  [(type Γ e_1 (-> τ_1 τ_2))
   (type Γ e_2 τ_1)
   -------------------------
   (type Γ (e_1 e_2) τ_2)]
  
  ;; function application with 0 arguments (for (λ () ?))
  [(type Γ e_1 τ_1)
   -------------------------
   (type Γ (λ () e_1) (-> τ_1))]
  
  [(type ((χ_1 τ_1) (χ_2 τ_2) ... Γ) e_3 τ_3)
   -----------------------------------
   (type Γ (λ ((χ_1 τ_1) (χ_2 τ_2) ...) e_3) (-> τ_1 τ_2 ... τ_3))]
  
  [---------------------
   (type ((χ τ) Γ) χ τ)]
  
  [--------------------
   (type Γ number num)]
  
   [(where τ (lookup Γ χ))
   ----------------------
   (type Γ χ τ)])

(define-metafunction L
  [(lookup Γ_1 χ_1) ,(lookup-rkt (term Γ_1) (term χ_1))])

(define (lookup-rkt env x)
  (if (equal? env '·)
      #f
      (lookup-helper (flatten env) x)))

(define (lookup-helper env x)
  (printf "~a~n" env)
  (cond
    [(empty? env) #f]
    [(equal? (first env) x) (second env)]
    [else (lookup-helper (rest env) x)]))

;; used in language definition with side conidition
;; to ensure that we only attempt to apply functions
;; to values, and not values to values. 
(define (fun? e)
  (let ([term (term ,e)])
    (printf "~a~n" term)
    (judgment-holds
     (type ·
           ,term
           (-> τ ...)))))


;;TESTS
(test-equal
 (judgment-holds
  (type · (λ ((x num)) x)
        τ) τ)
 (list (term (-> num num))))

(test-equal
 (judgment-holds
  (type ·
        ((λ ((x num)) 1) 1)
        τ) τ)
 '(num))

(test-equal
 (judgment-holds
  (type ·
        (λ () 1)
        τ) τ)
 '((-> num)))