#lang racket

(require redex
         "grammar.rkt")

(provide (all-defined-out))

;; takes a multiple layer environment and returns a single flattened list of 
;; variables in the environment and their types. 
(define (flatten-env env)
  (let [(last (last env))]
    (cond 
      [(equal? last '·) env]
      [else (append (remv last env) last)])))

;; takes an environment and checks to see if the given variable is a member of
;; that envirmonment. 
(define-metafunction L
  [(lookup Γ_1 χ_1) ,(lookup-rkt (term Γ_1) (term χ_1))])

;; escape to racket
(define (lookup-rkt env x)
  (if (equal? env '·)
      #f
      (lookup-helper (flatten-env env) x)))

;; the meat of the function
(define (lookup-helper env x)
  ;(printf "~a~n" env)
  (cond
    [(empty? env) #f]
    [(equal? (first env) x) (second env)]
    [else (lookup-helper (rest env) x)]))

;; adds a varaible to the given environment. 
(define-metafunction L
  [(+env Γ_1 χ_1 τ_1) ((χ_1 τ_1) Γ_1)])

;; gets a bound variable out of the environment. 
(define-metafunction L 
  [(get-bound-var Γ τ) ,(get-bound-var-rkt (term Γ) (term τ))])

;; escape to racket
(define (get-bound-var-rkt env t)
  (if (equal? env '·)
      #f
      (get-bound-var-helper (flatten-env env) t)))

;; the meat of the functoin. 
(define (get-bound-var-helper env t)
  (begin ;(printf "~a~n" env)
         (cond 
           ;; if the environment is empty, return 'error. This allows the
           ;; program to recover without anything to return. 
           [(equal? (first env) '·) 'error]
           [(equal? t (second (first env))) (first (first env))]
           [else (get-bound-var-rkt (rest env) t)])))

;; probability
(define (prob n)
  (> n (/ (random 100) 100)))

;; define a list of the used variable names so far to prevent shadowing
(define used-sym (box '()))
(define (get-next-sym)
  (let [(out (generate-term L χ 0))]
    (if (member out (unbox used-sym))
        (get-next-sym)
        (begin (set-box! used-sym (append (unbox used-sym) (list out)))
               out))))

;; get a random type from the grammar.
(define (random-type)
    (let [(x (random 2))]
      (if (= 0 x) 'num 'bool)))

;; get a random number-predicate from the grammar. 
(define (random-number-predicate)
  (let [(x (random 6))]
    (cond
      [(= 0 x) (term int?)]
      [(= 1 x) (term (<=/c ,(random 100)))]
      [(= 2 x) (term (>=/c ,(random 100)))]
      [(= 3 x) (term (</c ,(random 100)))]
      [(= 4 x) (term (>/c ,(random 100)))]
      [(= 5 x) (term (=/c ,(random 100)))])))
      

;;== not currently used ==
;; used in language definition with side conidition
;; to ensure that we only attempt to apply functions
;; to values, and not values to values. 
#;(define (fun? e)
  (let ([term (term ,e)])
    (printf "~a~n" term)
    (judgment-holds
     (type ·
           ,term
           (-> τ ...)))))