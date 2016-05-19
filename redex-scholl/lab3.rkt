#lang racket
(require redex
         "common3.rkt")

(define-language TLambda
  (e ::=
     n
     + - cons car cdr
     x
     (lambda ((x_!_ t) ...) e)
     (e e ...)
     list)
  (list ::=
        (cons e list)
        ())
  (n ::= natural)
  (t ::=
     (Listof t)
     empty
     int
     (t ... -> t))
  (x ::= variable-not-otherwise-mentioned))


(define-extended-language TLambda-tc TLambda
  (Γ ::= ((x t) ...)))

;; -----------------------------------------------------------------------------
;; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s

(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))

(define-metafunction TLambda-tc
  extend : Γ (x any) ... -> any
  [(extend ((x_1 any_1) ...) (x any) ...) ((x any) ... (x_1 any_1) ...)])

;; -----------------------------------------------------------------------------
;; (lookup Γ x) retrieves x's type from Γ

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

#;(module+ test
    (traces s->
            (term (((lambda ((x (int -> int))) x) (lambda ((x int)) x)) 1))
            #:pred (lambda (e)
                     (judgment-holds (type () ,e int)))))

(module+ test 
  (test-->> s->
            (term ((lambda ((x int) (y int)) (+ x y)) 1 2)) 3)
  (test-->> s->
            (term (+ 1 2)) 3)
  (test-->> s->
            (term (+ ((lambda ((x int) (y int)) (+ x y)) 1 2) 4)) 7)
  (test-->> s->
            (term (- 5 2)) 3)
  (test-->> s->
            (term (- ((lambda ((x int) (y int)) (+ x y)) 4 5) 3)) 6)
  (test-->> s->
            (term (car (cons 1 (cons 2 (cons 3 ()))))) 1)
  (test-->> s->
            (term (cdr (cons 1 (cons 2 (cons 3 ())))))
            (term (cons 2 (cons 3 ()))))
  (test-->> s-> 
            (term ((lambda ((x (Listof int))) x) (cons 1 (cons 2 ()))))
            (term (cons 1 (cons 2 ()))))
  (test-->> s->
            (term (+ (car (cons 1 (cons 2 ()))) 2)) 3)
  (test-->> s->
            (term ((lambda ((x (Listof int))) (car x)) (cons 2 ())))
            2)
  (test-->> s->
            (term ((lambda ((x (Listof int)) (y int)) (+ (car x) y)) (cons 2 (cons 1 ())) 6))
            8)
  (test-->> s->
            (term ((lambda ((l (Listof int))) (+ 1 (car (cdr l)))) (cons 1 (cons 2 ())))) 3))

(define-extended-language TStandard TLambda-tc
  (v ::= 
     n 
     + - car cdr
     (lambda ((x t) ...) e)
     list)
  (list ::=
        (cons e list)
        ())
  (E ::=
     hole
     (v ... E e ...)))

(define s->
  (reduction-relation
   TStandard
   #:domain e
   (--> (in-hole E ((lambda ((x_1 t_1) ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e))
        β)
   (--> (in-hole E (+ n_1 n_2))
        (in-hole E ,(+ (term n_1) (term n_2)))
        +)
   (--> (in-hole E (- n_1 n_2))
        (in-hole E ,(- (term n_1) (term n_2)))
        -)
   (--> (in-hole E (car (cons e_1 v_1)))
        (in-hole E e_1)
        car)
   (--> (in-hole E (cdr (cons e_1 v_1)))
        (in-hole E v_1)
        cdr)))


(module+ test
  (test-equal (judgment-holds 
               (type () (lambda ((x int) (f (int -> int))) (+ x 1))
                     (int (int -> int) -> int))) #true)
  (test-equal (judgment-holds (type ((x int)) x int)) #true) 
  (test-equal (judgment-holds (type () (+ 1 2) int)) #true)
  (test-equal (judgment-holds (type () (cons 1 ()) (Listof int))) #true)
  (test-equal (judgment-holds (type () (cons (lambda ((x int)) x) ()) (Listof (int -> int)))) #true)
  (test-equal (judgment-holds (type () (car (cons 1 ())) int)) #true)
  (test-equal (judgment-holds (type () (cdr (cons 1 (cons 2 ()))) (Listof int))) #true)
  (test-equal (judgment-holds (type () (cdr (cons 1 ())) empty)) #true)
  (test-equal (judgment-holds (type () (cons 1 (cons (lambda ((x int)) x) ())) (Listof int))) #false))

(define-judgment-form TStandard
  #:mode (type I I O)
  #:contract (type Γ e t)
  [----------------------- "number"
                           (type Γ n int)]
  
  [----------------------- "+"
                           (type Γ + (int int -> int))]
  
  [----------------------- "variable"
                           (type Γ x (lookup Γ x))]
  
  [----------------------- "-"
                           (type Γ - (int int -> int))]
  
  [----------------------- "empty"
                           (type Γ () empty)]
  
  [(type Γ e_1 t_1)
   (type Γ list empty)
   ----------------------- "empty list"
   (type Γ (cons e_1 list) (Listof t_1))]
  
  [(type Γ e_1 t_1)
   (type Γ list (Listof t_1))
   ----------------------- "list"
   (type Γ (cons e_1 list) (Listof t_1))]
  
  [(type Γ e_1 t_1)
   ----------------------- "car"
   (type Γ (car (cons e_1 list)) t_1)]
  
  [(type Γ list_1 t_1)
   ----------------------- "cdr"
   (type Γ (cdr (cons e list_1)) t_1)]
  
  [(type (extend Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (type Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]
  
  [(type Γ e_1 (t_2 ... -> t))
   (type Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (type Γ (e_1 e_2 ...) t)])
