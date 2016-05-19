#lang racket

(require redex
         "common2.rkt")

(define-extended-language Lambda-η Lambda
  (e ::= .... n)
  (n ::= natural)
  (C ::=
     hole
     (e ... C e ...)
     (lambda (x_!_ ...) C))
  (v ::=
     n
     (lambda (x ...) e)))


(module+ test
  (test--> -->β
           (term ((lambda (x) x) 5)) 5)
  (test--> -->β
           (term ((lambda (x y) (x y))
                  ((lambda (a) a) 5)
                  10))
           (term ((lambda (x y) (x y))
                  5
                  10))
           (term (((lambda (a) a) 5) 10))))


(define -->β
  (reduction-relation
   Lambda-η
   (--> (in-hole C ((lambda (x_1 ..._n) e) e_1 ..._n))
        (in-hole C (subst ([e_1 x_1] ...) e))
        β)))

(module+ test
  (test--> -->n
           (term ((lambda (x) ((lambda (y) y) x)) 5))
           (term ((lambda (y) y) 5)))
  (test--> -->n
           (term (lambda (x) (e x)))
           (term e)))

(define -->n
  (reduction-relation
   Lambda-η
   (--> (in-hole C (lambda (x_1 ..._n) (e x_1..._n)))
        (in-hole C e)
        n)))

(define -->βn (union-reduction-relations -->β -->n))

(module+ test 
  (test--> -->βn
           (term (lambda (x) ((lambda (y) y) x)))
           (term (lambda (y) y))
           (term (lambda (x) x)))
  (test--> -->βn 
           (term (lambda (x) ((lambda (y) 5) x)))
           (term (lambda (y) 5))
           (term (lambda (x) 5)))
  (test--> -->βn 
           (term (lambda (x) (6 x)))
           6)
  (test--> -->βn 
           (term ((lambda (y) 5) (lambda (x) (7 x))))
           (term ((lambda (y) 5) 7))
           5)
  (test--> -->βn
           (term ((lambda (x) (1 x)) 1))
           (term (1 1))
           (term (1 1))))


#;(traces -->βn
          (term (lambda (x) ((lambda (y) y) x))))

;; tests for the lambda-n? function
(module+ test
  (define e1 (term 5))
  (define e2 (term (lambda (x y) (x y))))
  (define e3 (term (lambda (x y) x)))
  (define e4 (term ((lambda (x) x) 5))))

(define lambda-n? (redex-match? Lambda-η e))


;; ------------------------------___---------------------------___------__-----__-------
(define-extended-language Standard Lambda-η
  (E ::=
     hole
     (v ... E e ...)))

(define s->β
  (reduction-relation
   Standard
   (--> (in-hole E ((lambda (x_1 ..._n) e) e_1 ..._n))
        (in-hole E (subst ([e_1 x_1] ...) e))
        β)))

(define s->βn
  (extend-reduction-relation s->β Standard
                           (--> (in-hole E (lambda (x_1 ..._n) (e x_1..._n)))
                                (in-hole E e)
                                βn)))

(module+ test 
  (test--> s->βn
           (term (lambda (x) ((lambda (y) y) x)))
           (term (lambda (y) y))
           (term (lambda (x) x)))
  (test--> s->βn 
           (term (lambda (x) ((lambda (y) 5) x)))
           (term (lambda (y) 5))
           (term (lambda (x) 5)))
  (test--> s->βn
           (term ((lambda (y) 5) (lambda (x) (7 x)) 1))
           (term ((lambda (y) 5) 7 1)))  ;; cannot be reduced further, meaningless
  (test--> s->βn 
           (term ((lambda (y) 5) (lambda (x) (7 x)))) ;; eta reduces to 
           (term ((lambda (y) 5) 7)) ;; this, which also beta reduces to
           5)) ;; this... so we reach the same result with either an eta and a beta or just a beta

#|

(define-metafunction Standard
  eval-value : e -> v or closure
  [(eval-value e) any_1 (where any_1 (run-value e))])

(define-metafunction Standard
  run-value : e -> v or closure
  [(run-value n) n]
  [(run-value v) closure]
  [(run-value e)
   (run-value e_again)
   ; (v) means that we expect s->βn to be a function 
   (where (e_again) ,(apply-reduction-relation s->βn (term e)))])


(module+ test 
  (define t1
    (term ((lambda (x) x) (lambda (x) x))))
  (test-equal (lambda? t1) #true)
  (test-equal (redex-match? Standard e t1) #true)
  (test-equal (term (eval-value (term 5))) 5)
  (test-equal (term (eval-value ,t1)) 'closure))

|#



(module+ test
  (test-results))