#lang racket

(require redex
         "mon_judgment.rkt"
         "contract_judgment.rkt"
         "gen_judgment.rkt"
         "pred_judgment.rkt"
         "lib.rkt")

(provide gen-term
         build-env)

(define (build-env Γ l)
  (cond
    [(empty? l) (cons Γ empty)]
    [else (cons (first l) (build-env Γ (rest l)))]))

(define (gen-term l judgment type in-c out-c exp-c)
  (let ([env (build-env '· l)])
    (cond
      [(equal? judgment 'mon)
       (let ([list (judgment-holds
                    (mgen ,env ,type ,in-c ,out-c ,exp-c
                          m) m)])
         (list-ref list (random (length list))))]
      [(equal? judgment 'contract)
       (let ([list (judgment-holds
                    (contract ,env ,type ,in-c ,out-c
                              contract) contract)])
         (list-ref list (random (length list))))]
      [(equal? judgment 'predicate)
       (let ([list (judgment-holds
                    (pred ,env ,type ,in-c
                          pred) pred)])
         (list-ref list (random (length list))))]
      [(equal? judgment 'expression)
       (let ([list (judgment-holds
                    (gen ,env ,type ,exp-c
                         e) e)])
         (list-ref list (random (length list))))])))