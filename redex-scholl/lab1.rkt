#lang racket
(require redex
         "common.rkt")

(define-extended-language Env Lambda
  (e ::= 
     .... 
     natural
     boolean)
  (env ::= ((x e) ...)))

(define env? (redex-match? Env env))

(module+ test
  (test-equal (env? (term ((x 2) (y 3) (z 4) (q (lambda (a) a))))) #t)
  (test-equal (env? (term ((x x)))) #t)
  (test-equal (env? (term 5)) #f))


;; lookup finds the first instance of x in the given env
(define-metafunction Env
  lookup : x env -> e
  
  
  #;[(lookup x_1 ((x_0 e_0) ... (x_1 e_1) (x_2 e_2) ...)) 
   e_1
   (where #false (in x (x_0 ...)))]
  
  [(lookup x_1 ((x_0 e_0) ... (x_1 e_1) (x_2 e_2) ...))
   e_1
   (where #false (in x ((x_0 e_0) ...)))]
  
  [(lookup x_1 any) #false])
  
  
  
  ;[(lookup x_1 ()) #false]
  ;[(lookup x_1 ((x_1 e_1) ...)) (e_1)]
  ;[(lookup x_1 ((x_0 e_0) (x_1 e_1) ...)
           
           
           


(module+ test 
  (test-equal (term (lookup y ((y 5)))) (term 5))
  (test-equal (term (lookup x ((y 5) (x 7)))) (term 7))
  (test-equal (term (lookup z ((y 5) (z 1) (x 7) (z 4)))) (term 1))
  (test-equal (term (lookup z ((y 5)))) (term #false)))
  