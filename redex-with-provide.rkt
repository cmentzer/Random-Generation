#lang racket
(require redex)

(define-language L
  (program (sub-module-form sub-module-form ...))
  (sub-module-form 
   (module module-name racket
     (provide (contract-out [id_1 (contract . -> . contract)])
              (define (id_1 value)))
     (define id value) ...))
  ;(require require-spec require-spec ...)
  ;((struct id (id ...)) (struct id (id ...)) ...)
  (provide-spec ;id 
   (contract-out p/c-item))
  (p/c-item (side-condition 
             [id (contract_1 . -> . contract_2)]
              (= (length (term (list contract_1))) (length (term (list contract_2))))))
  ;(struct id ([id contract] [id contract] ...)))
  (require-spec (submod ".." module-name))
  (module-name id)
  (contract predicate 
            any/c 
            (or/c contract contract contract ...) 
            (and/c contract contract contract ...))
  (predicate integer?
             number?
             boolean?
             real?)
  (expr value
        (if expr expr expr))
  ; (expr ...))
  (value number
         ;boolean
         op
         id
         (λ (id ...) expr))
  (op (+ expr expr expr ...)
      (- expr expr)
      (* expr expr expr ...)
      (/ expr expr))
  (id variable-not-otherwise-mentioned))

(redex-match L value (term (λ (x) x)))
(redex-match L (op_1 op_2) (term ((+ 1 2) (+ 1 2))))
(redex-match L contract (term real?))
(redex-match L contract (term (or/c real? integer?)))
(redex-match L require-spec (term (submod ".." x)))
(redex-match L p/c-item (term [foo (-> (or/c real? integer?) integer?)]))
;(redex-match L provide-spec (term foo))
(redex-match L 
             provide-spec 
             (term (contract-out [foo (-> (or/c real? boolean?) boolean?)])))
;(redex-match L sub-module-form (term ((struct foo (bar baz)))))
;(redex-match L sub-module-form (term ((struct foo (bar baz))
;                                      (struct goo (car caz)))))
(redex-match L program (term ((module foo racket (define bar (λ () 0))))))

(define-metafunction L
  same : id id -> boolean
  [(same id id) #t]
  [(same id_1 id_2) #f])

;; different syntactic categories for each type

#|
(define red
  (reduction-relation
   L
   #:domain expr
   (--> (in-hole Expr (if #t expr_1 expr_2))
        (in-hole Expr expr_1)
        "ift")
   (--> (in-hole Expr (if #f expr_1 expr_2))
        (in-hole Expr expr_2)
        "iff")
   (--> (in-hole Expr (- number number))
        (in-hole Expr (sub number number))
        "-")
   (--> (in-hole Expr (/ number number))
        (in-hole Expr (div number number))
        "/")
   (--> (in-hole Expr (+ number ...))
        (in-hole Expr (sum number ...))
        "+")
   (--> (in-hole Expr (* number ...))
        (in-hole Expr (mul number ...))
        "*")))
r
(define-metafunction L
  sum : number ... -> number
  [(sum number ...)
   ,(apply + (term (number ...)))])
(define-metafunction L 
  mul : number ... -> number
  [(mul number ...)
   ,(apply * (term (number ...)))])
(define-metafunction L
  div : number number -> number
  [(div number number)
   ,(apply / (term (number number)))])
(define-metafunction L
  sub : number number -> number
  [(sub number number)
   ,(apply - (term (number number)))])

(test-->>
   red
   (term (if #t 2 3))
   (term 2))
(test-->>
   red
   (term (if #f 2 3))
   (term 3))
(test-->>
   red
   (term (+ 2 3))
   (term 5))
(test-->>
   red
   (term (- 4 3))
   (term 1))
(test-->>
   re
   (term (* 2 3))
   (term 6))
(test-->>
   red
   (term (/ 3 3))
   (term 1))
|#