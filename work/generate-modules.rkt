#lang racket

(require redex
         racket/date
         "interface.rkt"
         "mutation_testing.rkt"
         "gen_judgment.rkt")

(define (generate-modules directory in-type out-type in-c out-c exp-c)
  (let ([plist (judgment-holds
                (gen ((x ,in-type) ·) ,out-type ,exp-c
                     e) e)])
    (let ([mlist (generate-modules-helper plist in-type out-type)])
      (for ([m (in-list mlist)])
        (let ([out (open-output-file (~a "progs/"(hash m)".rkt"))])
          (begin (write m out)
                 (close-output-port out)))))))

(define (generate-modules-helper plist in-type out-type)
  (if (empty? plist) 
      empty
      (cons (list 'module 'f 'racket
                  (list 'provide 
                        (list 'contract-out 
                              (list 'f
                                    (list '-> 
                                          (gen-term (list (list 'x in-type)) 
                                                    'predicate 
                                                    in-type 
                                                    (random 2) 
                                                    (random 2) 
                                                    0) 
                                          (gen-term (list (list 'x in-type)) 
                                                    'predicate 
                                                    out-type 
                                                    (random 2) 
                                                    (random 2) 
                                                    0)))))
                  (list 'define (list 'f 'x)
                        (first plist)))
            (generate-modules-helper (rest plist) in-type out-type))))


(define (hash l) (substring (~a (* 1000 (current-inexact-milliseconds))) 8 15))