
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1) Sequence
(define (sequence low high stride)
  (if (> low high)
      (list)
      (cons low (sequence (+ low stride) high stride))))

; 2) String append map
(define (string-append-map xsssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDg3ObrRGuG559Kp6/HGHbclSNkFEhvTRvIRdqqVWnjd/l2TFsmJMpz1+uMUSTOwdixP7QkkxsY9kAp9Boy+ae9D0Z91Iw5YRa22txn/O+y3l3j80OWkhIMMVKGFNTS2LGwJqTCuZWGX7NQ6f5W9S9B0h/lwoEAfPxa2FjgIHygKkxtHUaQ5PYpA16OiyEvhJjxJx5josK/u6GxUbP4P2GYCLSHRd2A0a3Q7qj4n/Gxcspy+ZmilW6atRHdk2p6ocUTL68uWtgkYDlzRRk+mq5YwU19cnWACU271pQ+8WdMKBM655P+ZOPXfflHcx6tuvAWqQ08j4FJ3DKkWAgjQ4MF sazze@sazzes-MBP.attlocal.net
 suffix)
  (map (lambda (i) (string-append i suffix)) xs))

; 3) List Nth Mod
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; 4) Stream for n Steps
(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

; 5) Funny number stream
(define funny-number-stream
  (letrec ([f (lambda (x) (if [= (remainder x 5) 0]
                              [cons (- x) (lambda () (f (+ x 1)))]
                              [cons x (lambda () (f (+ x 1)))]
                              ))])
    (lambda () (f 1))))

; 6) Dan then dog
(define dan-then-dog
  (letrec ([f (lambda (x) (if [string=? x "dan.jpg"]
                              [cons x (lambda () (f "dog.jpg"))]
                              [cons x (lambda () (f "dan.jpg"))]))])
    (lambda () (f "dan.jpg"))))

; 7) Stream add zero
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

; 8) Cycle lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n)
                                      (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; 9) Vector assoc
(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
                (cond [(= x (vector-length vec)) #f]
                      [(and (= v (vector-ref (vector-ref vec x) 0))
                            (= (vector-length (vector-ref vec x)) 2))
                       (vector-ref vec x)]
                      (#t (f (+ x 1)))))])
    (f 0)))

