#lang play
(require "main.rkt")
;(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
                  (seqn {+ x 1}
                        x))) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AVISO: hice suficiente para llegar a no mas de el 4.0

;; un test de runval
(test (run-val '(local ((define a 1)(define b 2)(define c 3))
                  (if (= a 1)
                      b
                      c))) 2)
;; member-separator
(test (member-separator (list (field (id 'x) (numV 1))))(list (list (cons (id 'x) (numV 1)))))

;; tests de run
;; enunciado
(test (run-val '(local
                  [(define c (class
                                 (field x 1)
                               (field y 2)
                               (method sum (z) (+ (get this x) (+ (get this y) z)))
                               (method set-x (val) (set this x val))))
                   (define o (new c))]
                  (seqn
                   (send o set-x (+ 1 3))
                   (+ (send o sum 3) (get o y))))) 11 )

(test (run-val '(local
                  [(define A
                     (class
                         (method apply (c)
                                 (send (new c) m))))
                   (define ins (new A))]
                  (send ins apply (class
                                      (field x 2) 
                                    (method m () (get this x)))))) 2)

;; yo

(test (run-val '(local
                  [(define c (class
                               (field t #f)
                               (method andt (val) (and val (get this t)))))
                   (define o (new c))]
                   (get o t))) #f )

(test (run-val '(local
                  [(define c (class
                               (field t #f)
                               (method andt (val) (and val (get this t)))))
                   (define o (new c))]
                   (seqn
                    (set o t #t)
                    (get o t)))) #t )

(test (run-val '(local
                  [(define c (class
                               (field t #f)
                               (method andt (val) (and val (get this t)))))
                   (define o (new c))]
                   (seqn
                    (set o t #t)
                    (send o andt #t)))) #t )

(test (run-val '(local
                  [(define c (class
                               (field t #f)
                               (method andt (val) (and val (get this t)))))
                   (define o (new c))]
                   (seqn
                    (set o t #t)
                    (send o andt #f)))) #f)

(test/exn  (run-val '(local
                  [(define c (class
                               (field t #f)
                               (method andt (val) (and val (get this t)))))
                   (define o (new c))]
                   (seqn
                    (set o t #t)
                    (send o fakemethod #f))))"method not found")

(test/exn  (run-val '(local
                  [(define c (class
                               (field t #f)
                               (method andt (val) (and val (get this t)))))
                   (define o (new c))]
                   (seqn
                    (set o fakefield #t)
                    (send o andt #f))))"field not found")

(test (run-val '(local
                  [(define c (class
                               (field x 1)
                               (field y 2)
                               (method multiple-variable-sum (a b c d e) (+ a (+ b (+ c (+ d e)))))))
                   (define o (new c))]
                   (send o multiple-variable-sum 1 2 3 4 5))) 15)

(test (run-val '(local
                  [(define c (class
                               (field x 1)
                               (field y 2)
                               (method twicex () (+ (get this x) (get this x)))))
                   (define o (new c))]
                   (+
                    (seqn
                     (set o x 10)
                     (send o twicex))
                    1))) 21)

(test (run-val '(local
                  [(define c (class
                               (field x 1)
                               (field y 2)
                               (method twicex () (+ (get this x) (get this x)))))
                   (define o (new c))]
                   (+
                    (seqn
                     (set o x 10)
                     (send o twicex))
                    (seqn
                     (set o x 100)
                     (send o twicex))))) 220)

(test (run-val '(local
                  [(define c (class
                               (field bool #f)
                               (field x 1)
                               (field y 2)
                               (method twicex () (+ (get this x) (get this x)))))
                   (define o (new c))]
                   (if (get o bool)
                       (get o x)
                       (get o y)))) 2)

(test (run-val '(local
                  [(define c (class
                               (field bool #f)
                               (field x 1)
                               (field y 2)
                               (method twicex () (+ (get this x) (get this x)))))
                   (define o (new c))]
                   (if (get o bool)
                       (get o x)
                       (get o y)))) 2)

(test (run-val '(local
                  [(define c (class
                               (field bool #f)
                               (field x 1)
                               (field y 2)
                               (method twicex () (+ (get this x) (get this x)))))
                   (define o (new c))
                   (define p (new c))]
                   (seqn
                    (set o x 100)
                    (+ (get o x) (get p x))))) 101)