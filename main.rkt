#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | (< <s-expr> <s-expr>)
         | (* <s-expr> <s-expr>)
         | (= <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (and <s-expr> <s-expr>)
         | (or <s-expr> <s-expr>)
         | (not <s-expr> <s-expr>)         
         | (seqn <expr> <expr>)
         | (local { <def> ...} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA CLASE Y OBJETOS
<expr>  ::= ...
         | (class <member> ...)
         | (class <: <expr> <member> ...)
         | (new <expr>)
         | this
         | (super id <expr> ...)
         | (set <expr> <id> <expr>)
         | (get <expr> <id>)
         | (send <expr> <id> <expr> ...)

<member> ::= 
        | (field <id> <s-expr>)
        | (method <id> (list <id> ...) <s-expr>)

|#

(deftype Expr
  (num n)
  (bool b)
  (id s)   
  (binop f l r)
  (unop f s)
  (my-if c tb fb)  
  (seqn expr1 expr2)  
  (lcal defs body)
  (clas members)
  (new cls)
  (get oj fld)
  (set oj fld newval)
  (send oj msg vals)
  (this))

;; values
(deftype Val
  (numV n)
  (boolV b))

(deftype Def
  (my-def id expr))

(deftype Member
  (field mid val)
  (method mid param body))

(define-struct obj (class values))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type
 
empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env 


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env)) 

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest)
     (if (hash-has-key? hash x)
         (hash-ref hash x)
         (env-lookup x rest))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (make-hash (list (cons id val))) env)]
    [(aEnv h rEnv) (let* ([l (hash->list h)]
                          [la (cons (cons id val) l)])
                     (set-aEnv-hash! env (make-hash la)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    ['this (this)]
    [(? symbol?) (id s-expr)]    
    [(? boolean?) (bool s-expr)]
    [(list '* l r) (binop * (parse l) (parse r))]
    [(list '+ l r) (binop + (parse l) (parse r))]
    [(list '- l r) (binop - (parse l) (parse r))]
    [(list '< l r) (binop < (parse l) (parse r))]
    [(list '= l r) (binop = (parse l) (parse r))]    
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l) (parse r))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l) (parse r))]
    [(list 'not b) (unop not (parse b))]
    [(list 'if c t f) (my-if (parse c)
                             (parse t)
                             (parse f))]
    [(list 'seqn e1 e2) (seqn (parse e1) (parse e2))]    
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b))]
    [(list 'class members ...)(clas (map parse members))]
    [(list 'new cls)(new (parse cls))]
    [(list 'get obj fld)(get (parse obj) fld)]
    [(list 'set obj fld newval) (set (parse obj) fld (parse newval))]
    [(list 'send obj msg vals ...) (send (parse obj) msg (map parse vals))]
    [(list 'field mid bod) (field (parse mid) (parse bod))]
    [(list 'method mid (list param ...) body) (method (parse mid) param (parse body))]
    ))


;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))


;; member-separator :: list -> cons
;; toma una lista con fields y methods y retorna un cons con una lista de fields interpretados y otra de methods interpretados
(define (member-separator members)
  (define (mem-sep memb-list fld-list mtd-list)
    (match memb-list
      [(list (field  mid bod) t ...) (mem-sep t (append (list (cons mid bod)) fld-list) mtd-list)]
      [(list (method mid param body) t ...) (mem-sep t fld-list (append (list (cons mid (cons param (λ param body)))) mtd-list))]
      [(list) (cons fld-list mtd-list)]))
  (mem-sep members (list) (list)))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]    
    [(binop f l r) (make-val (f (open-val (interp l env))
                                (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(id x) (env-lookup x env)]        
    [(seqn expr1 expr2) (begin 
                          (interp expr1 env)
                          (interp expr2 env))]
    [(lcal defs body)
     (let* ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) defs)       
       (interp body new-env))]
    [(clas members) 
     (def (cons fields methds) (member-separator members))
     (let ([methods methds])
       (letrec
           ([class
                (λ (msg . vals)
                  (case msg
                    [(create) ;new
                     (make-obj class
                               (make-hash fields))]
                    [(read) ;get
                     (def value (dict-ref (obj-values (first vals)) (id (second vals)) #f))
                     (if (not value)
                         (error "field not found")
                         value)]
                    [(write) ;set
                     (if (dict-ref (obj-values (first vals)) (id (second vals)) #f)
                         (dict-set! (obj-values (first vals)) (id (second vals)) (third vals))
                         (error "field not found"))]
                    [(invoke) ;send
                     (def method (assoc (id (second vals)) methods))
                     (define (interp-in-env body) (interp body env))
                     (if method
                         (let ([newenv (multi-extend-env (cadr method) (map interp-in-env (first (cddr vals))) env)])
                           (begin
                             (extend-frame-env! 'this (first vals) newenv)
                             (interp
                              (apply (cddr (assoc (id (second vals)) methods)) (cddr vals))
                              newenv)))
                         (error "method not found"))]))])
         class))]
    [(new cls) ((interp cls env) 'create)]
    [(get oj fld)(interp
                  (let ([obj (interp oj env)])
                    ((obj-class obj) 'read obj fld))
                  env)]
    [(set oj fld newval)(let ([obj (interp oj env)])
                          ((obj-class obj) 'write obj fld (num (open-val (interp newval env)))))]
    [(send oj msg vals) (let ([obj (interp oj env)]) ((obj-class obj) 'invoke obj msg vals))]
    [(this)(interp (id 'this) env)]))

  

;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]
    ))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [x x]))