#lang eopl

;; Proyecto FLP 

;; Juliana Melissa Bolaños 202372224
;; Laura Salazar Blanco 202327896
;; Juan Sebastian Rodas 202359681

;******************************************************************************************
;;;;; Interpretador Mini-Py Proyecto FLP 2025-1

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  ; Floats
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (cadena (#\" any (arbno (not #\")) #\") string)
  ))

;******************************************************************************************

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)

    ;Identificadores
    (expression (identifier) id-exp)
    (expression (number) num-exp)
    (expression (cadena) string-exp)

    ; Primitivas
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)

    
    
    ; Definiciones

    (expression ("var" (separated-list identifier "=" expression ",") "in" expression)
                var-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression)
                const-exp)

    
    #| (expression ("rec" (separated-list (identifier "(" (separated-list identifier ",") ")" "=" expression) ",") "in" expression)
                letrec-exp) |#
    
    ; Datos
    (expression (number) num-exp)
    (expression ("x8" "(" (arbno number) ")") num-oct-exp)
    (expression ("x16" "(" (arbno number) ")") num-hex-exp)
    (expression ("x32" "(" (arbno number) ")") num-base32-exp)
    (expression (cadena) string-exp)
     #|
 
    ; Const datos predefinidos

    (expression ("list" "[" (separated-list expression ",") "]" ) list-exp)
    (expression ("tupla" "[" (separated-list expression ";") "]" ) tupla-exp)
    |#

    ; Registros
    (register-item (identifier "=" expression) register-item-exp)
    (expression ("{" (separated-list register-item ";") "}") register-exp)

    (expression (expr-bool) expr-bool-exp)

    (expr-bool (pred-prim "(" expression "," expression ")") pred-exp-bool)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") binop-exp-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") unop-exp-bool)
    (expr-bool (bool) bool-exp)

    ; pred-prim
    (pred-prim (">") mayor-exp)
    (pred-prim (">=") mayor-igual-exp)
    (pred-prim ("<") menor-exp)
    (pred-prim ("<=") menor-igual-exp)
    (pred-prim ("==") igual-exp)
    (pred-prim ("!=") diferente-exp)
    ; oper−bin−bool
    (oper-bin-bool ("and") and-prim)
    (oper-bin-bool ("or") or-prim)
    ; oper-un-bool
    (oper-un-bool ("not") not-prim)

    (bool ("True") true-bool)
    (bool ("False") false-bool)

    ; Estructuras de control

    (expression ("begin" expression (arbno ";" expression) "end") begin-exp) 
    (expression ("if" expr-bool ":" expression "else" ":" expression) if-exp) 
    (expression ("while" expr-bool "do" expression "done" ) while-exp)
    ;(expression ("for" identifier "in" expression ":" expression) for-exp)

    ; enteros
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("%") mod-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)

    
    ; base octal
    (primitive ("+(x8)") oct-suma)
    (primitive ("-(x8)") oct-resta)
    (primitive ("*(x8)") oct-multi)
    (primitive ("add1(x8)") oct-add1)
    (primitive ("sub1(x8)") oct-sub1)
    ; hexadecimales
    (primitive ("+(x16)") hex-suma)
    (primitive ("-(x16)") hex-resta)
    (primitive ("*(x16)") hex-multi)
    (primitive ("add1(x16)") hex-add1)
    (primitive ("sub1(x16)") hex-sub1)
    ; base 32
    (primitive ("+(x32)") b32-suma)
    (primitive ("-(x32)") b32-resta)
    (primitive ("*(x32)") b32-multi)
    (primitive ("add1(x32)") b32-add1)
    (primitive ("sub1(x32)") b32-sub1)
    #|

    ; Primitivas sobre cadenas
    (primitive ("longitud") longitud-prim)
    (primitive ("concatenar") concatenar-prim)

    ; Primitivas sobre listas
    (expression ("vacio?" "(" expression ")") vacio?-exp)
    (expression ("vacio") vacio-exp)
    (expression ("crear-lista" "(" expression (arbno "," expression) ")" ) crear-lista-exp)
    (expression ("lista?" "(" expression ")") list?-exp)
    (expression ("cabeza" "(" expression ")") cabeza-exp)
    (expression ("cola" "(" expression ")") cola-exp)
    (expression ("append" "(" expression "," expression ")") append-exp)
    (expression ("ref-list" "(" expression "," expression ")") ref-list-exp)
    (expression ("set-list" "(" expression "," expression "," expression ")") set-list-exp)

    ; Primitivas sobre tuplas 
    (expression ("crear-tupla" "(" expression (arbno "," expression) ")" ) crear-tupla-exp)
    (expression ("tupla?" "(" expression ")") tupla?-exp)
    (expression ("ref-tupla" "(" expression "," expression ")" ) ref-tupla-exp)
    (expression ("cabeza-tupla" "(" expression ")") cabeza-tupla-exp)
    (expression ("cola-tupla" "(" expression")") cola-tupla-exp)

    ; Primitivas sobre registros
    (expression ("registros?" "(" expression ")") registro?-exp)
    (expression ("crear-registro" "(" identificador "=" expression (arbno "," identificador "=" expression) ")" )
                crear-registro-exp)
    (expression ("ref-registro" "(" expression "," expression")") ref-registro-exp)
    (expression ("set-registro" "(" expression "," expression "," expression")") set-registro-exp)

    ; Primitivas sobre circuitos
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") cons-circuit-prim)
    (primitive ("merge-circuits") merge-circuit-prim)

    ; Circuitos  
    (expression (circuit-exp) exp-circuit)
    (expression (type-op) ty-op)
    (type ("(" type-op ")" ) type-exp)
    (type-op ("and") and-op)
    (type-op ("or") or-op)
    (type-op ("not") not-op)
    (type-op ("xor") xor-op)
    (circuit-exp ( "circuit" gate_list) a-circuit)
    (gate_list ("(" (separated-list gate ";") ")" ) agate-list)
    (gate ("(" "gate" identifier  type input_list ")") a-gate)
    (input_list ("(" (arbno input-item) ")" ) input-list)
    (input-item (bool) bool-input)
    (input-item (identifier) id-input) |#

    ; Imprimir
    (expression ("print" "(" expression ")") print-exp)

    ; Set
    (expression ("set" identifier "=" expression)
                set-exp)

    ; Conversion numeros
    (expression ("decimal-to-base" "(" expression ";" expression ")")
               decimal-to-base-exp)
    (expression ("base-to-decimal" "(" expression ";" expression ")")
               base-to-decimal-exp)

    ))

;******************************************************************************************

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************

;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

(define-datatype target target?
  (direct-target (expval expval?))
  (const-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     '(3 5 10)
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (num-exp (number) number)
      (num-oct-exp (number) number)
      (num-hex-exp (number) number)
      (num-base32-exp (number) number)
      (id-exp (id) (apply-env env id))
      (string-exp (s) s)
      (primapp-exp (prim rands)
      ;(prim)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))

      ; Conversion numeros
      (decimal-to-base-exp (num base) (decimal-to-base (eval-expression num env) (eval-expression base env)))
      (base-to-decimal-exp (lst base) (base-to-decimal (eval-expression lst env) (eval-expression base env)))

      ; Definiciones
      (var-exp (vars rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body (extended-env-record vars (list->vector args) env))))

      (const-exp (ids rands body)
                (let ((args (map (lambda (x) (const-target (eval-expression x env))) rands)))
                   (eval-expression body (extended-env-record ids (list->vector args) env))))

      
      (if-exp (test-exp true-exp false-exp)
              (true-value? (eval-exp-bool test-exp env))
              (if (true-value? (eval-exp-bool test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env))
              )
      (set-exp (id rhs-exp)
          (let ((val (eval-expression rhs-exp env)))
          (display val)
          (newline)
            (setref! (apply-env-ref env id) val)
            val)
               )

      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))
      (expr-bool-exp (exp) 
                     (eval-exp-bool exp env))
        
      (while-exp (exp1 exp2)
                  (let loop ((test (eval-exp-bool exp1 env)))
                    (if (true-value? test)
                        (begin
                          (eval-expression exp2 env)
                          (loop (eval-exp-bool exp1 env)))
                        "fin del bucle"))
                )
      (register-exp (exp) 
                   (eval-expression exp env))
      (print-exp (exp)
            (begin (display (eval-expression exp env))
              (newline) "fin del print"
                  ))
    )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/(car args) (cadr args)))
      (mod-prim () (modulo (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      ; base octal
      (oct-suma () (suma-base (car args) (cadr args) 8))
      (oct-resta () (resta-base (car args) (cadr args) 8))
      (oct-multi () (multi-base (car args) (cadr args) 8))
      (oct-add1 () (sucesor-base (car args) 8))
      (oct-sub1 () (predecesor-base (car args) 8))
          
      ; base hexadecimal
      (hex-suma () (suma-base (car args) (cadr args) 16))
      (hex-resta () (resta-base (car args) (cadr args) 16))
      (hex-multi () (multi-base (car args) (cadr args) 16))
      (hex-add1 () (sucesor-base (car args) 16))
      (hex-sub1 () (predecesor-base (car args) 16))
      
      ; base 32
      (b32-suma () (suma-base (car args) (cadr args) 32))
      (b32-resta () (resta-base (car args) (cadr args) 32))
      (b32-multi () (multi-base (car args) (cadr args) 32))
      (b32-add1 () (sucesor-base (car args) 32))
      (b32-sub1 () (predecesor-base (car args) 32))

      )))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (eq? x #t)))


;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))


(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (string? x) (list? x) (vector? x))))
;
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (const-target (v) #t)
                    (indirect-target (v) #f)))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))
     ;(apply-env-ref env sym)))
    ;env))
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
            (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;Funciones para evaluar expresiones booleanas

(define (eval-exp-bool exp env)
  (cases expr-bool exp
    (pred-exp-bool (prim e1 e2)
      ((lookup-prim prim)
       (eval-expression e1 env)
       (eval-expression e2 env)))

    (binop-exp-bool (op b1 b2)
      ((lookup-bool-binop op)
       (eval-exp-bool b1 env)
       (eval-exp-bool b2 env)))

    (unop-exp-bool (op b)
      ((lookup-bool-unop op)
       (eval-exp-bool b env)))

    (bool-exp (b)
      (cases bool b
        (true-bool () #t)
        (false-bool () #f)))
    ))

(define (lookup-prim prim)
  (cases pred-prim prim
    (mayor-exp() (lambda (x y) (> x y)))
    (menor-exp () (lambda (x y) (< x y)))
    (mayor-igual-exp () (lambda (x y) (>= x y)))
    (menor-igual-exp () (lambda (x y) (<= x y)))
    (igual-exp () (lambda (x y) (equal? x y)))
    (diferente-exp () (lambda (x y) (not (equal? x y))))))

(define (lookup-bool-binop op)
  (cases oper-bin-bool op
    (and-prim () (lambda (x y) (and x y)))
    (or-prim () (lambda (x y) (or x y)))))

(define (lookup-bool-unop op)
  (cases  oper-un-bool op
    (not-prim () (lambda (x) (not x)))))

;******************************************************************************************
;Funciones para operar numeros no decimales en base 8,16 y 32

(define decimal-to-base
  (lambda (n b)
    (if (= n 0)
        '()
        (cons (remainder n b)
              (decimal-to-base (quotient n b) b)))))

(define base-to-decimal
  (lambda (lst b)
    (let loop ((lst lst) (pos 0) (acc 0))
      (if (null? lst)
          acc
          (loop (cdr lst)
                (+ 1 pos)
                (+ acc (* (car lst) (expt b pos))))))))


(define sucesor-base
  (lambda (num base)
    (if(null? num)
      '(1)
      (if (< (car num) (- base 1 ))
           (cons (+ 1 (car num)) (cdr num)  )
          (cons 0 (sucesor-base(cdr num) base ) )
          ))))

(define predecesor-base
  (lambda (num base)
    (if(null? num)
       (eopl:error "limite alcanzado")
       (if (> (car num) 0)
           (if (and (eq? (- (car num) 1) 0) (null? (cdr num)))
               '()
               (cons (- (car num) 1) (cdr num)))
           (cons (- base 1) (predecesor-base (cdr num) base)))
           )))

(define suma-base
 (lambda (elem1 elem2 base)
   (if(null? elem2)
      elem1
      (suma-base (sucesor-base elem1 base) (predecesor-base elem2 base) base))))


(define resta-base
 (lambda (elem1 elem2 base)
   (if (null? elem2)
       elem1
       (predecesor-base (resta-base elem1 (predecesor-base elem2 base) base) base))))

(define multi-base
 (lambda (elem1 elem2 base)
   (if (null? elem2 )
       elem1
       (suma-base elem1 (multi-base elem1 (predecesor-base elem2 base) base) base))))

;******************************************************************************************



;******************************************************************************************
;Definición de los tipos de datos para los registros



;******************************************************************************************

;Pruebas

(show-the-datatypes)
just-scan
scan&parse

(newline)
;== (3, 3)             ; ⇒ #t
;!= (3, 4)              ; ⇒ #t
;> (5, 2)            ; ⇒ #t
;>= (5, 5)              ; ⇒ #t
;<= (3, 8)             ; ⇒ #t

;not (and (True , False))  ; ⇒ #t
;not (or (True , False)) ; ⇒ #f
;not (> (x , 5)) ; ⇒ #t
;and (> (x , 0) , < (x , 10)) ; ⇒ #t

;if >=(+ (2 , 3) , 5) : * (2 , 2) else : 0 ; ⇒ 4
;set x = + (2, 3) ; ⇒ 5

;while and (> (x , 0) , < (x , 10)) do set x =-(x,1) done 
(interpretador)