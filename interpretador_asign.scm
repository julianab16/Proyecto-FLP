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
   ("%" (arbno (not #\newline))) skip)
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

    ; Definiciones

    (expression ("var" (separated-list (identifier "=" expression) ",") "in" expression)
                var-exp)
    (expression ("const" (separated-list (identifier "=" expression) ",") "in" expression)
                const-exp)
    (expression ("rec" (separated-list (identifier "(" (separated-list identifier ",") ")" "=" expression) ",") "in" expression)
                letrec-exp)
    
    ; Datos

    (expression (number) num-exp)
    (expression ("x8" "(" (arbno number) ")") num-oct-exp)
    (expression ("x16" "(" (arbno number) ")") num-hex-exp)
    (expression ("x32" "(" (arbno number) ")") num-base32-exp)
    (expression (cadena) string-exp)
    (bool ("True") true-bool)
    (bool ("False") false-bool)

    ; Const datos predefinidos

    (expression ("list" "[" (separated-list expression ",") "]" ) list-exp)
    (expression ("tupla" "[" (separated-list expression ";") "]" ) tupla-exp)
    (expression ("registro" "{" (separated-list (identifier "=" expression) ";") "}") register-exp)
    (expression (expr-bool) expr-bool-exp)
    (expr-bool (expression pred-prim expression) pred-prim-exp)
    (expr-bool (expr-bool oper-bin-bool expr-bool) oper-bin-exp)
    (expr-bool (bool) bool-exp)
    (expr-bool (oper-un-bool "(" expr-bool ")") oper-un-bool-exp)
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

    ; Estructuras de control

    (expression ("begin" expression (arbno ";" expression) "end") begin-exp) 
    (expression ("if" expr-bool ":" expression "else" ":" expression) if-exp) 
    (expression ("while" expr-bool ":" expression) while-exp)
    (expression ("for" identifier "in" expression ":" expression) for-exp)


    ; Primitivas
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)

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
    (primitive ("~(x8)") oct-resta)
    (primitive ("*(x8)") oct-multi)
    (primitive ("add1(x8)") oct-add1)
    (primitiva-unaria ("sub1(x8)") oct-sub1)
    ; hexadecimales
    (primitive ("+(x16)") hex-suma)
    (primitive ("~(x16)") hex-resta)
    (primitive ("*(x16)") hex-multi)
    (primitive ("add1(x16)") hex-add1)
    (primitive ("sub1(x16)") hex-sub1)
    ; base 32
    (primitive ("+(x32)") b32-suma)
    (primitive ("~(x32)") b32-resta)
    (primitive ("*(x32)") b32-multi)
    (primitive ("add1(x32)") b32-add1)
    (primitive ("sub1(x32)") b32-sub1)

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
    (input-item (identifier) id-input)

    ; Set
    (expression ("set" identifier "=" expression)
                set-exp)

    ; Imprimir
    (expression ("print" "(" expression ")") print-exp)
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

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z f)
;     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) ())) ())))
;                      (empty-env)))
;     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (num-exp (datum) datum)
      (id-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps))))))))

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
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1)))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

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

;(define iota
;  (lambda (end)
;    (iota-aux 0 end)))
;
;(define iota-aux
;  (lambda (ini fin)
;    (if (>= ini fin)
;        ()
;        (cons ini (iota-aux (+ 1 ini) fin)))))

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
;Pruebas

(show-the-datatypes)
just-scan
scan&parse
(just-scan "add1(x)")
(just-scan "add1(   x   )%cccc")
(just-scan "add1(  +(5, x)   )%cccc")
(just-scan "add1(  +(5, %ccccc x) ")
(scan&parse "add1(x)")
(scan&parse "add1(   x   )%cccc")
(scan&parse "add1(  +(5, x)   )%cccc")
(scan&parse "add1(  +(5, %cccc
x)) ")
(scan&parse "if -(x,4) then +(y,11) else *(y,10)")
(scan&parse "let
x = -(y,1)
in
let
x = +(x,2)
in
add1(x)")

(define caso1 (primapp-exp (incr-prim) (list (num-exp 5))))
(define exp-numero (num-exp 8))
(define exp-ident (id-exp 'c))
(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
(define programa (a-program exp-app))
(define una-expresion-dificil (primapp-exp (mult-prim)
                                           (list (primapp-exp (incr-prim)
                                                              (list (id-exp 'v)
                                                                    (id-exp 'y)))
                                                 (id-exp 'x)
                                                 (num-exp 200))))
(define un-programa-dificil
    (a-program una-expresion-dificil))

(interpretador)