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
   (letter (arbno (or letter digit "_"))) symbol)
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
     
 
    ; Const datos predefinidos

    (expression ("[" (separated-list expression ":") "]") list-exp)
    (expression ("tupla[" (separated-list expression ":") "]") tuple-exp)
    

    (expression (expr-bool) expr-bool-exp)

    (expr-bool (pred-prim "(" expression "," expression ")") pred-exp-bool)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") binop-exp-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") unop-exp-bool)
    (expr-bool (bool) bool-expr)

    (bool ("True") true-bool)
    (bool ("False") false-bool)

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
    (expression ("while" expr-bool "do" expression "done" ) while-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)

    ; Registros
    
    (expression ("{" (separated-list identifier "=" expression ";") "}") register-exp)

    ; Primitivas sobre registros
    ;(expression ("registros?" "(" expression ")") is-registro-exp)
    ;(expression ("crear-registro" "(" (separated-list identifier "=" expression ";")  ")") crear-registro-exp)
    ;(expression ("ref-registro" "(" expression "," expression ")") ref-registro-exp)
    ;(expression ("set-registro" "(" expression "," expression "," expression ")") set-registro-exp)

    (primitive ("registros?")  is-registro-prim)
    (primitive ("crear-registro")  crear-registro-prim)
    (primitive ("ref-registro")  ref-registro-prim)
    (primitive ("set-registro")  set-registro-prim)
    
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
    

    ; Primitivas sobre cadenas
    ;(primitive ("longitud") longitud-prim)
    ;(primitive ("concatenar") concatenar-prim)

    ; Primitivas sobre listas
    (primitive ("vacio?") empty?-prim)
    (primitive ("vacio") empty-prim)
    (primitive ("crear-lista") make-list-prim)
    (primitive ("lista?") list?-prim)
    (primitive ("cabeza") head-prim)
    (primitive ("cola") tail-prim)
    (primitive ("append") append-prim)
    (primitive ("ref-list") ref-list-prim)
    (primitive ("set-list") set-list-prim)

    ; Primitivas sobre tuplas 
    (primitive ("vacio-tupla?") tuple-empty?-prim)
    (primitive ("vacio-tupla") tuple-empty-prim)
    (primitive ("crear-tupla") make-tuple-prim)
    (primitive ("tupla?") tuple?-prim)
    (primitive ("cabeza-tupla") tuple-head-prim)
    (primitive ("cola-tupla") tuple-tail-prim)
    (primitive ("ref-tupla") tuple-ref-prim)
    #|
    (expression ((arbno identifier "="  expression) expression) let-exp)|#
    ; Primitivas sobre circuitos
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") cons-circuit-prim)
    (primitive ("merge-circuits") merge-circuit-prim)

    ; Circuitos  
    (expression (circuit-exp) exp-circuit)
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
     '(x v c)
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
          (cases primitive prim
          (cons-circuit-prim ()
            (apply-primitive prim rands env))
          (merge-circuit-prim ()
            (apply-primitive prim rands env))
          (crear-registro-prim ()
            (apply-primitive prim rands env))
          (ref-registro-prim ()
            (apply-primitive prim rands env))
          (else
            (let ((args (eval-rands rands env)))
              (apply-primitive prim args env)))))

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

      (for-exp (id iterable-exp body-exp)
        (let ((iterable (eval-expression iterable-exp env)))
          (let ((elements (cond 
            ((vector? iterable) (vector->list iterable))
            ((list? iterable) iterable)
            (else (eopl:error 'eval-expression "Iterable debe ser lista o tupla: ~s" iterable)))))
          (if (null? elements)'()  
            (let loop ((elements elements)
              (last-result 'undefined))
              (if (null? elements)
                last-result
                (let ((element (car elements)))
                  (let ((new-env (extend-env (list id) (list element) env)))
                    (loop (cdr elements) (eval-expression body-exp new-env))))))))))
      
      (list-exp (elements) 
                (list->vector 
                  (map (lambda (e) (eval-expression e env)) elements)))
      
      (tuple-exp (elements)
        (map (lambda (e) (eval-expression e env)) elements))
      
      (register-exp (campos valores)
        (if (null? campos)
            (eopl:error "Se requiere al menos un par identificador = expresión")
            (let* ((valores-evaluados (map (lambda (v) (eval-expression v env)) valores))
                   (nuevo-env (extend-env campos valores-evaluados env)))
              (crear-registro campos valores-evaluados))))
        

      (exp-circuit (gate_list) gate_list)
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
  (lambda (prim args env)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/(car args) (cadr args)))
      (mod-prim () (modulo (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (eval-circuit-prim () (eval-circuit (car args) env))
      (cons-circuit-prim ()
        (let* ((c1 (eval-expression (car args) env))
               (c2 (eval-expression (cadr args) env))
               (input-id (cases expression (caddr args)
                           (id-exp (id) id)
                           (num-exp (datum) datum)
                           (else (eopl:error 'connect-circuits 
                                            "Tercer parametro no es un identificador")))))
          (connect-circuits c1 c2 input-id env)))
      (merge-circuit-prim ()
        (let* ((c1 (eval-expression (car args) env))
               (c2 (eval-expression (cadr args) env))
               (ty-op (eval-expression (caddr args) env))
               (out-id (cases expression (cadddr args)
                         (id-exp (id) id)
                         (num-exp (datum) datum)
                         (else (eopl:error 'merge-circuits 
                                          "Cuarto parámetro no es un identificador")))))
          (merge-circuits c1 c2 ty-op out-id env)))
 
      (is-registro-prim ()
            (registro? (car args)))
      (crear-registro-prim ()
        (let ((arg (eval-expression (car args) env)))
          (if (registro? arg)
              arg
              (eopl:error 'crear-registro-prim "El argumento no es válido para crear un registro"))))
      (ref-registro-prim ()
      (let* ((reg (eval-expression (car args) env))
            (key (eval-expression (cadr args) env)))
        (if (symbol? key)
            (ref-registro reg key)
            (eopl:error 'ref-registro-prim "El segundo argumento debe ser un símbolo"))))
      (set-registro-prim ()
            (set-registro (eval-expression (car args) env)
                              (eval-expression (cadr args) env)
                              (eval-expression (caddr args) env)))
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

      ;listas
      (empty?-prim () (zero? (vector-length (car args)))) 
      (empty-prim () (vector)) 
      (make-list-prim () (list->vector args))
      (list?-prim () (vector? (car args)))
      (head-prim () (vector-ref (car args) 0)) 
      (tail-prim () 
        (if (vector? (car args))
            (list->vector (cdr (vector->list (car args)))) 
            (eopl:error 'tail-prim "Argumento no es una lista: ~s" (car args)))) 
      (append-prim () (vector-append (car args) (cadr args))) 
      (ref-list-prim () (vector-ref (car args) (cadr args))) 
      (set-list-prim () (vector-set! (car args) (cadr args) (caddr args))#t)

      ;tuplas
      (tuple-empty?-prim () 
        (null? (car args))) 
      (tuple-empty-prim () '()) 
      (make-tuple-prim () args) 
      (tuple?-prim ()(list? (car args))) ; 
      (tuple-head-prim () 
        (if (null? (car args))
            (eopl:error 'cabeza-tupla "Tupla vacía")
            (car (car args))))
      (tuple-tail-prim () 
        (if (null? (car args))
            (eopl:error 'cola-tupla "Tupla vacía")
            (cdr (car args)))) 
      (tuple-ref-prim () 
        (let ((tuple (car args))
              (index (cadr args)))
          (if (or (< index 0) (>= index (length tuple)))
              (eopl:error 'ref-tupla "Índice inválido: ~s" index)
              (list-ref tuple index))))
      
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

(define-datatype expval expval?
  (num-val (n number?))          ; Para valores numéricos
  (bool-val (b boolean?))        ; Para valores booleanos
  (string-val (s string?))       ; Para cadenas
  (registro-val (r registro?)))  ; Para registros


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

    (bool-expr (b) 
      (eval-bool b))
      ))

(define (eval-bool bool-d)
  (cases bool bool-d
    [true-bool () #t]
    [false-bool () #f]))


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



;-------------------------LISTAS---------------------------------------
;datatype para representar las listas


;extract-values-list <lista> -> <list>
;Funcion que extrae la list de valores de un datatype lista

;-------------------------VECTORES---------------------------------
;datatype para representar vectores
(define-datatype vec vec?
  (empty-vec)
  (vec-a (values vector?))
  )

;extract-values-vec <vec> -> <vector>
;Funcion que extrae el vector de valores de un datatype vec
(define extract-values-vec
  (lambda (vector)
    (cases vec vector
      (empty-vec()( #() ))
      (vec-a (values) values))
    )
  )

;funcion auxiliar de vectores
(define vector-append
  (lambda (vec1 vec2)
    (list->vector (append (vector->list vec1) (vector->list vec2)))))
;******************************************************************************************
;Definición de los tipos de datos para los registros

(define-datatype registro registro?
  (reg-a 
   (keys (list-of symbol?))    ; Lista de identificadores
   (values vector?))) ; Vector de valores

(define crear-registro
  (lambda (keys values)
    (if (not (list? keys))
        (eopl:error "Las claves deben ser una lista de identificadores")
        (if (not (list? values))
            (eopl:error "Los valores deben ser una lista")
            (reg-a keys (list->vector values))))))

(define ref-registro
  (lambda (reg key)
    (cases registro reg
      (reg-a (keys values)
             (let ((pos (rib-find-position key keys)))
               (if (number? pos)
                   (deref (a-ref pos values))
                   (eopl:error "No existe el identificador")))))))

(define set-registro
  (lambda (reg key value)
    (cases registro reg
      (reg-a (keys values)
             (let ((pos (rib-find-position key keys)))
               (if (number? pos)
                   (begin
                     (setref! (a-ref pos (list->vector values)) value)
                     value)
                   (eopl:error "No existe el identificador")))))))

;(define r (crear-registro '(x y) '("edad" "años")))
;(display r)
;(newline)
;******************************************************************************************

;; Funcion: eval-circuit
;; Proposito: Evalua un circuito lógico completo, comenzando con la primera compuerta.
;; Parametros:
;;   circuito: Representación del circuito (estructura tipo a-circuit).
;;   env: Entorno de variables (alist con pares (variable . valor)).
;; Retorno: Valor booleano de la última compuerta evaluada.

(define (eval-circuit circuito env)
  (cases circuit-exp circuito
    [a-circuit (gate)
      (cases gate_list gate
        [agate-list (gates) (eval-gate-list gates env)])]))

;; Funcion: eval-gate-list
;; Proposito: Evalúa una lista de compuertas lógicas secuencialmente, actualizando el entorno.
;; Parametros:
;;   gates: Lista de compuertas lógicas.
;;   env: Entorno de variables.
;; Retorno: Resultado booleano de la última compuerta evaluada.

(define (eval-gate-list gates env)
  (cond
    [(null? gates) '()]
    [else
     (let* ([result (eval-gate (car gates) env)]
            [id     (car result)]
            [value  (cadr result)]
            [new-env (extend-env (list id) (list value) env)])
       (if (null? (cdr gates))
           value
           (eval-gate-list (cdr gates) new-env)))]))

;; Funcion: eval-gate
;; Proposito: Evalúa una compuerta lógica individual usando el tipo de operación lógica correspondiente.
;; Parametros:
;;   g: Compuerta lógica a evaluar.
;;   env: Entorno actual de variables.
;; Retorno: Lista con el identificador de la compuerta y su valor evaluado.

(define (eval-gate g env)
  (cases gate g
    [a-gate (id t input-lst)
      (cases type t
        [type-exp (t-op)
          (let ([args (eval-input-list input-lst env)])
            (list id
              (cases type-op t-op
                [and-op () (applied-logic and-fn args)]
                [or-op  () (applied-logic or-fn args)]
                [xor-op () (multi-xor args)]
                [not-op () (not (car args))])))])]))

;; Funcion: applied-logic
;; Proposito: Aplica una función lógica binaria (como AND u OR) a una lista de argumentos.
;; Parametros:
;;   op: Función lógica (como and-fn u or-fn).
;;   lst: Lista de booleanos a combinar.
;; Retorno: Resultado booleano de aplicar la función a todos los elementos.

(define (applied-logic op lst)
  (cond
    [(null? lst) (if (eq? op not) '() #f)]
    [(null? (cdr lst)) (op (car lst) (car lst))]
    [else (op (car lst) (applied-logic op (cdr lst)))]))

;; Funcion: eval-input-list
;; Proposito: Evalúa una lista de entradas (inputs) a una compuerta.
;; Parametros:
;;   inputl: Lista de entradas (estructura input-list).
;;   env: Entorno de variables.
;; Retorno: Lista de valores booleanos evaluados.

(define (eval-input-list inputl env)
  (cases input_list inputl
    [input-list (items) (eval-items items env)]))

;; Funcion: eval-items
;; Proposito: Evalúa recursivamente una lista de entradas.
;; Parametros:
;;   items: Lista de entradas (input-item).
;;   env: Entorno de variables.
;; Retorno: Lista de valores booleanos.

(define (eval-items items env)
  (if (null? items)
      '()
      (cons (eval-item (car items) env)
            (eval-items (cdr items) env))))

;; Funcion: eval-item
;; Proposito: Evalúa una entrada individual: puede ser literal booleana o una variable.
;; Parametros:
;;   item: Elemento de entrada (input-item).
;;   env: Entorno de variables.
;; Retorno: Valor booleano correspondiente.

(define (eval-item item env)
  (cases input-item item
    [bool-input (bool) (eval-bool bool)]
    [id-input (id) (apply-env env id)]))

;; Funcion: and-fn
;; Proposito: Función auxiliar que implementa la operación lógica AND.
;; Parametros: a, b: booleanos.
;; Retorno: a AND b.

(define (and-fn a b)
  (and a b))

;; Funcion: or-fn
;; Proposito: Función auxiliar que implementa la operación lógica OR.
;; Parametros: a, b: booleanos.
;; Retorno: a OR b.

(define (or-fn a b)
  (or a b))

;; Funcion: multi-xor
;; Proposito: Evalúa una operación XOR múltiple sobre una lista de booleanos.
;; Parametros:
;;   lst: Lista de valores booleanos.
;; Retorno: #t si hay un número impar de verdaderos; #f en caso contrario.

(define (multi-xor lst)
  (define (count-trues lst)
    (if (null? lst)
        0
        (+ (if (car lst) 1 0)
           (count-trues (cdr lst)))))
  (odd? (count-trues lst)))

;; Funcion: last
;; Proposito: Devuelve el último elemento de una lista.
;; Parametros:
;;   lst: Lista no vacía.
;; Retorno: Último elemento de la lista.

(define (last lst)
  (cond
    [(null? lst) (eopl:error 'last "lista vacía")]
    [(null? (cdr lst)) (car lst)]
    [else (last (cdr lst))]))

;; Funcion: connect-circuits
;; Proposito: Conecta dos circuitos reemplazando una entrada en el segundo con la salida del primero.
;; Parametros:
;;   c1: Primer circuito.
;;   c2: Segundo circuito.
;;   input-to-replace: Identificador a reemplazar.
;;   env: Entorno (no usado aquí).
;; Retorno: Nuevo circuito con las compuertas de c1 y c2 conectadas.

(define (connect-circuits c1 c2 input-to-replace env)
  (let* ([gates1 (extract-gates c1)]
         [last-id (gate-id (last gates1))]
         [gates2 (update-gates (extract-gates c2) input-to-replace last-id)])
    (make-circuit (append gates1 gates2))))

;; Funcion: extract-gates
;; Proposito: Extrae la lista de compuertas desde una estructura de circuito.
;; Parametros:
;;   circuit: Circuito lógico (estructura tipo a-circuit).
;; Retorno: Lista de compuertas.

(define (extract-gates circuit)
  (cases circuit-exp circuit
    [a-circuit (glist)
      (cases gate_list glist
        [agate-list (gates) gates])]))

;; Funcion: update-gates
;; Proposito: Actualiza las entradas de compuertas para reemplazar un identificador específico.
;; Parametros:
;;   gates: Lista de compuertas.
;;   target-id: Identificador a reemplazar.
;;   replacement-id: Identificador nuevo.
;; Retorno: Lista de compuertas actualizadas.

(define (update-gates gates target-id replacement-id)
  (map (lambda (g)
        (cases gate g
          [a-gate (id ty inputs)
            (a-gate id ty (update-inputs inputs target-id replacement-id))]))
      gates))

;; Funcion: update-inputs
;; Proposito: Actualiza los identificadores de entrada dentro de una estructura input-list.
;; Parametros:
;;   inputs: Lista de entradas (input-list).
;;   target-id: Identificador a reemplazar.
;;   replacement-id: Nuevo identificador.
;; Retorno: input-list con las entradas actualizadas.

(define (update-inputs inputs target-id replacement-id)
  (input-list
   (map (lambda (inp)
          (cases input-item inp
            [bool-input (b) inp]
            [id-input (id)
              (if (eqv? id target-id)
                  (id-input replacement-id)
                  inp)]))
        (input-list-items inputs))))

;; Funcion: merge-circuits
;; Proposito: Une dos circuitos y agrega una compuerta lógica que conecta ambas salidas.
;; Parametros:
;;   c1, c2: Circuitos a unir.
;;   typ-op: Operación lógica para la nueva compuerta (and-op, or-op, etc).
;;   out-id: Identificador de salida de la nueva compuerta.
;;   env: Entorno (no usado aquí).
;; Retorno: Nuevo circuito que representa la unión de c1 y c2 conectados mediante typ-op.

(define (merge-circuits c1 c2 typ-op out-id env)
  (let* ([gates1 (extract-gates c1)]
         [gates2 (extract-gates c2)]
         [last-id1 (gate-id (last gates1))]
         [last-id2 (gate-id (last gates2))]
         [new-gate (create-merged-gate typ-op out-id last-id1 last-id2)])
    (make-circuit (append gates1 gates2 (list new-gate)))))

;; Funcion: create-merged-gate
;; Proposito: Crea una nueva compuerta lógica que conecta dos identificadores.
;; Parametros:
;;   typ-op: Tipo de operación lógica.
;;   out-id: Identificador de salida.
;;   in1, in2: Identificadores de entrada.
;; Retorno: Compuerta lógica (estructura a-gate).

(define (create-merged-gate typ-op out-id in1 in2)
  (a-gate out-id
          (type-exp typ-op)
          (input-list (list (id-input in1) (id-input in2)))))

;; Funcion: gate-id
;; Proposito: Extrae el identificador de una compuerta lógica.
;; Parametros:
;;   g: Compuerta (estructura a-gate).
;; Retorno: Identificador de la compuerta.

(define (gate-id g)
  (cases gate g
    [a-gate (id type inputs) id]))  

;; Funcion: make-circuit
;; Proposito: Construye una estructura de circuito a partir de una lista de compuertas.
;; Parametros:
;;   gates: Lista de compuertas.
;; Retorno: Circuito (estructura a-circuit).

(define (make-circuit gates)
  (a-circuit (agate-list gates)))

;; Funcion: input-list-items
;; Proposito: Extrae la lista de entradas desde una estructura input-list.
;; Parametros:
;;   il: input-list.
;; Retorno: Lista de entradas (input-item).

(define (input-list-items il)
  (cases input_list il
    [input-list (items) items]))

;******************************************************************************************
;Pruebas

;(show-the-datatypes)
just-scan
scan&parse
;(display (scan&parse "ref-registro({x = 10; y = 20}, x)"))
;(display (show-the-datatypes))
;(newline)
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

; Crear un registro
;crear-registro(x = 42; y = True; mensaje = "hola")
;registros?(crear-registro(a = 1))

; crear-registro(x = 10; y = 20)

;var A = True, c1 = circuit ( (gate G1 (not) (A)) ) in eval-circuit(c1)
;var A = True, B = True, c1 = circuit ( (gate G2 (and) (A B)) ) in eval-circuit(c1) 

(interpretador)