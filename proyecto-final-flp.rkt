#lang eopl

;; Proyecto Final - Fundamentos de Lenguaje de Programación - FLP
;; Autores: Paola Andrea Domínguez - 202059956, Valentina Cobo  - 202060174, Juan David Moreno - 202059997, Juan Felipe Jaramillo - 202060257


;; Especificación léxica que define como será tratado cada elemento por el interpretador
(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("*" (arbno (not #\newline)) "*") skip)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit)"." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)"." digit (arbno digit)) number)
    (caracter ( "\'" letter "\'") symbol)
    (cadena ("\""(or letter digit) (arbno (or letter digit "?" "$"))"\"") string)
    )
  )
 
;; Especificación gramatical que define que tipo de expresiones y otros elementos existen en el interpretador. 
(define especificacion-gramatical
  '(
    (programa (expresion) a-program)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) id-exp)
    (expresion (numero) num-exp)
    (expresion (caracter) carac-exp)
    (expresion (cadena) string-exp)
    (expresion ("ok") ok-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") exp-primitiva)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion "end") var-exp)
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion "end") letrec-exp)
    (expresion ("set" identificador ":=" expresion) set-exp)
    (expresion ("begin" expresion (separated-list expresion ",") "end") begin-exp)
    (expresion ("if" bool-expresion "then" expresion (arbno "elseif" bool-expresion "then" expresion) "else" expresion "end") if-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    (expresion ("meth" "(" identificador "," (separated-list identificador ",") ")" expresion "end") meth-exp)
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)
    (expresion ("object" "{" (arbno identificador "=>" expresion) "}" ) object-exp)
    (expresion ("get" identificador "." identificador) get-exp)
    (expresion ("send" identificador "." identificador "(" (separated-list expresion ",") ")") send-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("clone" "(" identificador (separated-list identificador ",") ")" ) clone-exp)
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)
    (bool-expresion ( bool-primitiva "(" (separated-list expresion ",") ")" ) bool-prim)
    (bool-expresion ( bool-oper "(" (separated-list bool-expresion ",") ")") bool-operation)
    (bool-primitiva (">") greater-prim)
    (bool-primitiva ("<") lesser-prim)
    (bool-primitiva ("<=") lesser-or-equal-prim)
    (bool-primitiva (">=") greater-or-equal-prim)
    (bool-primitiva ("is") is-prim)
    (bool-oper ("and") and-prim)
    (bool-oper ("or") or-prim)
    (bool-oper ("not") not-prim)
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva ("&") txt-prim)
    
    )
  )
 
;;Creamos los datatypes automaticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

 
 
;;Evaluar programa
;; programa -> expresion
;; Función que permite evaluar el programa ingresado al interpretador
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
                 (evaluar-expresion exp ambiente-inicial))
      ))
  )
 
 
 
;; Definición de los datatypes que de los ambientes normales y ambientes extendidos

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))


;;ambiente-extendido
;; lista-simbolos, lista-expresiones, ambiente -> ambiente extendido
(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))

 
;; ambiente-extendido-recursivo
;; lista-identificadores, lista-lista-identificadores, lista-expresiones, ambiente -> ambiente extendido recursivamente
;; Implementación del ambiente extendido recursivamente
(define ambiente-extendido-recursivo
  (lambda (procnames lidss cuerpos old-env)
    (let
        (
         (vec-clausuras (make-vector (length procnames)))
         )
      (letrec
          (
           (amb (ambiente-extendido-ref procnames vec-clausuras old-env))
           (obtener-clausuras
            (lambda (lidss cuerpos pos)
              (cond
                [(null? lidss) amb]
                [else
                 (begin
                   (vector-set! vec-clausuras pos
                                (closure (car lidss) (car cuerpos) amb))
                   (obtener-clausuras (cdr lidss) (cdr cuerpos) (+ pos 1)))]
                )
              )
            )
           )
        (obtener-clausuras lidss cuerpos 0)
        )
      )
    )
  )


;; apply-env
;; ambiente, identificador -> expresion
;; Definición de funciones que se encargan de buscar un identificador en el ambiente y retornarlo si se encuentra
 (define apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))))


;; apply-env-ref
;; ambiente, identificador -> expresion
;; Función auxiliar que busca la variable en el ambiente
(define apply-env-ref
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido-ref (lid vec old-env)
                          (letrec
                              (
                               (buscar-variable (lambda (lid vec pos)
                                                  (cond
                                                    [(null? lid) (apply-env-ref old-env var)]
                                                    [(equal? (car lid) var) (a-ref pos vec)]
                                                    [else
                                                     (buscar-variable (cdr lid) vec (+ pos 1)  )]
                                                    )
                                                  )
                                                )
                               )
                            (buscar-variable lid vec 0)
                            )
                          
                          )
      
      )
    )
  )


;; Definición del ambiente inicial para trabajar ejemplos
(define ambiente-inicial
  (ambiente-extendido '(x y z) '(4 2 5)
                      (ambiente-extendido '(a b c) '(6 5 6)
                                          (ambiente-vacio))))

 
;; evaluar-expresión
;; expresion -> valor
;; Esta función se encarga de evaluar cada una de las posibles expresiones de la especificación gramatical y darle el tratamiento adecuado de acuerdo al tipo
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (bool-exp (bool) (evaluar-bool-expresion bool amb))
      (id-exp (id) (apply-env amb id))
      (num-exp (numero) numero)
      (carac-exp (caracter) caracter)
      (string-exp (cadena) cadena)
      (ok-exp () "te amamos cardel :3" )
      (exp-primitiva (prim args)
                     (let
                         (
                          (lista-numeros (map (lambda (x) (evaluar-expresion x amb)) args))
                          )
                       (evaluar-primitiva prim lista-numeros)
                       )
                     )
      (var-exp (lids lexp exp)
               (let
                   (
                    (lvalues (map (lambda (x) (evaluar-expresion-sin-set x amb)) lexp))
                    )
                 (evaluar-expresion-sin-set exp (ambiente-extendido lids lvalues amb))
                 )
               )
      (let-exp (ids rands body)
               (let
                   (
                    (lvalues (map (lambda (x) (evaluar-expresion x amb)) rands))
                    
                    )
                 (evaluar-expresion body (ambiente-extendido ids lvalues amb))
                 )
               )
       (letrec-exp (procnames idss cuerpos cuerpo-letrec)
                  (evaluar-expresion cuerpo-letrec
                                     (ambiente-extendido-recursivo procnames idss cuerpos amb)))
      
      (set-exp (id exp)
               (begin
                 (setref!
                  (apply-env-ref amb id)
                  (evaluar-expresion exp amb))
                 1)
               )
      (begin-exp (exp lexp)
                 (if
                  (null? lexp)
                  (evaluar-expresion exp amb)
                  (begin
                    (evaluar-expresion exp amb)
                    (letrec
                        (
                         (evaluar-begin (lambda (lexp)
                                          (cond
                                            [(null? (cdr lexp)) (evaluar-expresion (car lexp) amb)]
                                            [else
                                             (begin
                                               (evaluar-expresion (car lexp) amb)
                                               (evaluar-begin (cdr lexp))
                                               )
                                             ]
                                            )
                                          )
                                        )
                         )
                      (evaluar-begin lexp)
                      )
                    )
                  )
                 )
      (if-exp (condicion-inicial hace-verdadero lcondiciones lexpresiones hace-falso)
              (let
                  (
                   (lista-condiciones (map (lambda (x) (evaluar-bool-expresion x amb))lcondiciones))
                   (lista-expresiones (map (lambda (x) (evaluar-expresion x amb))lexpresiones))
                   )
                (if
                 (evaluar-bool-expresion condicion-inicial amb)
                 (evaluar-expresion hace-verdadero amb)
                 (letrec
                    (
                     (encontrar-true (lambda (lista-cond lista-expre)
                                     (cond
                                       [(null? lista-cond)(evaluar-expresion hace-falso amb)]
                                       [(equal? (car lista-cond) #t)(car lista-expre)]
                                       [else (encontrar-true (cdr lista-cond) (cdr lista-expre))])))
                     )
                    (encontrar-true lista-condiciones lista-expresiones)))))
      
      (proc-exp (ids body)
                (closure ids body amb))
      (meth-exp (idinicial restofids body)
                
                (closure (append  (list idinicial) restofids) body amb)
                   
             )
      
      (apply-exp (rator rands)
                 (let
                     (
                      (lrands (map (lambda (x) (evaluar-expresion x amb)) rands))
                      (procV (apply-env amb rator))
                      
                      )
                    (if
                    (procval? procV)
                    (cases procval procV
                      (closure (lid body old-env)
                               (if (= (length lid) (length lrands))
                                   (evaluar-expresion body
                                                      (ambiente-extendido lid lrands old-env))
                                   (eopl:error "El número de argumentos no es correcto, debe enviar" (length lid)  " y usted ha enviado" (length lrands))
                                   )
                               ))
                    (eopl:error "No puede evaluarse algo que no sea un procedimiento" procV) 
                    )
                   )
                 )
         
      (for-exp (id valor cond-parada hacer)
                 (letrec
                   (
                    (lvalues (list (evaluar-expresion valor amb)))
                    (parada (evaluar-expresion cond-parada amb))
                    (ide (ambiente-extendido (list id) lvalues amb))
                    (iterador (lambda (ide val parada hacer)
                                (cond
                                  [(equal? (car val) parada)(evaluar-expresion hacer ide)]
                                  [else  (iterador (ambiente-extendido (list id) (list (+ 1 (car val))) amb)
                                                                (list (+ 1 (car val))) parada hacer)])))
                                                      
                    )
                   (iterador ide lvalues parada hacer)))
(object-exp (lid lexp)
                  (let
                      (
                       (lvalores (map (lambda (x) (evaluar-expresion x amb)) lexp))
                       )
                    (letrec
                        (
                         (crear-objeto
                          (lambda (lid lval)
                            (cond
                              [(null? lval) (object-empty)]
                              [(and (value?  (car lval)) (not (procval?(car lval))))
                               (object-atributes
                                (car lid)
                                (car lval)
                                (crear-objeto (cdr lid) (cdr lval))
 
                                )]
                              [else
                               (object-methods
                                (car lid)
                                (closure (procval->lid (car lval)) (procval->body (car lval)) (procval->amb (car lval)))
                                (crear-objeto (cdr lid) (cdr lval))
                                )
                               ]
                              )
                              
                              
                            )
 
                          )
                         )
                      
                    (crear-objeto lid lvalores)
                    )
                  ))
      (get-exp (nameobject campo)
               (if (object? (apply-env amb nameobject))
                    (obtener-datos (apply-env amb nameobject) campo)
                    (eopl:error "No puede obtenerse el valor de algo que no sea un objeto" nameobject)
                    )
               )
      (send-exp (nameobj metodo largs)
                (let
                    (
                     (largumentos (map (lambda (x) (evaluar-expresion x amb)) largs))
                     (obj (apply-env amb nameobj))
                     )
                (if (object? obj)
                    (ejecucion-metodo obj metodo largumentos amb)
                     (eopl:error "No puede evaluarse un método de algo que no sea un objeto" nameobj)
                    )
                  
                
                )
                )
      (update-exp (objeto campo valoractualizar)
                  (let
                      (
                       (val (evaluar-expresion valoractualizar amb))
                       (obj (apply-env amb objeto))
                       )
                    (if (object? obj)
                        (actualizar-datos obj campo val)
                        (eopl:error "No puede actualizarse un campo de algo que no sea un objeto" obj)
                        )
                    )
                  )
                   
 
      (else 0)
      )
    )
  )

;;Esta función validará de una forma más eficiente para el sistema si el set, esta siendo usado en un var-exp
;;por lo que hace el rol de un if sin tener que usar ambientes
(define evaluar-expresion-sin-set
  (lambda (exp amb)
    (cases expresion exp
      (bool-exp (bool) (evaluar-bool-expresion bool amb))
      (id-exp (id) (apply-env amb id))
      (num-exp (numero) numero)
      (carac-exp (caracter) caracter)
      (string-exp (cadena) cadena)
      (ok-exp () "te amamos cardel :3" )
      (exp-primitiva (prim args)
                     (let
                         (
                          (lista-numeros (map (lambda (x) (evaluar-expresion x amb)) args))
                          )
                       (evaluar-primitiva prim lista-numeros)
                       )
                     )
      (var-exp (lids lexp exp)
               (let
                   (
                    (lvalues (map (lambda (x) (evaluar-expresion x amb)) lexp))
                    )
                 (evaluar-expresion exp (ambiente-extendido lids lvalues amb))
                 )
               )
      (let-exp (ids rands body)
               (let
                   (
                    (lvalues (map (lambda (x) (evaluar-expresion x amb)) rands))
                    
                    )
                 (evaluar-expresion body (ambiente-extendido ids lvalues amb))
                 )
               )
       (letrec-exp (procnames idss cuerpos cuerpo-letrec)
                  (evaluar-expresion cuerpo-letrec
                                     (ambiente-extendido-recursivo procnames idss cuerpos amb)))
      
      (set-exp (id exp)
               (eopl:error"no se puede usar el set aqui")
               )
      (begin-exp (exp lexp)
                 (if
                  (null? lexp)
                  (evaluar-expresion exp amb)
                  (begin
                    (evaluar-expresion exp amb)
                    (letrec
                        (
                         (evaluar-begin (lambda (lexp)
                                          (cond
                                            [(null? (cdr lexp)) (evaluar-expresion (car lexp) amb)]
                                            [else
                                             (begin
                                               (evaluar-expresion (car lexp) amb)
                                               (evaluar-begin (cdr lexp))
                                               )
                                             ]
                                            )
                                          )
                                        )
                         )
                      (evaluar-begin lexp)
                      )
                    )
                  )
                 )
      (if-exp (condicion-inicial hace-verdadero lcondiciones lexpresiones hace-falso)
              (let
                  (
                   (lista-condiciones (map (lambda (x) (evaluar-bool-expresion x amb))lcondiciones))
                   (lista-expresiones (map (lambda (x) (evaluar-expresion x amb))lexpresiones))
                   )
                (if
                 (evaluar-bool-expresion condicion-inicial amb)
                 (evaluar-expresion hace-verdadero amb)
                 (letrec
                    (
                     (encontrar-true (lambda (lista-cond lista-expre)
                                     (cond
                                       [(null? lista-cond)(evaluar-expresion hace-falso amb)]
                                       [(equal? (car lista-cond) #t)(car lista-expre)]
                                       [else (encontrar-true (cdr lista-cond) (cdr lista-expre))])))
                     )
                    (encontrar-true lista-condiciones lista-expresiones)))))
      
      (proc-exp (ids body)
                (closure ids body amb))
      (meth-exp (idinicial restofids body)
                
                (closure (append  (list idinicial) restofids) body amb)
                   
             )
      
      (apply-exp (rator rands)
                 (let
                     (
                      (lrands (map (lambda (x) (evaluar-expresion x amb)) rands))
                      (procV (apply-env amb rator))
                      
                      )
                    (if
                    (procval? procV)
                    (cases procval procV
                      (closure (lid body old-env)
                               (if (= (length lid) (length lrands))
                                   (evaluar-expresion body
                                                      (ambiente-extendido lid lrands old-env))
                                   (eopl:error "El número de argumentos no es correcto, debe enviar" (length lid)  " y usted ha enviado" (length lrands))
                                   )
                               ))
                    (eopl:error "No puede evaluarse algo que no sea un procedimiento" procV) 
                    )
                   )
                 )
         
      (for-exp (id valor cond-parada hacer)
                 (letrec
                   (
                    (lvalues (list (evaluar-expresion valor amb)))
                    (parada (evaluar-expresion cond-parada amb))
                    (ide (ambiente-extendido (list id) lvalues amb))
                    (iterador (lambda (ide val parada hacer)
                                (cond
                                  [(equal? (car val) parada)(evaluar-expresion hacer ide)]
                                  [else  (iterador (ambiente-extendido (list id) (list (+ 1 (car val))) amb)
                                                                (list (+ 1 (car val))) parada hacer)])))
                                                      
                    )
                   (iterador ide lvalues parada hacer)))
(object-exp (lid lexp)
                  (let
                      (
                       (lvalores (map (lambda (x) (evaluar-expresion x amb)) lexp))
                       )
                    (letrec
                        (
                         (crear-objeto
                          (lambda (lid lval)
                            (cond
                              [(null? lval) (object-empty)]
                              [(and (value?  (car lval)) (not (procval?(car lval))))
                               (object-atributes
                                (car lid)
                                (car lval)
                                (crear-objeto (cdr lid) (cdr lval))
 
                                )]
                              [else
                               (object-methods
                                (car lid)
                                (closure (procval->lid (car lval)) (procval->body (car lval)) (procval->amb (car lval)))
                                (crear-objeto (cdr lid) (cdr lval))
                                )
                               ]
                              )
                              
                              
                            )
 
                          )
                         )
                      
                    (crear-objeto lid lvalores)
                    )
                  ))
      (get-exp (nameobject campo)
               (if (object? (apply-env amb nameobject))
                    (obtener-datos (apply-env amb nameobject) campo)
                    (eopl:error "No puede obtenerse el valor de algo que no sea un objeto" nameobject)
                    )
               )
      (send-exp (nameobj metodo largs)
                (let
                    (
                     (largumentos (map (lambda (x) (evaluar-expresion x amb)) largs))
                     (obj (apply-env amb nameobj))
                     )
                (if (object? obj)
                    (ejecucion-metodo obj metodo largumentos amb)
                     (eopl:error "No puede evaluarse un método de algo que no sea un objeto" nameobj)
                    )
                  
                
                )
                )
      (update-exp (objeto campo valoractualizar)
                  (let
                      (
                       (val (evaluar-expresion valoractualizar amb))
                       (obj (apply-env amb objeto))
                       )
                    (if (object? obj)
                        (actualizar-datos obj campo val)
                        (eopl:error "No puede actualizarse un campo de algo que no sea un objeto" obj)
                        )
                    )
                  )
                   
 
      (else 0)
      )
    )
  )

;;Extractor closure
(define procval->lid
  (lambda (cl)
    (cases procval cl
      (closure (lid body amb) lid)
      (else 0)
     )
    )
  )
 
(define procval->body
  (lambda (cl)
    (cases procval cl
      (closure (lid body amb) body)
      (else 0)
     )
    )
  )
 
(define procval->amb
  (lambda (cl)
    (cases procval cl
      (closure (lid body amb) amb)
      (else 0)
     )
    )
  )
 
;;Obtener campos de un objeto
(define obtener-datos
  (lambda (obj campo)
    (cases object obj
      (object-empty () 0)
      (object-atributes (id valor old-obj)
                        (if (equal? id campo)
                            valor
                            (obtener-datos old-obj campo)
                        )
                        )
      (object-methods (nameproc proc old-obj)
                      (obtener-datos old-obj campo)
                      )
    )
  ))
 
;;Se define una función que se encarga de ejecutar el método
(define ejecucion-metodo
  (lambda (obj metodo lrands amb)
    (cases object obj
      (object-empty () 0)
      (object-atributes (id valor old-obj)
 
                        (ejecucion-metodo old-obj metodo lrands amb)
                        
                        )
      (object-methods (nameproc proc old-obj)
                      (if (equal? nameproc metodo)
                          (if (equal? (length (procval->lid proc)) (length lrands))
                              (evaluar-expresion (procval->body proc)
                                                 (ambiente-extendido (procval->lid proc) lrands (procval->amb proc))
                                                 )
                          
                              (eopl:error "El número de argumentos no es correcto, debe enviar" (length (procval->lid proc)))
                              )
                          (ejecucion-metodo old-obj metodo lrands amb)
                          )
                                             
                                      
                      )
      )
  ))
 
 
;;Funcion para actualizar un atributo
 
 
(define actualizar-datos
  (lambda (obj campo nuevovalor)
    (cases object obj
      (object-empty () 0)
      (object-atributes (id valor old-obj)
                        (if (equal? id campo)
                            (object-atributes campo nuevovalor
                                              (actualizar-datos-aux obj)
                                              )
                            (actualizar-datos old-obj campo nuevovalor)
                                      
                            )
                        )
      (object-methods (nameproc proc old-obj)
                      (if (equal? nameproc campo)
                          (object-methods campo nuevovalor
                                          (actualizar-datos-aux obj)
                                          )
                          (actualizar-datos old-obj campo nuevovalor)
                                      
                          )
                      )
      )
    )
  )

;funcion auxiliar
(define actualizar-datos-aux
  (lambda (obj)
    (cases object obj
      (object-empty () (object-empty))
      (object-atributes (id valor old-obj)
                        (object-atributes id valor
                                          (actualizar-datos-aux old-obj )
                                          )
                        )
      (object-methods (nameproc proc old-obj)
                      (object-methods nameproc proc
                                      (actualizar-datos-aux old-obj)
                                      )
                          )
                      )
    )
  )

;; evaluar-primitiva
;; primitiva, lista -> expresion
;; Definición de la función que se encarga de evaluar las primitivas definidas en la especificación. De acuerdo a cada caso aplicará la operación correspondiente. 
(define evaluar-primitiva
  (lambda (prim lista)
    (cases primitiva prim
      (sum-prim () (operar lista + 0))
      (minus-prim () (- (car lista) (cadr lista)))
      (mult-prim () (operar lista * 1))
      (div-prim () (operar lista / 1))
      (mod-prim () (modulo (car lista) (cadr lista)))
      (txt-prim () (string-append (car lista)(cadr lista)))
      (else 0))))
 

;; operar
;; lista-valores, expresion, expresion -> expresion
;; Función que se encarga de realizar una operación brindada por el usuario a una lista de valores
(define operar
  (lambda (lst f acc [res acc])
    (cond
      [(null? lst) res]
      [else
       (operar (cdr lst) f acc (f res (car lst)))])))


;; evaluar-bool-primitiva
;; expresion, lista-valores -> expresion
;; Función que se encarga de evaluar las funciones primitivas que tienen como retorno booleanos
(define evaluar-bool-primitiva
  (lambda (bool lista)
    (cases bool-primitiva bool
      (greater-prim ()(> (car lista)(cadr lista)))
      (lesser-prim () (< (car lista)(cadr lista)))
      (lesser-or-equal-prim () (<= (car lista)(cadr lista)))
      (greater-or-equal-prim () (>= (car lista)(cadr lista)))
      (is-prim () (equal? (car lista)(cadr lista)))
      (else 0))))
 
 

;; evaluar-bool-expresion
;; expresion, ambiente
;; Función que se encarga de evaluar las posibles expresiones booleanas definidas en la especificación
(define evaluar-bool-expresion
  (lambda (bool amb)
    (cases bool-expresion bool
      (true-exp () #T)
      (false-exp () #F)
      (bool-prim (prim exp)(let
                               (
                                (lista-exp (map (lambda (x) (evaluar-expresion x amb)) exp))
                                )
                             (evaluar-bool-primitiva prim lista-exp)
                             ))
      (bool-operation (oper bool-exp)(let
                                         (
                                          (lista-bool-exp (map (lambda (x) (evaluar-bool-expresion x amb)) bool-exp))
                                          )
                                       (evaluar-bool-operador oper lista-bool-exp)
                                       ))
      (else 0)
      )))
 
 

;; evaluar-bool-operador
;; expresion, lista-valores -> expresion
;; Función que se encarga de evaluar una operación con retorno booleano en una lista de valores
(define evaluar-bool-operador
  (lambda (oper lista)
    (cases bool-oper oper
      (and-prim () (and (car lista)(cadr lista)))
      (or-prim () (or (car lista)(cadr lista)))
      (not-prim ()(not (car lista)))
      (else 0))))   


;;Definiciones para los procedimientos
(define-datatype procval procval?
  (closure (lid (list-of symbol?))
           (body expresion?)
           (amb-creation ambiente?)))
 
;; Definición de datatype para las referencias
(define-datatype referencia referencia?
  (a-ref (pos number?)
         (vec vector?)))
 
;; deref
;; referencia -> expresion
;; Función que se encargan de extraer las referencias
(define deref
  (lambda (ref)
    (primitiva-deref ref)))


;; primitiva-deref
;; referecia -> expresion
;; Función auxiliar que se encarga de extraer el valor de una referencia
(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))
 
;; setref!
;; referencia, valor -> ok
;; Función que se encarga de asignar/cambiar los valores de las referencias
(define setref!
  (lambda (ref val)
    (primitiva-setref! ref val)))


;; primitiva-setref!
;; referencia, valor -> ok
;; Función auxiliar que se encarga de asignar/cambiar los valores de las referencias
(define primitiva-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))
 
 
;Objetos datatype
(define-datatype object object?
  (object-atributes (lid  symbol?)
                    (lval value?)
                    (obj object?)
                    )
                    
  (object-methods (lnameproc  symbol?)
                  (lmeth procval?)
                   (obj object?)
                  )
  (object-empty)
 
  )

;;no mire nada mas plis
(define value?
  (lambda (v)
    (or (symbol? v) (number? v))
    ))
 
 
 
 
 
 
 
;;Interpretador
(define interpretador
  (sllgen:make-rep-loop "-->" evaluar-programa
                        (sllgen:make-stream-parser
                         especificacion-lexica especificacion-gramatical)))
 
 
 
 
 
(interpretador)

;;EJEMPLOS

;let casita= object{z=>1 m=>2} in casita end
;let  m= meth(s,n) +(n,1) end in apply m(7,6)
;let casita= object{z=>1 m=> meth(s,n) +(n,1) end} in casita endIF
