# Program Language

# Autors
<div>
  <ol>
      <li>
        Juan David Moreno
    </li>
    <li>
      Valentina Cobo
    </li>
    <li>
      Paola Andrea Domínguez
    </li>
    <li>
      Juan Felipe Jaramillo
    </li>
  </ol>
</div>

# Description 
```bash
Final Project - Fundamentos de Lenguaje de Programación - FLP
```

# About
With this project we can investigate the core ideas of programming language design, such as syntax, semantics and grammar. We can build our own language structures and create a domain-specific language that suits the unique needs of the project using Racket's sophisticated macro system.

# Some Functions
```racket
;;Defines how each element will be treated by the interpreter
(define especificacion-lexica
  '(
    ;;(here we add the lexical specification)
    )
)

;;Defines what kind of expressions and other elements exist in the interpreter.
(define especificacion-gramatical
  '(
  ;;(here we add the grammatical specification)
    )
)

;;Create the datatypes automatically
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;;evaluates the program
(define evaluar-programa
)

;;Definition of datatypes of normal and extended environments

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))

(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))

;;It's in charge of evaluating each of the possible expressions of the grammatical specification and giving it the appropriate treatment according to its type
(define evaluar-expresion
  (lambda (exp amb))
  ;;checks the exp and the amb
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

;;Evaluates primitives
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

;;Interpretador
(define interpretador
  (sllgen:make-rep-loop "-->" evaluar-programa
                        (sllgen:make-stream-parser
                         especificacion-lexica especificacion-gramatical)))
 
 
 
 
 
(interpretador)
```



