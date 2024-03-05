#lang nanopass

;; Cambiar -- por //
;; es bool no boolean

; GRAMATICA DE MI LENGUAJE
; expr    -> expr + expr | ! expr | constante | id | ( expr )
; id      -> [a..z]+
; consante <- [0..9]+

; ARBOL DE SINTAXIS CONCRETA PARA "3 + var"
;         expr
;        / | \
;    expr  +  expr
;     |        |
;   const      id
;     |        |
;     3        var

; ARBOL DE SINTAXIS ABSTRACTA PARA 3 + (2 - 4)
;         +
;        / \
;       3   -
;          / \
;         2   4

(require "lexer_tokens.rkt"
         parser-tools/yacc)
(provide (all-defined-out))

#|
       INTEGRANTES:
Aguilera Moreno Adrian.
Rojas Reyes Saul Adrian.
Aquino Chapa Armando Abraham.
Gutierrez Medina Sebastián Alejandro.
|#

;ESTRUCTURAS PARA LOS NODOS DEL ARBOL
(define-struct id (i) #:transparent) 
(define-struct num (n) #:transparent)
(define-struct bool (b) #:transparent)
(define-struct return (exp) #:transparent)
(define-struct length (exp) #:transparent)
(define-struct arg (id tipo) #:transparent)
(define-struct un-expr (op e) #:transparent)
(define-struct if-corto (g t e) #:transparent)                ; ( ... ) ? 
(define-struct call (nombre arg) #:transparent)
(define-struct my-while (exp then) #:transparent)
(define-struct bin-expr (op o1 o2) #:transparent)
(define-struct main (instrucciones) #:transparent)
(define-struct my-if (exp then else) #:transparent)
(define-struct arreglo (id tipo tamano) #:transparent)           ; arreglo
(define-struct procedimiento (nombre argumentos tipo instrucciones) #:transparent)

(define jelly-parser
    (parser
     [start programa]
     [end EOF]
     [tokens contenedores vacios] 
     [error ; procedimiento que tiene que seguir el parser cuando encuentre un error
      (lambda (tok-ok? tok-name tok-value)
        (raise-syntax-error 'error "Tu token no es admisible: "
                            (if tok-value tok-value tok-name)))]
       
      [precs (nonassoc LP)     ; (
             (nonassoc RP)     ; )
             (nonassoc LC)     ; [
             (nonassoc RC)     ; ]
             (nonassoc LL)     ; {
             (nonassoc RL)     ; }
             (nonassoc IF)     ; if
             (nonassoc ELSE)   ; else
             (nonassoc NOT)    ; !
             (nonassoc MAIN)   ; main
             (nonassoc COM)    ; ,
             
            ; FALTA TYP ":"
             (right RETURN)    ; return
             (right INC)       ; ++
             (right DEC)       ; --
             (right QIN)       ; ?
             (right ASIG)      ; =   
             (right MASI)      ; +=
             (right MENI)      ; -=
             (left MULT)       ; *
             (left DIV)        ; /
             (left MOD)        ; %
             (left SUM)        ; +
             (left REST)       ; -
             (left MEN)        ; <
             (left MAY)        ; >
             (left ME1D)       ; <=
             (left MA1D)       ; >; while ( ... ) { ... }
             (left 1D)         ; ==
             (left DIF)        ; !=
             (left AND)        ; &
             (left OR)         ; |
      ]
      
      [grammar
        [programa
            [(procedimientos) $1]
            [(main) $1]
            [(main procedimientos) (list* $1 $2)]]
        [main
            [(MAIN LP RP instrucciones) (main $4)]]
        ; Obs. Quite los métodos y funciones y los metí aquí (pocos shift/reduce adelante):
        [procedimiento
         [(id LP declaraciones RP TYP tipo instrucciones) (procedimiento $1 $3 $6 $7)]
        ]
        ; Recursión de procedimiento:
        [procedimientos
         [(procedimiento) $1]
         [(procedimiento procedimientos) (list* $1 $2)]
        ]
        
        (id
            [(ID) (id $1)])                         
        (return
            [(RETURN expresion) (return $2)])
       
        
        [sentencias ; Acciones:  S -> ...
         [(estructura) $1]
         [(estructura sentencias) (list* $1 $2)]
        ]

        ; Diferentes estructuras:
        [estructura  ; estructuras
            [(if)              $1]
            [(call)            $1]                       
            [(while)           $1]                            
            ;[(argumento)       $1]    ; Genera shift/reduce por declaraciones (No es necesario).
            [(if-corto)        $1]     ; TODO. shift/reduce por causa del if
            [(return)          $1]
            [(length)          $1]
            [(asignacion)      $1]                       
            [(declaraciones)   $1]
            [(instrucciones)   $1]
        ] 
        
        ; length
         [length
          [(LENGTH LP id RP) (length $3)]
         ]

         ; if ( ... ) then { ... } else { ... }
         [if
          [(IF expresion estructura ELSE estructura)   (my-if $2 $3 $5)]
         ]

         ; ( ... ) ? { ... } : { ... }
         [if-corto
          [(LP expresion RP QIN estructura TYP estructura) (if-corto $2 $5 $7)]
         ]

         ; while ( ... ) { ... }
         [while
          [(WHILE expresion instrucciones) (my-while $2 $3)]
         ]
        
         [asignacion
          [(argumento ASIG expresion)               (bin-expr '= $1 $3)]
          ; Asignación para arreglos con declaración incluida:
          [(argumento ASIG LL constantes RL)        (bin-expr '= $1 $4)]
          [(id ASIG expresion)                      (bin-expr '= $1 $3)]
          ; Asignación múltiple:
          [(id ASIG asignacion)                     (bin-expr '= $1 $3)]
          ; Asignación de arreglo declarado (múltiple):
          [(id ASIG LL constantes RL)               (bin-expr '= $1 $4)]
         ]     

         ; Declaraciones de argumentos y valores en general:
         [declaraciones
          [(argumento COM declaraciones) (list* $1 $3) ]
          [(argumento)                   $1            ]
         ]

         ; { ... }
         [instrucciones
          [(LL RL)            empty ]
          [(LL expresion RL)  $2    ]         ; TODO. shift/reduce   (Para que acepte cosas como: 5+5)
          [(LL sentencias RL) $2    ]
         ]

         ; Distintos datos declarados
         [argumento
          [(id TYP tipo LC NUM RC)     (arreglo $1 $3 $5)   ] ; TODO. Agregarle id a la representación.
          ; $1  $2  $3
          ; id  :  tipo [   ]
          [(id TYP tipo LC RC)         (arreglo $1 $3 empty) ]
          [(id TYP tipo)               (arg $1 $3)       ]
         ]

         ; Valor constante:
         [constante
          [(NUM)      (num $1)]
          [(BOOLEAN)  (bool $1)]
         ]
        
         ; Recursión con constante:
         [constantes
          [(constante COM constantes)         (list* $1 $3)]
          [(constante)                        (list $1)]
         ]

         ; llamadas, invocaciones, etc:
         [call
          [(id LP expresiones RP) (call $1 $3)]
         ]

         ; Expresion: puede ser una operación unaria o binaria:
         [expresion ; TODO. Esto puede tronar
          [(LP expresion RP)           $2                   ]                             
          [(constante)                 $1                   ]
          [(id)                        $1                   ]
          [(expresion OR   expresion)  (bin-expr '\| $1 $3) ]      
          [(expresion AND  expresion)  (bin-expr '&  $1 $3) ]      
          [(expresion 1D   expresion)  (bin-expr '== $1 $3) ]      
          [(expresion DIF  expresion)  (bin-expr '!= $1 $3) ]      
          [(expresion MEN  expresion)  (bin-expr '<  $1 $3) ]        
          [(expresion MAY  expresion)  (bin-expr '>  $1 $3) ]        
          [(expresion MA1D expresion)  (bin-expr '>= $1 $3) ]      
          [(expresion ME1D expresion)  (bin-expr '<= $1 $3) ]      
          [(expresion SUM  expresion)  (bin-expr '+  $1 $3) ]        
          [(expresion REST expresion)  (bin-expr '-  $1 $3) ]
          [(expresion INC)             (bin-expr '+ $1 1)   ] ; Quitamos azucar sintáctica
          [(expresion DEC)             (bin-expr '- $1 1)   ] ; Quitamos azucar sintáctica
          [(expresion MASI expresion)  (bin-expr '+ $1 $3)  ] ; Quitamos azucar sintáctica
          [(expresion MENI expresion)  (bin-expr '- $1 $3)  ] ; Quitamos azucar sintáctica
          [(expresion MULT expresion)  (bin-expr '*  $1 $3) ]        
          [(expresion DIV  expresion)  (bin-expr '/  $1 $3) ]        
          [(expresion MOD  expresion)  (bin-expr '%  $1 $3) ]        
          [(REST expresion)            (un-expr  '-  $2)    ]                
          [(NOT expresion)             (un-expr  '!  $2)    ]
          [(call)                      $1                   ]
         ]

         ; Recursión con expresion:
         [expresiones
          [(expresion COM expresiones)  (list* $1 $3)]
          [(expresion) $1]
         ]

         ; Sistema de tipos:
         [tipo
          [(INT)    'INT]
          [(BOOL)   'BOOL]
         ]
        ]
    )
)

; Pruebas
(define (lex lexer input) (lambda () (lexer input)))
;(jelly-parser (lex jelly-lex (open-input-string "main() { if 5+3 > 4 { return 4+3} else {2} }")))
;(jelly-parser (lex jelly-lex (open-input-string "main() { if 5+3 > 4 {return 4+3} else { return 2+3} a = b = c =  4+5 }")))
;(jelly-parser (lex jelly-lex (open-input-string "main() { a = true}")))
(jelly-parser (lex jelly-lex (open-input-string "main() {a: int[] = {1,2,3,4,5} a:int = 3}")))
;(jelly-parser (lex jelly-lex (open-input-string "main() {length(a)}")))