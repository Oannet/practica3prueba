#lang nanopass
(require "lexer.rkt"
         parser-tools/yacc)

(provide contenedores
         vacios
         jelly-lexer)

#|
Definimos las estructuras y expresiones con sus 
respectivos cuerpos dentro de ellas para que trabajen
como nodos en el árbol.
|#
(define-struct bin-exp (op arg1 arg2) #:transparent)
(define-struct sin-exp (op arg1) #:transparent)
(define-struct number (n) #:transparent)
(define-struct bool (n) #:transparent)
(define-struct id (i) #:transparent)
(define-struct decl-var (i type) #:transparent)
(define-struct array (type) #:transparent)
(define-struct decl-arr (i type args) #:transparent)
(define-struct length (var) #:transparent)
(define-struct int-type ( ) #:transparent)
(define-struct bool-type ( ) #:transparent)
(define-struct fun (id  args body) #:transparent)
(define-struct if-exp (conditional yes no) #:transparent)
(define-struct if-short-exp (declaration conditional yes no) #:transparent)
(define-struct while-exp (conditional body) #:transparent)
(define-struct funcion-expr (id args type body) #:transparent)
(define-struct funcall (id args) #:transparent)
(define-struct main (exp func-list) #:transparent)
(define-struct program (m met) #:transparent)
(define-struct return (exp) #:transparent)

(define jelly-parser
  (parser
    [start program]    ; Simbolo inicial de la gramatica
    [end EOF]       ; Paramos cuando veamos el token EOF
    [tokens contenedores vacios] ; Tokens que reconocerá el parser
    [error void]    ; Procedimiento si encontramos un error
..... ;continua aqui con el codigo