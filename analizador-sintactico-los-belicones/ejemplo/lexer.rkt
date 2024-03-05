#lang nanopass

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))
#|
       INTEGRANTES:
Aguilera Moreno Adrian.
Rojas Reyes Saul Adrian.
Aquino Chapa Armando Abraham.
Alejandro Gutierrez Medina Sebastián.
|#

;; Lexer y Tokens.

(define-tokens contenedores (ID BOOLEAN NUM)) ; Token para identificadores, e.g. var1 => (ID var1).
(define-empty-tokens vacios (
                                 LP      RP     ; (    )
                                 LC      RC     ; [    ]
                                 LL      RL     ; {    }
                                 MAIN
                                 IF
                                 ELSE
                                 LENGTH
                                 WHILE
                                 RETURN
                                 SUM     REST   ; +    -
                                 DIV     MULT   ; /    *
                                 MOD     1D     ; %    ==
                                 DIF     MEN    ; !=   <
                                 MAY     ME1D   ; >    <=
                                 MA1D    AND    ; >=   &
                                 OR      NOT    ; |    !
                                 ASIG           ; =
                                 MASI    MENI   ; +=   -=
                                 INC     DEC    ; ++   --
                                 TYP     QIN    ; :    ?
                                 COM            ; ,
                                 INT     BOOL
                                 EOF
                             ))  ; Tokens de tipos, e.g. LP => (

(define jelly-lex
  (lexer
   ; [(Exp. regular) (token)]
   [" "   (jelly-lex input-port)]          ; Espacios vacíos.
   [(:: "," )             (token-COM)    ] ; ,
   [(:: "main" )          (token-MAIN)   ] ; if
   [(:: "if" )            (token-IF)     ] ; else
   [(:: "else" )          (token-ELSE)   ] ; then
   [(:: "?" )             (token-QIN)    ] ; ?
   [(:: "while" )         (token-WHILE)  ] ; while
   [(:: "return" )        (token-RETURN) ] ; return
   [(:: "length" )        (token-LENGTH) ] ; length
   ["bool"                (token-BOOLEAN)] ; bool
   ["int"                 (token-INT) ]
   ["num"                 (token-NUM)    ] ; num
   [(:: "(" )             (token-LP)     ] ; (
   [(:: ")" )             (token-RP)     ] ; )
   [(:: "[" )             (token-LC)     ] ; [
   [(:: "]" )             (token-RC)     ] ; ]
   [(:: "{" )             (token-LL)     ] ; {
   [(:: "}" )             (token-RL)     ] ; }
   [(:: ":" )             (token-TYP)    ] ; :
   [(:= 1 "=")            (token-ASIG)   ] ; =
   [(:= 1 "+")            (token-SUM)    ] ; +
   [(:: "/")              (token-DIV)    ] ; /
   [(:: "*" )             (token-MULT)   ] ; *
   [(:= 1 "-")            (token-REST)   ] ; -
   [(:: #\! )             (token-NOT)    ] ; !
   [(:: "!=" )            (token-DIF)    ] ; !=
   [(:: "==" )            (token-1D)     ] ; ==   
   [(:: "%" )             (token-MOD)    ] ; %
   [(:: "<=" )            (token-ME1D)   ] ; <=
   [(:: ">=" )            (token-MA1D)   ] ; >=
   [(:: "<" )             (token-MEN)    ] ; <
   [(:: ">" )             (token-MAY)    ] ; >
   [(:: "&" )             (token-AND)    ] ; &
   [(:: "|" )             (token-OR)     ] ; |
   [(:: "+=" )            (token-MASI)   ] ; +=
   [(:: "++" )            (token-INC)    ] ; ++
   [(:: "-=" )            (token-MENI)   ] ; -=
   [(:: "--" )            (token-DEC)    ] ; --
   
   ; Identificadores.
   [(:+ numeric) (token-NUM lexeme)]
   [(:or "True" "False") (token-BOOLEAN lexeme)]
   [(:: (char-range #\a #\z) (:* (:or numeric alphabetic #\_))) (token-ID lexeme)]
   ; Por último agregamos la expresion regular para los comentarios
   [(:: "{-" (complement (:: any-string "-}" any-string)) "-}") (jelly-lex input-port)]
   [(:: "//" (complement (:: any-string "\n" any-string)) "\n") (jelly-lex input-port)]
   [(eof)      (token-EOF)]))

;; Función que lexea una cadena a nuestra gramática (función de prueba):
(define (lexear s) (let* ([input (open-input-string s)])
    (print (jelly-lex input))))
; Ejemplo de prueba (escribir en terminal): (lexear "main() {5+3 4+3}")