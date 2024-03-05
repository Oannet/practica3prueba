#lang nanopass
(require nanopass/base)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
         (provide (all-defined-out))
#|
    Definimos los tokens que se utilizaran en el lenguaje
    ID: Identificador
    NUM: Numero
    BOOLEAN: Booleano
|#
(define-tokens contenedores (ID NUM BOOLEAN))
#|
    Definimos los tokens que se utilizaran en el lenguaje
    están especificados en el PDF de la práctica
|#
(define-empty-tokens vacios 
                            (MAIN
                            INT
                            BOOL
                            LK
                            RB
                            LP
                            RP
                            LBr
                            RBr
                            EQ
                            NEQ
                            DEC
                            ++
                            +
                            - 
                            * 
                            /
                            =
                            >
                            >=
                            <
                            <= 
                            +=
                            -= 
                            *= 
                            /= 
                            :
                            COM
                            MOD
                            AND
                            OR
                            NOT
                            COND
                            IF
                            ELSE
                            WHILE
                            RETURN
                            ARRAY
                            LEN
                            QUESTION_MARK
                            EOF))
#|
    Definimos las expresiones regulares que se utilizaran en el lenguaje
    están especificados en el PDF de la práctica
|#
(define jelly-lex
    (lexer
        [(:: "main") (token-MAIN)]
        [(:: "int") (token-INT)]
        [(:: "?") (token-QUESTION_MARK)]
        [(:: "true")(token-BOOL)]
        [(:: "false")(token-BOOL)]
        [(:: "bool") (token-BOOL)]
        [(:: "{") (token-LK)]
        [(:: "}") (token-RB)]
        [(:: "(") (token-LP)]
        [(:: ")") (token-RP)]
        [(:: "[") (token-LBr)]
        [(:: "]") (token-RBr)]
        [(:: "=") (token-=)]
        [(:: "!=") '(!=)]
        [(:: "--")(token-DEC)]
        [(:: "++")(token-++)]
        [(:: #\+)(token-+)]
        [(:: #\-)(token--)]
        [(:: #\*)(token-*)]
        [(:: #\/)(token-/)]
        [(:: #\>)(token->)]
        [(:: ">=")(token->=)]
        [(:: #\<)(token-<)]
        [(:: "<=")(token-<=)]
        [(:: #\<)(token-<)]
        [(:: "+=")(token-+=)]
        [(:: "-=")(token--=)]
        [(:: "*=")(token-*=)]
        [(:: "/=")(token-/=)]
        [(:: #\:)(token-:)]
        [(:: ",") (token-COM)]
        [(:: #\%)(token-MOD)]       
        [(:or #\& "and")(token-AND)]
        [(:or #\| "or")(token-OR)]
        [(:or #\! "not")(token-NOT)]
        [(:: "if")(token-IF)]
        [(:: "cond")(token-COND)]
        [(:: "else")(token-ELSE)]
        [(:: "while")(token-WHILE)]
        [(:: "return")(token-RETURN)]
        [(:: "length") (token-LEN)]
        [(:: #\=)(token-=)]
        [(:: #\:)(token-:)]
        [(:+ (char-range #\0 #\9))  (token-NUM (string->number lexeme))]
        [(:seq (char-range #\a #\z) (:+ (:or (char-range #\a #\z) (char-range #\0 #\9) #\_)))  (token-ID lexeme)] 
        [(:+ (char-range #\a #\z))(token-ID lexeme)]
       #| [(:: #\[ (+: (or (char-range #\0 #\9) (char-range #\a #\z) #\space)) #\]) 
         (token-ARRAY (substring lexeme 1 (sub1 (string-length lexeme))))]|#
        [(:: "//" (complement (:: any-string "\n" any-string)) "\n") (jelly-lex input-port)]
        [(:: "{-" (complement (:: any-string "-}" any-string)) "-}") (jelly-lex input-port)]
        [whitespace (jelly-lex input-port)]
        [any-char (error "Lexema no reconocido: " lexeme)]
        [(eof) (token-EOF)]))
        
#| 
Función para leer un archivo y devolver una lista de tokens,
|#
(define (lexa s)
  (let* ([input (open-input-file s)]
         [tokens '()])
    (let loop ()
      (let ([token (jelly-lex input)])
        (unless (eq? token 'EOF)
          (set! tokens (cons (if (pair? token) (token->readable token) token) tokens))
          (loop))))
    (close-input-port input)
    (reverse tokens)))
    
#| 
Función jelly-lex para procesar un puerto de entrada directamente,
permitiendo la flexibilidad en la entrada de datos para el lexer. 
|#
(define (jelly-lexer input-port)
  (let ([tokens '()])
    (let loop ()
      (let ([token (jelly-lex input-port)])
        (unless (eq? token 'EOF)
          (set! tokens (cons (if (pair? token) (token->readable token) token) tokens))
          (loop))))
    (reverse tokens)))
 
#| 
Función para convertir tokens a una forma legible.
|#
(define (token->readable token)
  (match token
    [(list 'token-ID lexeme) `(token 'ID ,lexeme)]
    [(list 'token-NUM value) `(token 'NUM ,value)]
    [else token])) 

#| 
Función para convertir una cadena a tokens de forma legible,
|#
(define (lexa-readable s)
  (map token->readable (lexa s)))





