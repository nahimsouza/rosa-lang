
(* Load content from a file into a list of chars *)
fun loadFile(filename) =
    let 
        val file = TextIO.openIn filename
        val content = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in
        (String.explode content)
    end

datatype token =
    PLUS
    | MINUS
    | TIMES
    | DIV
    | LPAREN
    | RPAREN
    | LBRACKET
    | RBRACKET
    | GT
    | LT
    | EQUAL
    | DIFF
    | LEQ
    | GEQ
    | OR
    | AND
    | NOT
    | KEYWORD of string
    | INTEGER of int
    | REAL of real
    | LITERAL of char

fun tokenize nil = nil
    | tokenize (#" " :: cs) = tokenize cs 
    | tokenize (#"\n" :: cs) = tokenize cs 
    | tokenize (#"\t" :: cs) = tokenize cs 
    | tokenize (#"+" :: cs) = (PLUS :: tokenize cs)
    | tokenize (#"-" :: cs) = (MINUS :: tokenize cs)
    | tokenize (#"*" :: cs) = (TIMES :: tokenize cs)
    | tokenize (#"/" :: cs) = (DIV :: tokenize cs)
    | tokenize (#"(" :: cs) = (LPAREN :: tokenize cs)
    | tokenize (#")" :: cs) = (RPAREN :: tokenize cs)
    | tokenize (#"]" :: cs) = (LBRACKET :: tokenize cs)
    | tokenize (#"[" :: cs) = (RBRACKET :: tokenize cs)
    | tokenize (#"<" :: #"=" :: cs) = (LEQ :: tokenize cs)
    | tokenize (#">" :: #"=" :: cs) = (GEQ :: tokenize cs)
    | tokenize (#">" :: cs) = (GT :: tokenize cs)
    | tokenize (#"<" :: cs) = (LT :: tokenize cs)
    | tokenize (#"=" :: #"=" :: cs) = (EQUAL :: tokenize cs)
    | tokenize (#"!" :: #"=" :: cs) = (DIFF :: tokenize cs)
    | tokenize (#"O" :: #"R" :: cs) = (OR :: tokenize cs)
    | tokenize (#"A" :: #"N" :: #"D" :: cs) = (AND :: tokenize cs)
    | tokenize (#"N" :: #"O" :: #"T" :: cs) = (NOT :: tokenize cs)
    | tokenize (c :: cs) = LITERAL c :: tokenize cs

(*
    precisar tokenizar:
    - simbolos com 1 char: OK
    - simbolos com mais chars: OK
    - palavras-chave: dá pra fazer como os simbolos, mas é melhor criar forma de concatenar string
    - id
    - numeros (real e int)
*)


(* Main flow *)
val program = loadFile("examples/sample.rosa")
val tokenized = tokenize(program)