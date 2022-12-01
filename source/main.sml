
(* Load content from a file into a string *)
fun loadFile(filename) =
    let
        val file = TextIO.openIn filename
        val content = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in
        content
    end

fun isWordSeparator c = Char.isSpace c orelse c = #"\n" orelse c = #"\t"

fun splitText(text) = String.tokens isWordSeparator text

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
    | DQUOTE
    | SEMICOLON
    | LEQ
    | GEQ
    | OR
    | AND
    | NOT
    | TRUE
    | FALSE
    | LITERAL of string
    | INTEGER_TYPE
    | REAL_TYPE
    | BOOLEAN_TYPE
    | STRING_TYPE
    | SEQUENCE_TYPE
    | QUALITY_TYPE
    | FASTA_TYPE
    | FASTQ_TYPE
    (* | INTEGER of int
    | REAL of real *)

fun tokenize nil = nil
    | tokenize ("+" :: strList) = (PLUS :: tokenize strList)
    | tokenize ("-" :: strList) = (MINUS :: tokenize strList)
    | tokenize ("*" :: strList) = (TIMES :: tokenize strList)
    | tokenize ("/" :: strList) = (DIV :: tokenize strList)
    | tokenize ("(" :: strList) = (LPAREN :: tokenize strList)
    | tokenize (")" :: strList) = (RPAREN :: tokenize strList)
    | tokenize ("]" :: strList) = (LBRACKET :: tokenize strList)
    | tokenize ("[" :: strList) = (RBRACKET :: tokenize strList)
    | tokenize ("\"" :: strList) = (DQUOTE :: tokenize strList)
    | tokenize (";" :: strList) = (SEMICOLON :: tokenize strList)
    | tokenize ("<=" :: strList) = (LEQ :: tokenize strList)
    | tokenize (">=" :: strList) = (GEQ :: tokenize strList)
    | tokenize (">" :: strList) = (GT :: tokenize strList)
    | tokenize ("<" :: strList) = (LT :: tokenize strList)
    | tokenize ("==" :: strList) = (EQUAL :: tokenize strList)
    | tokenize ("!=" :: strList) = (DIFF :: tokenize strList)
    | tokenize ("OR" :: strList) = (OR :: tokenize strList)
    | tokenize ("AND" :: strList) = (AND :: tokenize strList)
    | tokenize ("NOT" :: strList) = (NOT :: tokenize strList)
    | tokenize ("True" :: strList) = (TRUE :: tokenize strList)
    | tokenize ("False" :: strList) = (FALSE :: tokenize strList)
    | tokenize ("Integer" :: strList) = (INTEGER_TYPE :: tokenize strList)
    | tokenize ("Real" :: strList) = (REAL_TYPE :: tokenize strList)
    | tokenize ("Boolean" :: strList) = (BOOLEAN_TYPE :: tokenize strList)
    | tokenize ("String" :: strList) = (STRING_TYPE :: tokenize strList)
    | tokenize ("Sequence" :: strList) = (SEQUENCE_TYPE :: tokenize strList)
    | tokenize ("Quality" :: strList) = (QUALITY_TYPE :: tokenize strList)
    | tokenize ("FASTA" :: strList) = (FASTA_TYPE :: tokenize strList)
    | tokenize ("FASTQ" :: strList) = (FASTQ_TYPE :: tokenize strList)
    | tokenize (s :: strList) = LITERAL s :: tokenize strList

(*
    precisa tokenizar:
    - identificadores
    - numeros (real e int)
    - criar identificador e ver como fazer assign
        > os valores precisam ser atribuidos e o tipo verificado de alguma forma
*)


(* Main flow *)
val program = loadFile "examples/sample.rosa"
val splited = splitText program
val tokenized = tokenize splited