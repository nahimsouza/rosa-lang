(* Rosa programming language interpreter - Lexer *)

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

(* RosaType represent the types used in ROSA lang *)
type Sequence = char * string (* char represents the Type (d, r or p) *)
type Quality = string
datatype RosaType =
  Integer of int
  | Real of real
  | Boolean of bool
  | String of string
  | Sequence of char * string
  | Quality of string
  | FASTA of string * Sequence
  | FASTQ of string * Quality * Sequence

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%

%header (functor RosaLexFun(structure Tokens: Rosa_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
optsign=("+"|"-")?;
ws = [\ \t];
boolean = "True" | "False";

symbol = ("!" | "\"" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," |
         "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "[" |  "\\" | "|" |
         "]" | "^" | "_" | "`" | "{" | "}" | "~");

special = ("á" | "â" | "à" | "ã" | "é" | "ê" | "è" | "í" | "î" | "ì" |
          "ó" | "ô" | "ò" | "õ" | "ú" | "û" | "ù" | "ç" | "ñ" | "ý" | "Á" | "Â" | "À" | "Ã" | "É" | "Ê" |
          "È" | "Í" | "Î" | "Ì" | "Ó" | "Ô" | "Ò" | "Õ" | "Ú" | "Û" | "Ù" | "Ç" | "Ñ" | "Ý");

char = ({alpha} | {digit} | {symbol} | {special});

string = "\""{char}*"\"";

dna = "A" | "C" | "G" | "T" | "R" | "Y" | "S" | "W" | "K" | "M" | "B" | "D" | "H" | "V" | "N" | "." | "-";
protein = "A" | "R" | "N" | "D" | "C" | "Q" | "E" | "G" | "H" | "I" | "M" | "L" | "K" | "F" | "P" | "S" | "T" | "W" | "Y" | "v" | "*";
rna = "A" | "C" | "G" | "U" | "R" | "Y" | "S" | "W" | "K" | "M" | "B" | "D" | "H" | "V" | "N" | "." | "-";

sequence = "d"{dna}* | "p"{protein}* | "r"{rna}*;

quality = {symbol}* | {alpha}*;

fasta = ">"{string}{sequence}

fastq = "@"{string}{sequence}"+"[{sequence}]{quality};

%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{optsign}{digit}+ => (Tokens.INT ( valOf (Int.fromString yytext), !pos, !pos));
{optsign}{digit}+\.{digit}+ => (Tokens.REAL ( valOf (Real.fromString yytext), !pos, !pos));
{boolean} => (Tokens.BOOLEAN ( valOf (Bool.fromString (String.map Char.toLower yytext)), !pos, !pos));

"="      => (Tokens.EQUAL(!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
{alpha}+ => (if yytext="print"
                 then Tokens.PRINT(!pos,!pos)
                 else Tokens.ID(yytext,!pos,!pos)
            );
"-"      => (Tokens.SUB(!pos,!pos));
"^"      => (Tokens.CARAT(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


