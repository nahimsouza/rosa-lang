(* Rosa programming language interpreter - Lexer *)

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

fun isWordSeparator c = c = #"\n"

fun splitText(text) = String.tokens isWordSeparator text

fun splitFasta fastaString =
  let
    val trimmed = String.substring(fastaString, 3, (String.size fastaString) - 6);
    val splited = splitText trimmed
  in
    (List.nth(splited,0), List.nth(splited,1))
  end

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

identifier = {alpha}("_" | {alpha} | {digit})*;

rsequence = "@r"{rna}*;
dsequence = "@d"{dna}*;
psequence = "@p"{protein}*;

sequence = {rsequence} | {dsequence} | {psequence};

quality = ({symbol}|{alpha})*;
quality_tok = "@q"{quality}"ç";

separator = "§";

header = {char}*;

fasta = ">"{header}{separator}{sequence}{separator};

fastq = "@"{header}{separator}{sequence}"+"[{sequence}]{separator}{quality}{separator};

primitive_type = "Integer" | "Real" | "Boolean" | "String" | "Quality" | "Sequence";

composite_type = "FASTA" | "FASTQ";

recursive_type = "List of "({primitive_type} | {composite_type});

type = ({primitive_type} | {composite_type} | {recursive_type});

op_log = "NOT" | "AND" | "OR";

op_arit = "+" | "-" | "*" | "/";

op_rel = ">" | ">=" | "<" | "<=" | "==" | "!=";

op_bool = {op_log} | {op_rel};


%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.INT ( valOf (Int.fromString yytext), !pos, !pos));
{digit}+\.{digit}+ => (Tokens.REAL ( valOf (Real.fromString yytext), !pos, !pos));
{boolean} => (Tokens.BOOLEAN ( valOf (Bool.fromString (String.map Char.toLower yytext)), !pos, !pos));

{type} => (Tokens.TYPE(yytext, !pos, !pos));

{string} => (Tokens.STRING(yytext, !pos, !pos));

{rsequence} => (Tokens.RSEQUENCE(yytext, !pos, !pos));
{dsequence} => (Tokens.DSEQUENCE(yytext, !pos, !pos));
{psequence} => (Tokens.PSEQUENCE(yytext, !pos, !pos));

{quality_tok} => (Tokens.QUALITY(yytext, !pos, !pos));

{fasta} => (Tokens.FASTA(splitFasta(yytext), !pos, !pos));

"NOT"    => (Tokens.NOT(!pos,!pos));
"AND"    => (Tokens.AND(!pos,!pos));
"OR"    => (Tokens.OR(!pos,!pos));

{identifier} =>
  (
    case yytext of
        "print" => Tokens.PRINT(!pos,!pos)
      | "ins" => Tokens.INS(!pos,!pos)
      | "del" => Tokens.DEL(!pos,!pos)
      | "point" => Tokens.POINT(!pos,!pos)
      | "transcribe" => Tokens.TRANSCRIBE(!pos,!pos)
      | "translate" => Tokens.TRANSLATE(!pos,!pos)
      | "complement" => Tokens.COMPLEMENT(!pos,!pos)
      | "motif" => Tokens.MOTIF(!pos,!pos)
      | "qctrl" => Tokens.QCTRL(!pos,!pos)
      | "trim" => Tokens.TRIM(!pos,!pos)
      | "tofasta" => Tokens.TOFASTA(!pos,!pos)
      | "read" => Tokens.READ(!pos,!pos)
      | "readfa" => Tokens.READFA(!pos,!pos)
      | "readfq" => Tokens.READFQ(!pos,!pos)
      | "write" => Tokens.WRITE(!pos,!pos)
      | "writefa" => Tokens.WRITEFA(!pos,!pos)
      | "writefq" => Tokens.WRITEFQ(!pos,!pos)
      | "input" => Tokens.INPUT(!pos,!pos)
      | _ => Tokens.ID(yytext,!pos,!pos)
  );

"=="     => (Tokens.DOUBLEEQUAL(!pos,!pos));
"="      => (Tokens.EQUAL(!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"^"      => (Tokens.CARAT(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());


