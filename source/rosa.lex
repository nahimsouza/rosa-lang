%name RosaLexer;

%let digit = [0-9];
%let Integer = {digit}+;
%let Real = {Integer}["."]{Integer}+;
%let alpha = [a-zA-Z];
%let alphaChars = [a-zA-Z:\" !@#.$/%*záàâãéèêíïóôõöúçñÁÀÂÃÉÈÍÏÓÔÕÖÚÇÑ&0-9]*;
%let Boolean = ("True"|"False");
%let id = {alpha}({alpha} | {digit} | "_")*;
%let String = ["]{alphaChars}["];
%let DNA = (A | C | G | T | R | Y | S | W | K | M | B | D | H | V | N | . | -);
%let RNA = (A | C | G | U | R | Y | S | W | K | M | B | D |H | V | N | . | -);
%let protein = (A | R | N | D | C | Q | E | G | H | I | M | L | K | F | P | S | T | W | Y | v | *);
%let Sequence = (["d"] {DNA}* ) | (["r"] {RNA}* ) | (["p"] {protein}*);
%let Quality = {alphaChars}*;
%let primitivo = ("Integer"|"String"|"Boolean"|"Real"|"Sequence"|"Quality");

%let FASTA = ([">"] {String})  {Sequence};
%let FASTQ = (["@"] {String}) {Sequence} (["+"] {String}?) {Quality};
%let composto = ("FASTA" | "FASTQ");

%let List = ("List of "({primitivo}|{composto}));
%let tipo = ({primitivo} | {composto} | {List});
%let valPrim = ({Integer} | {String} | {Boolean} | {Real} | {Sequence} | {Quality});
%let empty = "[]";
%let intList = ([empty] | "[" (" ")* {Integer} (" ")* (","(" ")* {Integer})* (" ")* "]" );
%let realList = ({empty} | "[" (" ")* {Real} (" ")* ("," (" ")* {Real})* (" ")*"]" );
%let booleanList = ({empty} | "[" (" ")* {Boolean} (" ")* ("," (" ")* {Boolean})* (" ")* "]" );
%let strList = ({empty} | "[" (" ")* {String} (" ")* ("," (" ")* {String})* (" ")* "]" );
%let fastaList = ({empty} | "[" {FASTA} ("," {FASTA})* "]" );
%let fastqList = ({empty} | "[" {FASTQ} ("," {FASTQ})* "]" );

%defs (
    structure T = RosaTokens
    type lex_result = T.token
    fun eof() = T.EOF
);

(* Keywords definition *)

"if" => (T.KW_if);
"elif" => (T.KW_elif);
"else" => (T.KW_else);
"then" => (T.KW_then);
"while" => (T.KW_while);
"True" => (T.KW_True);
"False" => (T.KW_False);
"Integer" => (T.KW_Integer);
"Real" => (T.KW_Real);
"String" => (T.KW_String);
"Boolean" => (T.KW_Boolean);
"FASTA" => (T.KW_FASTA);
"FASTQ" => (T.KW_FASTQ);
"Quality" => (T.KW_Quality);
"Sequence" => (T.KW_Sequence);
"List" => (T.KW_List);
"of" => (T.KW_of);
"ins" => (T.KW_ins);
"del" => (T.KW_del);
"point" => (T.KW_point);
"transcribe" => (T.KW_transcribe);
"translate" => (T.KW_translate);
"complement" => (T.KW_complement);
"motif" => (T.KW_motif);
"qctrl" => (T.KW_qctrl);
"trim" => (T.KW_trim);
"tofasta" => (T.KW_tofasta);
"read" => (T.KW_read);
"readfa" => (T.KW_readfa);
"readfq" => (T.KW_readfq);
"writefa" => (T.KW_writefa);
"writefq" => (T.KW_writefq);
"print" => (T.KW_print);
"input" => (T.KW_input);
"NOT" => (T.KW_NOT);
"AND" => (T.KW_AND);
"OR" => (T.KW_OR);


(* DARWIN - nao modificado

{tipo} => ( T.TIPO yytext );
{booleano} => ( T.BOOL (valOf (Bool.fromString yytext)) );
{id} => ( T.ID yytext );
{str} => (T.STR yytext);
{int} => ( T.NUM (valOf (Int.fromString yytext)) );
{float} => ( T.REAL (valOf (Real.fromString yytext)) );
{intList} => (T.SINT (Grammar.toIntList (Grammar.tokenize yytext)));
{floatList} => (T.SFLOAT (Grammar.toFloatList (Grammar.tokenize yytext)));
{booleanList} => (T.SBOOL (Grammar.toBoolList (Grammar.tokenize yytext)));
{strList} => ( T.SSTRING (Grammar.tokenize yytext));
{tuple} => ( T.TUPLE yytext);
{tupleList} => (T.STUPLE yytext ); *)


"==" => ( T.EEQ );
";" => ( T.SEMI);
"+" => ( T.PLUS );
"-" => ( T.MINUS );
"*" => ( T.TIMES );
"/" => ( T.DIV );
"(" => ( T.LP );
")" => ( T.RP );
"AND" => ( T.AND );
"OR" => ( T.OR );
"NOT" => ( T.NOT );
">=" => ( T.GEQ );
"<=" => ( T.LEQ );
">" => ( T.GT );
"<" => ( T.LT );
"!=" => ( T.NEQ );
"{}" => ( T.EMPTY );
"," => ( T.COMMA );
"=" => ( T.ASSIGN );
" " | \n | \t  => ( continue() );
.		=> (print (concat ["Unexpected character: '", yytext,
                       "'\n"]); continue());
