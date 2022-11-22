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
%let FASTQ = (["@"] {String}) {Sequence} (["+"] {Sequence})? {Quality};
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
