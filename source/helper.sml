structure Helper = struct
exception SizeNotAllowed
exception TypeError


open Grammar;

val isComma = Char.contains ","
val isSemi = Char.contains ";"

fun readFromString(s) =
  case (Real.fromString s) of
      SOME n => Primitivo (Float_ n)
    | NONE => readInt(s)
and readInt(s) =
  case (Int.fromString s) of
      SOME f => Primitivo (Int_ f)
    | NONE => readBool(s)
and readBool(s) =
  case (Bool.fromString s) of
      SOME b => Primitivo (Boolean_ b)
    | NONE => Primitivo (String_ s)

fun toSemi nil = nil
  | toSemi((#")")::(#",")::ss) = (#")")::(#";")::(toSemi ss)
  | toSemi(x::xs) = x :: (toSemi xs)

fun toTupla(x1::x2::nil) = Tupla2 (readFromString x1, readFromString x2)
  | toTupla(x1::x2::x3::nil) = Tupla3 (readFromString x1, readFromString x2, readFromString x3)
  | toTupla(x1::x2::x3::x4::nil) = Tupla4 (readFromString x1, readFromString x2, readFromString x3,readFromString x4)
  | toTupla(x1::x2::x3::x4::x5::nil) = Tupla5 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5)
  | toTupla(x1::x2::x3::x4::x5::x6::nil) = Tupla6 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::nil) = Tupla7 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::x8::nil) = Tupla8 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7, readFromString x8)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::nil) = Tupla9 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7, readFromString x8, readFromString x9)
  | toTupla(x1::x2::x3::x4::x5::x6::x7::x8::x9::x0::nil) = Tupla0 (readFromString x1, readFromString x2, readFromString x3,readFromString x4, readFromString x5, readFromString x6,readFromString x7, readFromString x8, readFromString x9,readFromString x0)
  | toTupla _ = raise SizeNotAllowed

fun removeHashTag(ls: string) = String.implode(List.filter (fn(l)=> not (l = #"#")) (String.explode(ls)))

fun deleteOpenParenthesis (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #"(" = xs then String.implode ys
          else Char.toString(xs) ^ deleteOpenParenthesis(ys)

fun deleteCloseParenthesis (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #")" = xs then String.implode ys
          else Char.toString(xs) ^ deleteCloseParenthesis(ys)

fun deleteOpenBracket (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #"{" = xs then String.implode ys
          else Char.toString(xs) ^ deleteOpenBracket(ys)

fun deleteCloseBracket (list: char list) =
    case list of
      nil=> ""
      | xs::ys => if #"}" = xs then String.implode ys
          else Char.toString(xs) ^ deleteCloseBracket(ys)

fun length nil = 0
    | length (x::xs) = 1 + length xs

fun removeParenthesis (tupleString: string) = removeHashTag(deleteOpenParenthesis(String.explode(deleteCloseParenthesis(String.explode tupleString))));
fun removeBrackets (sampleString: string) = deleteOpenBracket(String.explode(deleteCloseBracket(String.explode sampleString)));

fun getCleanList(tupleString) =
  let
    val cleanString = removeParenthesis(tupleString)
  in
    String.tokens isComma cleanString
  end


fun getListFrom(sampleString: string) =
  let
    val cleanString = removeBrackets(sampleString)
  in
    (String.tokens isComma cleanString)
  end

fun getTupleFrom(sampleString: string) =
  let
    val cleanString = removeParenthesis(sampleString)
  in
    (String.tokens isComma cleanString)
  end

end
