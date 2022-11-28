structure TypeChecker =
struct

open Statistics
open Grammar

exception TypeMismatch
exception FunctionOneNotImplemented
exception FunctionTwoNotImplemented
exception StatisticsNotImplemented
exception TypeError

fun extractList (Sample x) = x
    | extractList _ = raise TypeMismatch

fun extractString (Primitivo(String_ x)) = x
    | extractString _ = raise TypeMismatch

fun extractBool (Primitivo(Boolean_ i)) = i
    | extractBool _ = raise TypeMismatch

fun extractInt (Primitivo(Int_ i)) = i
    | extractInt _ = raise TypeMismatch

fun extractFloat (Primitivo(Float_ i)) = i
    | extractFloat _ = raise TypeMismatch

fun typeof (Primitivo(Float_ _)) = "float"
    | typeof (Primitivo(Int_ _)) = "int"
    | typeof (Primitivo(Boolean_ _)) = "boolean"
    | typeof (Primitivo(String_ _)) = "string"
    | typeof (Sample nil) = "[]"
    | typeof (Sample (x::_)) = "sample of " ^ (typeof x)
    | typeof (Tupla2 (a,b)) = "(" ^ typeof a ^ "," ^ typeof b ^ ")"
    | typeof (Tupla3 (a,b,c)) = "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ ")"
    | typeof (Tupla4 (a,b,c,d)) =
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ ")"
    | typeof (Tupla5 (a,b,c,d,e)) =
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^")"
    | typeof (Tupla6 (a,b,c,d,e,f)) =
        "(" ^ typeof a ^ "," ^ typeof b ^ ","
            ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^"," ^ typeof f ^")"
    | typeof (Tupla7 (a,b,c,d,e,f,g)) =
        "(" ^ typeof a ^ "," ^ typeof b ^ ","
            ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^"," ^ typeof f ^"," ^ typeof g ^")"
    | typeof (Tupla8 (a,b,c,d,e,f,g,h)) =
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^","
            ^ typeof f ^"," ^ typeof g ^"," ^ typeof h ^")"
    | typeof (Tupla9 (a,b,c,d,e,f,g,h,i)) =
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c
            ^ "," ^ typeof d ^ "," ^ typeof e ^","
            ^ typeof f ^"," ^ typeof g ^","
            ^ typeof h ^"," ^ typeof i ^")"
    | typeof (Tupla0 (a,b,c,d,e,f,g,h,i,j)) =
        "(" ^ typeof a ^ "," ^ typeof b ^ "," ^ typeof c ^ "," ^ typeof d ^ "," ^ typeof e ^ ","
            ^ typeof f ^ "," ^ typeof g ^"," ^ typeof h ^","
            ^ typeof i ^"," ^ typeof j ^")"
    | typeof _ = "null"

fun != (x: real, y:real):bool = Real.!=(x,y)

infix !=

fun oper("+", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Int_ (i+j))
   | oper("+", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i+j))
   | oper("-", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Int_ (i-j))
   | oper("-", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i-j))
   | oper("*", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Int_ (i*j))
   | oper("*", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i*j))
   | oper("/", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (i/j))
   | oper(">=", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i>=j))
   | oper("<=", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i<=j))
   | oper(">", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i>j))
   | oper("<", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Boolean_ (i<j))
   | oper("!=", Primitivo(Float_ i),Primitivo(Float_ j)) = if Real.==(i,j) then Primitivo (Boolean_ false) else Primitivo (Boolean_ true)
   | oper("==", Primitivo(Float_ i),Primitivo(Float_ j)) = if Real.==(i,j) then Primitivo (Boolean_ true) else Primitivo (Boolean_ false)
   | oper(">=", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i>=j))
   | oper("<=", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i<=j))
   | oper(">", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i>j))
   | oper("<", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Boolean_ (i<j))
   | oper("!=", Primitivo(Int_ i),Primitivo(Int_ j)) = if i = j then Primitivo (Boolean_ false) else Primitivo (Boolean_ true)
   | oper("==", Primitivo(Int_ i),Primitivo(Int_ j)) = if i = j then Primitivo (Boolean_ true) else Primitivo (Boolean_ false)
   | oper("pow", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (Math.pow(i, j)))
   | oper("rt", Primitivo(Float_ i),Primitivo(Float_ j)) = Primitivo (Float_ (Math.pow(i, 1.0/j)))
   | oper("pow", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Float_ (Math.pow(Real.fromInt i, Real.fromInt j)))
   | oper("rt", Primitivo(Int_ i),Primitivo(Int_ j)) = Primitivo (Float_ (Math.pow(Real.fromInt i, 1.0/(Real.fromInt j))))
   | oper("neg",_,Primitivo(Int_ i)) = Primitivo(Int_ (0-i))
   | oper("neg",_,Primitivo(Float_ i)) = Primitivo(Float_ (0.0-i))
   | oper("&&", Primitivo(Boolean_ i),Primitivo(Boolean_ j)) = Primitivo(Boolean_ (i andalso j))
   | oper("||", Primitivo(Boolean_ i),Primitivo(Boolean_ j)) = Primitivo(Boolean_ (i orelse j))
   | oper("++",Primitivo(String_ l),Primitivo(String_ m)) = Primitivo(String_ ("\""^ String.implode((List.filter (fn(x) => not(x = #"\"")) (String.explode(l ^ m)))) ^ "\""))
   | oper(_,_,_) = raise FunctionTwoNotImplemented

fun exprTypes e1 e2 = (typeof e1) = (typeof e2)
fun isType e1 t = (typeof e1) = t

fun intToFloat (Primitivo(Int_ i)) = Real.fromInt i
   | intToFloat (_) = raise TypeMismatch

fun statistics("sample of float", "correlation", Sample(x), Sample(y)) = Primitivo(Float_ (Statistics.correlation((List.map extractFloat x), (List.map extractFloat y))))
  | statistics("sample of float", "covariance", Sample(x), Sample(y)) = Primitivo(Float_ (Statistics.covariance((List.map extractFloat x), (List.map extractFloat y))))
  | statistics("sample of float", "linearRegression", Sample(x), Sample(y)) = Primitivo(String_ (Statistics.linearRegression((List.map extractFloat x), (List.map extractFloat y))))
  | statistics("sample of int", "correlation", Sample(x), Sample(y)) = Primitivo(Float_ (Statistics.correlation((List.map intToFloat x), (List.map intToFloat y))))
  | statistics("sample of int", "covariance", Sample(x), Sample(y)) = Primitivo(Float_ (Statistics.covariance((List.map intToFloat x), (List.map intToFloat y))))
  | statistics("sample of int", "linearRegression", Sample(x), Sample(y)) = Primitivo(String_ (Statistics.linearRegression((List.map intToFloat x), (List.map intToFloat y))))
  | statistics(_, _, _, _) = raise StatisticsNotImplemented
  handle e => (print ("Exception: " ^ exnName e); e)

fun printValuesFList(Primitivo (Int_ i)) = print("Valor: " ^ (Int.toString i) ^ "\n")

fun functionTwo("getFloat", Sample ls, Primitivo(Int_ i)) = List.nth(ls,i)
  | functionTwo("getInt", Sample ls, Primitivo(Int_ i)) = List.nth(ls,i)
  | functionTwo("getString", Sample ls, Primitivo(Int_ i)) = List.nth(ls,i)
  | functionTwo(_,_,_) = raise FunctionTwoNotImplemented

fun functionOne("sample of float", "mean", Sample(x)) = Primitivo(Float_ (Statistics.mean (List.map extractFloat x)))
  | functionOne("sample of float", "stdDeviation", Sample(x)) = Primitivo(Float_ (Statistics.standardDeviation (List.map extractFloat x)))
  | functionOne("sample of float", "variance", Sample(x)) = Primitivo(Float_ (Statistics.variance (List.map extractFloat x)))
  | functionOne("sample of float", "median", Sample(x)) = Primitivo(Float_ (Statistics.median (List.map extractFloat x)))
  | functionOne("sample of int", "mean", Sample(x)) = Primitivo(Float_ (Statistics.mean (List.map intToFloat x)))
  | functionOne("sample of int", "stdDeviation", Sample(x)) = Primitivo(Float_ (Statistics.standardDeviation (List.map intToFloat x)))
  | functionOne("sample of int", "variance", Sample(x)) = Primitivo(Float_ (Statistics.variance (List.map intToFloat x)))
  | functionOne("sample of int", "median", Sample(x)) = Primitivo(Float_ (Statistics.median (List.map intToFloat x)))
  | functionOne(_, "toString", x ) = Primitivo (String_ (Grammar.show x))
  | functionOne("", "toFloat", Primitivo(Int_ value)) = Primitivo(Float_ (Real.fromInt value))
  | functionOne("", "toInt", Primitivo(Float_ value)) = Primitivo(Int_ (Real.trunc value))
  | functionOne("", "sum", Sample(x)) = raise FunctionOneNotImplemented
  | functionOne("", "prod", Sample(x)) = raise FunctionOneNotImplemented
  | functionOne(_, _, _) = raise TypeMismatch

end
