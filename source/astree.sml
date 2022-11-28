
structure ParseTree = struct

exception OperationNotSupported

open Grammar
open Helper

datatype UnOp = Mean | StdDev | Median | SumL | ProdL | ToString | ToInt | ToFloat | Variance
datatype BinOp = Add | Sub | Div | Mul | And | Or | Pow | RT | Cov | Corr | Concat | LinReg | GetFloat | GetInt | GetString
datatype OpRel = GTR | LTR | EQR | NEQR | GEQR | LEQR

datatype Expr = Const of tipo
              | FuncOne of UnOp * Expr
              | FuncTwo of BinOp * Expr * Expr
              | Rel of OpRel * Expr * Expr
              | Var of string


datatype Tree = Assign of string * Expr
              | Print of Expr
              | If of Expr * (Tree list) * (Tree list)
              | While of Expr * (Tree list)
              | Null

type RoseTree = Tree list

fun getBinaryFun("+", e1, e2) = FuncTwo(Add, e1, e2)
  | getBinaryFun("-", e1, e2) = FuncTwo(Sub, e1, e2)
  | getBinaryFun("/", e1, e2) = FuncTwo(Div, e1, e2)
  | getBinaryFun("*", e1, e2) = FuncTwo(Mul, e1, e2)
  | getBinaryFun("&&", e1, e2) = FuncTwo(And, e1, e2)
  | getBinaryFun("||", e1, e2) = FuncTwo(Or, e1, e2)
  | getBinaryFun("pow", e1, e2) = FuncTwo(Pow, e1, e2)
  | getBinaryFun("rt", e1, e2) = FuncTwo(RT, e1, e2)
  | getBinaryFun("covariance", e1, e2) = FuncTwo(Cov, e1, e2)
  | getBinaryFun("correlation", e1, e2) = FuncTwo(Corr, e1, e2)
  | getBinaryFun("++", e1, e2) = FuncTwo(Concat, e1, e2)
  | getBinaryFun("linearRegression", e1, e2) = FuncTwo(LinReg, e1, e2)
  | getBinaryFun("getFloat", e1, e2) = FuncTwo(GetFloat, e1, e2)
  | getBinaryFun("getString", e1, e2) = FuncTwo(GetString, e1, e2)
  | getBinaryFun("getInt", e1, e2) = FuncTwo(GetInt, e1, e2)
  | getBinaryFun(_,_,_) = raise OperationNotSupported
  handle e => (print "Exception: ";exnName e; e)

fun floatListToSampleExpr(fl) = Const(Sample (List.map (fn(x) => Primitivo(Float_ x)) fl))

fun intListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(Int_ x)) il))

fun stringListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(String_ x)) il))

fun boolListToSampleExpr(il) = Const(Sample (List.map (fn(x) => Primitivo(Boolean_ x)) il))

fun showBinOp(Add) = "+"
  | showBinOp(Sub) = "-"
  | showBinOp(Div) = "/"
  | showBinOp(Mul) = "*"
  | showBinOp(And) = "&&"
  | showBinOp(Or) = "||"
  | showBinOp(Pow) = "pow"
  | showBinOp(RT) = "rt"
  | showBinOp(Cov) = "covariance"
  | showBinOp(Corr) = "correlation"
  | showBinOp(Concat) = "++"
  | showBinOp(LinReg) = "linearRegression"
  | showBinOp(GetFloat) = "getFloat"
  | showBinOp(GetInt) = "getInt"
  | showBinOp(GetString) = "getString"


fun getFunctionOne("mean", e1) = FuncOne(Mean,e1)
  | getFunctionOne("stdDeviation", e1) = FuncOne(StdDev,e1)
  | getFunctionOne("variance", e1) = FuncOne(Variance,e1)
  | getFunctionOne("median", e1) = FuncOne(Median,e1)
  | getFunctionOne("sum", e1) = FuncOne(SumL,e1)
  | getFunctionOne("prod", e1) = FuncOne(ProdL,e1)
  | getFunctionOne("toString", e1) = FuncOne(ToString,e1)
  | getFunctionOne("toInt", e1) = FuncOne(ToInt,e1)
  | getFunctionOne("toFloat", e1) = FuncOne(ToFloat,e1)
  | getFunctionOne(_, _) = raise OperationNotSupported
  handle e => (print "Exception: ";exnName e; e)

fun getExprBoolTree("==",e1,e2) = Rel(EQR,e1,e2)
    | getExprBoolTree("!=",e1,e2) = Rel(NEQR,e1,e2)
    | getExprBoolTree("<=",e1,e2) = Rel(LEQR,e1,e2)
    | getExprBoolTree(">=",e1,e2) = Rel(GEQR,e1,e2)
    | getExprBoolTree("<",e1,e2) = Rel(LTR,e1,e2)
    | getExprBoolTree(">",e1,e2) = Rel(GTR,e1,e2)
    | getExprBoolTree(_,_,_) = raise OperationNotSupported

fun insere(hm,n,"int") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Int_ 0))
    | insere(hm,n,"string") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.String_ ""))
    | insere(hm,n,"float") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Float_ 0.0))
    | insere(hm,n,"boolean") = AtomMap.insert(hm, n, Grammar.Primitivo(Grammar.Boolean_ false))
    | insere(hm,n,_) = AtomMap.insert(hm, n, Grammar.Void)

fun showOpRel(NEQR) = "!="
  | showOpRel(EQR) = "=="
  | showOpRel(GEQR) = ">="
  | showOpRel(LEQR) = "<="
  | showOpRel(GTR) = ">"
  | showOpRel(LTR) = "<"

fun showFunctionOne(Mean) = "mean"
  | showFunctionOne(StdDev) = "stdDeviation"
  | showFunctionOne(Median) = "median"
  | showFunctionOne(SumL) = "sum"
  | showFunctionOne(ProdL) = "prod"
  | showFunctionOne(ToInt) = "toInt"
  | showFunctionOne(ToString) = "toString"
  | showFunctionOne(ToFloat) = "toFloat"
  | showFunctionOne(Variance) = "variance"

fun eval(Const t,vars) = t
  | eval(Var s,vars) =
    let
        val v = valOf(AtomMap.find(vars,Atom.atom s))
    in
        v
    end
  | eval(Rel(oprel,e1,e2),vars) =
        let
            val ee1 = eval(e1,vars)
            val ee2 = eval(e2,vars)
        in
            case (TypeChecker.typeof ee1,TypeChecker.typeof ee2) of
                  ("int","int") => TypeChecker.oper(showOpRel oprel,ee1,ee2)
                | ("float","float") => TypeChecker.oper(showOpRel oprel,ee1,ee2)
                | (_,_) => raise TypeChecker.TypeMismatch
        end
  | eval(FuncOne(func, e1), vars) =
        let
            val ee1 = eval(e1, vars)
            (* val _ = print("Type 1: " ^ (TypeChecker.typeof ee1) ^ " fun " ^ showFunctionOne func ^ "\n") *)
        in
            case (TypeChecker.typeof ee1) of
                ("sample of float") => TypeChecker.functionOne("sample of float", showFunctionOne func, ee1)
              | ("sample of int") => TypeChecker.functionOne("sample of int", showFunctionOne func, ee1)
              | ("float") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | ("int") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | ("string") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | ("boolean") => TypeChecker.functionOne("", showFunctionOne func, ee1)
              | (_) => TypeChecker.functionOne("", showFunctionOne func, ee1)
        end
  | eval(FuncTwo(binop, e1, e2), vars) =
        let
            val ee1 = eval(e1, vars)
            val ee2 = eval(e2, vars)
            (* val _ = print("Type 2: " ^ (TypeChecker.typeof ee1)^ " " ^ (TypeChecker.typeof ee2) ^ " " ^ (showBinOp binop) ^"\n") *)
        in
            case (TypeChecker.typeof ee1, TypeChecker.typeof ee2) of
                ("sample of float", "sample of float") => TypeChecker.statistics("sample of float", showBinOp binop, ee1, ee2)
              | ("sample of int", "sample of int") => TypeChecker.statistics("sample of int", showBinOp binop, ee1, ee2)
              | ("sample of float", "int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("sample of int", "int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("string","sample of string") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("sample of string","int") => TypeChecker.functionTwo(showBinOp binop,ee1,ee2)
              | ("float", "float") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("int", "int") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("string", "string") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | ("boolean", "boolean") => TypeChecker.oper(showBinOp binop, ee1, ee2)
              | (_, _) => raise TypeChecker.TypeMismatch
        end


fun interpret((Print expr),vars,tps) =
        let
            val evaluedExpr = eval(expr,vars)
        in
            print(Grammar.show evaluedExpr ^ "\n");
            vars
        end
  | interpret(Assign(var,e),vars,tps) =
        let
            val evaluedExpr = eval(e,vars)
            val varExists = AtomMap.inDomain(vars,Atom.atom var)
            val t = valOf(AtomMap.find (tps,Atom.atom var))
            val sameType = (TypeChecker.isType evaluedExpr t)
        in
            if (varExists andalso sameType) then
                Grammar.updateHt(vars,Atom.atom var,evaluedExpr)
            else
                raise TypeChecker.TypeError
        end
  | interpret(If(e,c1,c2),vars,tps) =
        let
            val evaluedExpr = TypeChecker.extractBool(eval(e,vars))
        in
            if evaluedExpr then
                programa(c1, vars, tps)
            else
                programa(c2, vars, tps);
            vars
        end
  | interpret(While(e,c1),vars,tps) =
        let
            val evaluedExpr = TypeChecker.extractBool(eval(e,vars))
        in
            if evaluedExpr then
              let val newVars = programa(c1, vars, tps) in interpret(While(e,c1), newVars, tps) end
            else
              vars
        end
  | interpret(cs,vars,tps) = vars
  (* handle e => (print ("Exception: " ^ exnName e); ()) *)

and programa(x::ls, vars, tps) = let val varsNew = interpret(x, vars, tps) in programa(ls, varsNew, tps) end
  | programa([], vars, tps) = vars


end
