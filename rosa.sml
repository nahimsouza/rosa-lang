(* Rosa programming language interpreter - Main*)

(* Based on ML-yacc sample calculator - https://www.smlnj.org/doc/ML-Yacc/mlyacc007.html *)

(* This file provides glue code for building the interpreter using the
 * parser and lexer specified in rosa.lex and rosa.grm.
*)

structure Rosa : sig
	           val parse : unit -> unit
                 end =
struct

(*
 * We apply the functors generated from rosa.lex and rosa.grm to produce
 * the RosaParser structure.
 *)

  structure RosaLrVals =
    RosaLrValsFun(structure Token = LrParser.Token)

  structure RosaLex =
    RosaLexFun(structure Tokens = RosaLrVals.Tokens)

  structure RosaParser =
    Join(structure LrParser = LrParser
	 structure ParserData = RosaLrVals.ParserData
	 structure Lex = RosaLex)

datatype RosaType =
  Integer of int
  | Real of real

(*
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdOut,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in RosaParser.parse(0,lexstream,print_error,())
      end

(*
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the calculator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse () =
    let
        val lexer = RosaParser.makeLexer (fn _ =>
                                            (case TextIO.inputLine TextIO.stdIn
                                               of SOME s => s
                                               | _ => ""))
        val dummyEOF = RosaLrVals.Tokens.EOF(0,0)
        val dummySEMI = RosaLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
            let
              val (result,lexer) = invoke lexer
              val (nextToken,lexer) = RosaParser.Stream.get lexer
              val _ = case result
                          of SOME r => TextIO.output(TextIO.stdOut, "output = " ^ r ^ " \n")
                            | NONE   => ()
            in
              if RosaParser.sameToken(nextToken,dummyEOF) then () else loop lexer
            end
    in
      loop lexer
    end

end (* structure Rosa *)
