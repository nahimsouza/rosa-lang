exception NothingFound

structure RosaTest =
struct
	open Grammar
	open ParseTree
	structure CP = RosaParseFn(RosaLexer)

	fun rosa instrm =
		let
			val sm = AntlrStreamPos.mkSourcemap()
			val lex = RosaLexer.lex sm
			val strm = RosaLexer.streamifyInstream instrm
			val _ = print "    Interpreting ROSA code...     \n"
			val (r, strm', errs, {tree=tr,vars=vs,ts=tps}) = CP.parse lex strm
			fun doErr err = print ("Syntax error " ^
			    AntlrRepair.repairToString RosaTokens.toString sm err ^ "\n")
			val _ = app doErr errs
		in
			(case r of SOME r2 => ParseTree.programa(r2,vs,tps) | NONE => raise NothingFound);
			r
		end

	fun main (prog_name) =
    	let
      		val _ = rosa (TextIO.openIn prog_name)
    	in
      		1
    	end

end
