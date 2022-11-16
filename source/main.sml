
(* Read content from a file into a list of strings *)
fun readFile(filename) =
    let val file = TextIO.openIn filename
        val content = TextIO.inputAll file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") content
    end


val program = readFile "examples/sample.rosa"

