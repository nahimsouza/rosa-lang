open String;

fun ins(sequence, fragment, position) = 
    let
        val first = substring(sequence, 0, position - 1)
        val second = substring(sequence, position, size(sequence) - position)
    in
        concat [first, fragment, second]
    end;

fun del(sequence : string, start : int, final : int) = 
    let
        val first = substring(sequence, 0, start - 1)
        val second = substring(sequence, final, size(sequence) - final)
    in
        concat [first, second]
    end;

fun point(sequence : string, position : int, monomer : string) =
    let
        val first = substring(sequence, 0, position - 1)
        val second = substring(sequence, position, size(sequence) - position)
    in
        if size(monomer) = 1
        then concat [first, monomer, second]
        else raise Fail "Monomer length must be equal to 1"
    end;

fun transcribe(sequence) = 
    let
      val monomers = explode(sequence)
    in
      (* fazer iteração, transcrever cada caracter e depois fazer implode*)
    end;

fun translate(sequence) = 
    let
      val monomers = explode(sequence)
    in
      (* fazer iteração, traduzir cada caracter e depois fazer implode*)
    end;

fun complement(sequence : string) = 
    let
      val monomers = explode(sequence)
    in
      (* fazer iteração, complementar cada caracter e depois fazer implode*)
    end;

fun motif(sequence, motif) = ;

fun qctrl(fastq, threshol, range) = ;

fun trim(file, adapters, primers) = ;

fun tofasta(fastq) = ;

fun readfa(path) = ;

fun readfq(path) = ;

fun writefa(file, name, path) = ; 

fun writefq(file, name, path) = ;

fun input(prompt) = ;
