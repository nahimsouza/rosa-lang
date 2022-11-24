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
      fun iter([]) = [] 
      | iter(x::xs) = 
      if x = #"A" then [#"U"] @ iter(xs)
      else if x = #"T" then [#"A"] @ iter(xs)
      else if x = #"C" then [#"G"] @ iter(xs)
      else [#"C"] @ iter(xs)
      val result = iter(monomers)
    in
      implode(result)
    end;

fun translate(sequence) =
let
  val i = 0
  val trans = false
  val codon = ""
  fun iter(seq, size(seq) - 1, trans) = []
  | iter(seq, i, false) = 
  codon = substring(sequence, i, 3)
  if codon = "AUG" orelse codon = "ATG"
  then [#"M"] @ iter(seq, i + 3, true)
  else iter(seq, i + 1, false)
  | iter(seq, i, true) =
  codon = substring(seq, i, 3)
  if codon = "GCT" orelse codon = "GCC" orelse codon = "GCA" orelse codon = "GCG" orelse "GCU"
  then [#"A"] @ iter(seq, i + 3, true)
  else if codon = "CGT" orelse codon = "CGU" orelse codon = "CGC" orelse codon = "CGA" orelse codon = "CGG" orelse codon = "AGA" orelse codon = "AGG"
  then [#"R"] @ iter(seq, i + 3, true)
  else if codon = "AAT" orelse codon = "AAU" orelse codon = "AAC"
  then [#"N"] @ iter(seq, i + 3, true)
  else if codon = "GAT" orelse codon = "GAU" orelse codon = "GAC"
  then [#"D"] @ iter(seq, i + 3, true)
  else if codon = "TGT" orelse codon = "UGU" orelse codon = "TGC" orelse codon = "UGC"
  then [#"C"] @ iter(seq, i + 3, true)
  else if codon = "CAA" orelse codon = "CAG"
  then [#"Q"] @ iter(seq, i + 3, true)
  else if codon = "GAA" orelse codon = "GAG"
  then [#"E"] @ iter(seq, i + 3, true)
  else if codon = "GGT" orelse codon = "GGU" orelse codon = "GGC" orelse codon = "GGA" orelse codon = "GGG"
  then [#"G"] iter(seq, i + 3, true)
  else if codon = "CAT" orelse codon = "CAU" orelse codon = "CAC"
  then [#"H"] @ iter(seq, i + 3, true)
  else if codon = "ATT" orelse codon = "AUU" orelse codon = "ATC" orelse codon = "AUC" orelse codon = "ATA"
  then [#"I"] @ iter(seq, i + 3, true)
  else if codon = "TTA" orelse codon = "UUA" orelse codon = "TTG" orelse codon = "UUG" orelse codon = "CTT" orelse codon = "CUU" orelse codon = "CTC" orelse 
  codon = "CUC" orelse codon = "CTA" orelse codon = "CUA" orelse codon = "CTG" orelse codon = "CUG"
  then [#"L"] @ iter(seq, i + 3, true)
  else if codon = "AAA" orelse codon = "AAG"
  then [#"K"] @ iter(seq, i + 3, true)
  else if codon = "TTT" orelse codon = "UUU" orelse codon = "TTC" orelse codon = "UUC"
  then [#"F"] @ iter(seq, i + 3, true)
  else if codon = "CCT" orelse codon = "CCU" orelse codon = "CCC" orelse codon = "CCA" orelse codon = "CCG"
  then [#"P"] @ iter(seq, i + 3, true)
  else if codon = "TCT" orelse codon = "UCU" orelse codon = "TCC" orelse codon = "UCC" orelse codon = "TCA" orelse codon = "UCA" orelse codon = "TCG" orelse 
  codon = "UCG" orelse codon = "AGT" orelse codon = "AGU" orelse codon = "AGC"
  then [#"S"] @ iter(seq, i + 3, true)
  else if codon = "ACT" orelse codon = "ACU" orelse codon = "ACC" orelse codon = "ACA" orelse codon = "ACG"
  then [#"T"] @ iter(seq, i + 3, true)
  else if codon = "TGG" orelse codon = "UGG"
  then [#"W"] @ iter(seq, i + 3, true)
  else if codon = "TAT" orelse codon = "UAU" orelse codon = "TAC" orelse codon = "UAC"
  then [#"Y"] @ iter(seq, i + 3, true)
  else if codon = "GTT" orelse codon = "GUU" orelse codon = "GTC" orelse codon = "GUC" orelse codon = "GTA" orelse codon = "GUA" orelse codon = "GTG" orelse codon = "GUG"
  then [#"V"] @ iter(seq, i + 3, true)
  else if codon = "TAA" orelse codon = "UAA" orelse codon = "TGA" orelse codon = "UGA" orelse codon = "TAG" orelse codon = "UAG"
  then [] @ iter(seq, i + 3, false)

  result = iter(sequence)

in
  implode(result)
end;

(* rever essas duas funções abaixo mais adiante para serem feitas apenas pela chamada de complement() em função de ser dna ou rna!!!! *)
fun complement_dna(sequence) = 
    let
      val monomers = explode(sequence)
      fun iter([]) = [] 
      | iter(x::xs) = 
      if x = #"A" then [#"T"] @ iter(xs)
      else if x = #"T" then [#"A"] @ iter(xs)
      else if x = #"C" then [#"G"] @ iter(xs)
      else [#"C"] @ iter(xs)
      val result = iter(monomers)
    in
      implode(result)
    end;

fun complement_rna(sequence) = 
    let
      val monomers = explode(sequence)
      fun iter([]) = [] 
      | iter(x::xs) = 
      if x = #"A" then [#"U"] @ iter(xs)
      else if x = #"U" then [#"A"] @ iter(xs)
      else if x = #"C" then [#"G"] @ iter(xs)
      else [#"C"] @ iter(xs)
      val result = iter(monomers)
    in
      implode(result)
    end;
    
fun motif(sequence : string, motif : string) = 
let
  val i = 0
  val sequence = sequence
  val motif = motif
  fun iter(sequence, motif, i) = 
  if i = size(sequence) - size(motif) then ""
  else if substring(sequence, i, size(motif)) = motif
  then concat [(Int.toString(i)), " ", (iter(sequence, motif, i + 1))]
  else iter(sequence, motif, i + 1)
in
  iter(sequence, motif, 0)
end;

fun qctrl(fastq, threshol, range) = ;

fun trim(file, adapters, primers) = ;

fun tofasta(fastq) = ;

fun readfa(path) = ;

fun readfq(path) = ;

fun writefa(file, name, path) = ; 

fun writefq(file, name, path) = ;

fun input(prompt) = ;
