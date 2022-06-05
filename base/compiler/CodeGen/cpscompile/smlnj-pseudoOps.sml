(* smlnj-pseudoOps.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * pseudo ops for the sml/nj compiler
 *)

functor SMLNJPseudoOps ( structure Asm : PSEUDO_OPS_BASIS ) : SMLNJ_PSEUDO_OPS =
  struct
    structure AsmPseudoOps = Asm
    structure W = Word
    structure PB = PseudoOpsBasisTyp
    structure T = Asm.T

    datatype smlnj_pseudo_op
      = JUMPTABLE of {base:Label.label,targets:Label.label list}
      | FILENAME of string

    type pseudo_op = smlnj_pseudo_op

    val wordBitSz = Asm.wordSize
    val alignSz = (case wordBitSz of 32 => 2 | 64 => 3)
    val wordShift = Word.fromInt alignSz

(* 64BIT: even on 64-bit targets, we should use 32-bit entries in the jump table *)
    fun toBasis (JUMPTABLE{base, targets}) = let
	  fun targetOffset t = T.SUB(wordBitSz, T.LABEL t, T.LABEL base)
	  fun pseudoOpOff lab = PB.INT{sz=wordBitSz, i=[T.LABEXP(targetOffset lab)]}
	  in
	    PB.ALIGN_SZ alignSz ::
	      PB.DATA_LABEL base ::
		List.foldr (fn (target, acc) => pseudoOpOff target :: acc) [] targets
	  end
      | toBasis (FILENAME file) = let
	(* the source file name, which is attached to the end of the generated code, is
	 * followed by a length byte.  The length is specified in words (4 or 8 bytes)
	 * and includes the nul terminator and length byte.  This code should be
	 * consistent with the function BO_GetCodeObjTag in runtime/gc/big-objects.c.
	 *)
	  fun INT8 n = PB.INT{sz=8, i=[T.LI(T.I.fromInt(8, n))]}
	(* length, accounting for zero termination and length byte *)
	  val len = Word.fromInt(String.size file) + 0w2
	(* round up to a multiple of the word size *)
	  val lenW = let val wsm1 = Word.<<(0w1, wordShift)-0w1
		in
		  Word.andb(len + wsm1, Word.notb wsm1)
		end
	  fun pad 0w0 = [INT8(Word.toInt(Word.>>(lenW, wordShift)))]
	    | pad n = INT8 0 :: pad(n - 0w1)
	  in
	     PB.ALIGN_SZ alignSz :: PB.ASCIIZ(file) :: pad (lenW-len)
	  end

    fun toString pOp = String.concat(
	  List.foldr
	    (fn (p, acc) => AsmPseudoOps.toString p :: "\n" :: acc)
	      [] (toBasis pOp))

    fun emitValue {pOp, loc, emit} = let
	  val pb = toBasis pOp
	  fun output(p, loc) = (
		AsmPseudoOps.emitValue{pOp=p, loc=loc, emit=emit};
	        loc + AsmPseudoOps.sizeOf(p, loc))
	  in
	    List.foldl output loc (toBasis pOp); ()
	  end

    fun sizeOf (pOp,loc) = List.foldl (fn (p, a) => a + AsmPseudoOps.sizeOf(p, loc)) 0 (toBasis pOp)

    fun adjustLabels (JUMPTABLE{base, ...}, loc) = let
	  val baseAddr = loc + AsmPseudoOps.sizeOf(PB.ALIGN_SZ alignSz, loc)
	  in
	    if Label.addrOf(base) = baseAddr
	     then false
	     else (Label.setAddr(base, baseAddr); true)
	  end
      | adjustLabels (FILENAME _, _) = false

  end (* SMLNJPseudoOps *)
