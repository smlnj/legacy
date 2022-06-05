(* cps-comp.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Translate FLINT to machine code via CPS.
 *)

functor CPSCompFn (

    structure Gen : MACHINE_GEN
    val collect : (unit -> int) -> CodeObj.code_object

  ) : CODE_GENERATOR = struct

    structure MachSpec = Gen.MachSpec
    structure Convert = Convert(MachSpec)
    structure CPStrans = CPStrans(MachSpec)
    structure CPSopt = CPSopt(MachSpec)
    structure Closure = Closure(MachSpec)
    structure Spill = SpillFn(MachSpec)

    structure Machine = Gen

    val architecture = Gen.MachSpec.architecture
    val abi_variant = Gen.abi_variant

    fun bug s = ErrorMsg.impossible ("CPSComp:" ^ s)
    val say = Control.Print.say

    fun phase x = Stats.doPhase (Stats.makePhase x)

    val convert   = phase "CPS 060 convert" Convert.convert
    val cpstrans  = phase "CPS 065 cpstrans" CPStrans.cpstrans
    val cpsopt    = phase "CPS 070 cpsopt" CPSopt.reduce
    val litsplit  = phase "CPS 075 litsplit" Literals.split
    val newlitsplit = phase "CPS 075 litsplit" NewLiterals.split
    val closure   = phase "CPS 080 closure"  Closure.closeCPS
    val globalfix = phase "CPS 090 globalfix" GlobalFix.globalfix
    val spill     = phase "CPS 100 spill" Spill.spill
    val limit     = phase "CPS 110 limit" Limit.nolimit
    val codegen   = phase "CPS 120 cpsgen" Gen.codegen

  (** pretty printing for the CPS code *)
    fun prC s e = if !Control.CG.printit
	  then (
	    say (concat["\n[After ", s, " ...]\n\n"]);
	    PPCps.printcps0 e;
	    say "\n"; e)
	  else e

    fun compile {source, prog} = let
	(* convert to CPS *)
	  val function = convert prog
	  val _ = prC "convert" function
	  val function = (prC "cpstrans" o cpstrans) function
	(* optimize CPS *)
	  val function = cpsopt (function, NONE, false)
	  val _ = prC "cpsopt-code" function
	(* split out heap-allocated literals; litProg is the bytecode *)
(* TODO: switch to newLiterals for all targets *)
	  val (function, data) = if !Control.CG.newLiterals orelse Target.is64
		then newlitsplit function
		else litsplit function
	  val _ = prC "lit-split" function
	(* convert CPS to closure-passing style *)
	  val function = prC "closure" (closure function)
	(* flatten to 1st-order CPS *)
	  val funcs = globalfix function
	(* spill excess live variables *)
	  val funcs = spill funcs
	(* heap-limit checks *)
	  val (funcs, limit) = limit funcs
	  val getEP = codegen {source = source, funcs = funcs, maxAlloc = #1 o limit}
	  val code = collect getEP
	  in
	    {code=code, data=data}
	  end (* function compile *)

  end (* CPSComp *)
