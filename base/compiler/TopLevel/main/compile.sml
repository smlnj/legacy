(* compile.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor CompileF (

    structure M  : CODE_GENERATOR
    structure CC : CCONFIG
    val cproto_conv : string

  ) : COMPILE0 = struct

    fun mkCompInfo { source, transform } = CompInfo.mkCompInfo {
	    source = source,
	    transform = transform,
	    mkStampGenerator = CC.mkMkStamp
	  }

    type pickle     = CC.pickle		(* pickled format *)
    type hash       = CC.hash		(* environment hash id *)
    type pid        = CC.pid
    type guid       = CC.guid

    (*************************************************************************
     *                             ELABORATION                               *
     *************************************************************************)

    (** several preprocessing phases done after parsing or
     ** after elaborations *)
    (*
    val fixityparse =
	(* Stats.doPhase (Stats.makePhase "Compiler 005 fixityparse") *)
	    FixityParse.fixityparse
    val lazycomp =
	(* Stats.doPhase (Stats.makePhase "Compiler 006 lazycomp") *)
	    LazyComp.lazycomp
     *)
    val pickUnpick =
	  Stats.doPhase (Stats.makePhase "Compiler 036 pickunpick") CC.pickUnpick

    (** take ast, do semantic checks,
     ** and output the new env, absyn and pickles *)
    fun elaborate {ast, statenv=senv, compInfo=cinfo, guid} = let
	  val (absyn, nenv) = ElabTop.elabTop(ast, senv, cinfo)
	  val (absyn, nenv) = if CompInfo.anyErrors cinfo
		then (Absyn.SEQdec nil, StaticEnv.empty)
	        else (absyn, nenv)
	  val { pid, pickle, exportLvars, exportPid, newenv } =
	        pickUnpick { context = senv, env = nenv, guid = guid }
	  in {
	    absyn=absyn, newstatenv=newenv, exportPid=exportPid,
	    exportLvars=exportLvars, staticPid = pid, pickle = pickle
	  } end (* function elaborate *)

    val elaborate =
	  Stats.doPhase(Stats.makePhase "Compiler 030 elaborate") elaborate

    (*************************************************************************
     *                        ABSYN INSTRUMENTATION                          *
     *************************************************************************)

    local
      val specialSyms = [
	      SpecialSymbols.paramId,
	      SpecialSymbols.functorId,
	      SpecialSymbols.hiddenId,
	      SpecialSymbols.tempStrId,
	      SpecialSymbols.tempFctId,
	      SpecialSymbols.fctbodyId,
	      SpecialSymbols.anonfsigId,
	      SpecialSymbols.resultId,
	      SpecialSymbols.returnId,
	      SpecialSymbols.internalVarId
	    ]
      fun isSpecial s = List.exists (fn s' => Symbol.eq (s, s')) specialSyms
    in
    (** instrumenting the abstract syntax to do time- and space-profiling *)
    fun instrument {source, senv, compInfo} =
	  SProf.instrumDec (senv, compInfo) source
	  o TProf.instrumDec PrimopId.isPrimCallcc (senv, compInfo)
	  o TDPInstrument.instrument isSpecial (senv, compInfo)
    end (* local *)

    val instrument =
	  Stats.doPhase (Stats.makePhase "Compiler 039 instrument") instrument

    (*************************************************************************
     *                       TRANSLATION INTO FLINT                          *
     *************************************************************************)

    (** take the abstract syntax tree, generate the flint intermediate code *)
    fun translate{absyn, exportLvars, newstatenv, oldstatenv, compInfo} = let
	(*** statenv used for printing Absyn in messages ***)
	  val statenv = StaticEnv.atop (newstatenv, oldstatenv)
	  in
	    Translate.transDec {
		rootdec = absyn,
		exportLvars = exportLvars,
		oldenv = oldstatenv,
		env = statenv,
		cproto_conv = cproto_conv,
		compInfo = compInfo
	      }
	  end

    val translate =
	  Stats.doPhase (Stats.makePhase "Compiler 040 translate") translate


    (*************************************************************************
     *                       CODE GENERATION                                 *
     *************************************************************************)

    (** take the flint code and generate the machine binary code *)
    local
      val inline = LSplitInline.inline
      val addCode = Stats.addStat (Stats.makeStat "Code Size")
    in
    fun codegen { flint, imports, symenv, splitting, compInfo } = let
	(* hooks for cross-module inlining and specialization *)
	  val (flint, revisedImports) = inline (flint, imports, symenv)
	(* optimized FLINT code *)
	  val (flint, inlineExp) = FLINTOpt.optimize (flint, compInfo, splitting)
	(* from optimized FLINT code, generate the machine code.  *)
	  val csegs = M.compile {
		  prog = flint,
		  source = #sourceName compInfo
		}
	(* Obey the nosplit directive used during bootstrapping.  *)
	(* val inlineExp = if isSome splitting then inlineExp else NONE *)
	  val codeSz = (CodeObj.size(#code csegs) + Word8Vector.length(#data csegs))
	  in
	    addCode codeSz;
	    { csegments=csegs, inlineExp=inlineExp, imports = revisedImports }
	  end
    end (* local codegen *)

    (*
    val codegen =
	Stats.doPhase (Stats.makePhase "Compiler 140 CodeGen") codegen
     *)

    (*************************************************************************
     *                         COMPILATION                                   *
     *        = ELABORATION + TRANSLATION TO FLINT + CODE GENERATION         *
     * used by interact/evalloop.sml, cm/compile/compile.sml only            *
     *************************************************************************)
    (** compiling the ast into the binary code = elab + translate + codegen *)
    fun compile {source, ast, statenv, symenv, compInfo=cinfo, checkErr=check, splitting, guid} = let
	  val {absyn, newstatenv, exportLvars, exportPid, staticPid, pickle } =
		elaborate {ast=ast, statenv=statenv, compInfo=cinfo, guid = guid}
		before (check "elaborate")
	  val absyn =
		instrument {source=source, senv = statenv, compInfo=cinfo} absyn
		before (check "instrument")
	  val {flint, imports} =
		translate {
		    absyn=absyn, exportLvars=exportLvars,
		    newstatenv=newstatenv, oldstatenv=statenv,
		    compInfo=cinfo
		  }
		before check "translate"
	  val {csegments, inlineExp, imports = revisedImports} =
		codegen {
		    flint = flint, imports = imports, symenv = symenv,
		    splitting = splitting, compInfo = cinfo
		  }
		before (check "codegen")
	(*
	 * interp mode was currently turned off.
	 *
	 * if !Control.interp then Interp.interp flint
	 *  else codegen {flint=flint, splitting=splitting, compInfo=cinfo})
	 *)
	  in {
	    csegments = csegments,
	    newstatenv = newstatenv,
	    absyn = absyn,
	    exportPid = exportPid,
	    exportLvars = exportLvars,
	    staticPid = staticPid,
	    pickle = pickle,
	    inlineExp = inlineExp,
	    imports = revisedImports
	  } end (* function compile *)

  end (* functor CompileF *)
