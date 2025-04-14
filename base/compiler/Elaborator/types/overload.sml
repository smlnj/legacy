(* Elaborator/types/overload.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature OVERLOAD =
  sig

  (* create a new overloading environment, which is represented by three functions:
   *	pushv -- add an overloaded variable to the environment
   *	pushl -- add an overloaded literal to the environment
   *	resolve -- resolve the overloadings in the environment
   *)
    val new : unit -> {
	    pushv : VarCon.var ref * SourceLoc.region -> Types.ty,
	    pushl : IntInf.int * Types.ty * string * SourceLoc.region -> Types.ty,
	    resolve : StaticEnv.staticEnv -> unit
	  }

    val debugging : bool ref (* = ElabControl.debugging = Control.Elab.debugging *)

  end  (* signature OVERLOAD *)

structure Overload : OVERLOAD =
struct

local (* imports *)

  structure SL = SourceLoc
  structure EM = ErrorMsg

  structure BT = BasicTypes
  structure TU = TypesUtil
  structure ED = ElabDebug
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure Ty = Types
  structure VC = VarCon
  structure OLV = OverloadVar
			
in
    val debugging = ElabControl.ovlddebugging

    fun bug msg = ErrorMsg.impossible("Overload: "^msg)

    fun debugMsg (msg: string) = ED.debugMsg debugging msg

    val ppType = PPType.ppType StaticEnv.empty
    fun debugPPType (msg, ty) = ED.debugPrint debugging (msg, ppType, ty)

    (* var_info: information about overloaded variables *)
    type var_info = VC.var ref * Ty.tyvar * SL.region

  (* information about overloaded literals; once the type has been resolved, we use this
   * information to check that the literal value is within range for its type.
   * The 3rd and 4th fields of the tuple are used in error messages when the value
   * is out of range.
   *)
    type lit_info = IntInf.int * Ty.ty * string * SL.region

  (* overloaded functions *)
    fun new () =
	let (* the overloaded variable and literal stacks *)
	  val overloadedvars = ref (nil: var_info list)
	  val overloadedlits = ref (nil: lit_info list)
				   
	  (* push an overloaded variable onto the var list *)
	  fun pushvar (varref as ref(VC.OVLDvar{name,variants}), region) =
	      let val tyvar = ref(Ty.OVLDV{eq=false,sources=[(name,region)]})
		  val scheme = OLV.symToScheme name
		  val schemeInst = TU.applyTyfun(scheme,[Ty.VARty tyvar])
	      in
		  debugMsg ">>ovld-push";
		  overloadedvars := (varref,tyvar,region) :: !overloadedvars;
		  debugPPType("<<ovld-push "^Symbol.name name, schemeInst);
		  schemeInst
	      end
	    | pushvar _ = bug "Overload.push"

          (* push an overloaded literal onto the lit list *)
	  fun pushlit (info as (_, ty, _, _)) =
	      (overloadedlits := info :: !overloadedlits;
	       ty)

        (* resolve variable and literal overloadings *)
	fun resolve env =
	    (* this function implements defaulting behavior -- if the context type
	     * is uninstantiated, and all variants match, the first one variant
	     * is used as the default (at the moment, this is always the intTy variant,
	     * until more overloaded ops (for reals, chars, or strings) are added).
	     * For defaulting to work correctly when matching different OVLD tyvars,
	     * it is assumed that the ordering of options is consistent (e.g., between
	     * operators like +, -, and * ). These orderings are established by the
	     * order they appear in the overload declaration.
	     *)
	    let fun resolveOVLDvar(varref as ref(VC.OVLDvar{name,variants}), context, region) =
		    let val contextTy = TU.headReduceType(Ty.VARty context)
			val defaultTy = OLV.defaultTy name
			val (defaultVar :: _) = variants
			val _ = debugPPType
			      (concat[">>resolveOVLDvar ", Symbol.name name, ", contextTy:"],
			       contextTy)
		    in case contextTy
			of Ty.VARty(tyvar as ref(Ty.OVLDV _)) =>
			   (varref := defaultVar;
			    tyvar := Ty.INSTANTIATED defaultTy)
			 | _ =>
			   (case OverloadVar.resolveVar (name,contextTy,variants)
			     of SOME var => varref := var
			     |  NONE => 
				EM.error region EM.COMPLAIN
				   "overloaded variable not defined at context type"
				   (fn ppstrm =>
				       (PPType.resetPPType();
					PP.newline ppstrm;
					PP.string ppstrm "symbol: ";
					PU.ppSym ppstrm name;
					PP.newline ppstrm;
					PP.string ppstrm "type: ";
					PPType.ppType env ppstrm (Ty.VARty context))))
		    end (* fun resolveOVLDvar *)

	        (* resolve overloaded literals *)
	        fun resolveOVLDlit (value, Ty.VARty tyvar, _, _) =
		    (case !tyvar
		      of Ty.OVLDI _ =>
			 (tyvar := Ty.INSTANTIATED BT.intTy)  (* default *)
		       | Ty.OVLDW _ =>
			 (tyvar := Ty.INSTANTIATED BT.wordTy) (* default *)
		       | Ty.INSTANTIATED ty => ()
		       | _ => bug "resolveOVLDlit")

	    (* check that overloaded literals are in range; note that we have
	     * do this check as a separate pass after *all* overloading has
	     * been resolved, because literal resolution does not follow
	     * instance chains.
	     *)
	        fun checkLitRange ((value, ty, src, region): lit_info) =
		    if TU.numInRange(value, ty)
		    then ()
		    else EM.error region EM.COMPLAIN
			     (String.concat["literal '", src, "' is too large for type "])
			     (fn ppstrm => PPType.ppType env ppstrm ty)

	        val overloadedLits = rev (!overloadedlits)
		val overloadedVars = rev (!overloadedvars)
	     in
		app resolveOVLDlit overloadedLits;
		app resolveOVLDvar overloadedVars;
		app checkLitRange overloadedLits
	    end (* fun resolve *)
	 in
	    {pushv = pushvar, pushl = pushlit, resolve = resolve}
	 end (* new *)

end (* top local (imports *)
end (* structure Overload *)
