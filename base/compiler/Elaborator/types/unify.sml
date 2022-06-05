(* Copyright 1997 Bell Laboratories *)
(* unify.sml *)

signature UNIFY =
sig

  datatype unifyFail
    = CIRC of Types.tyvar * Types.ty * SourceMap.region * SourceMap.region
        (* circularity *)
    | TYC of Types.tycon * Types.tycon * SourceMap.region * SourceMap.region
        (* tycon mismatch *)
    | TYP of Types.ty * Types.ty * SourceMap.region * SourceMap.region
        (* type mismatch *)
    | UBV of Types.tvKind * Types.ty * SourceMap.region * SourceMap.region
        (* UBOUND match *)
    | OVLD_F of string                  (* overload mismatch *)
    | OVLD_UB of string                 (* overload and user-bound ty var mismatch *)
    | EQ                                (* equality type required *)
    | REC                               (* record labels *)
    | UBVE of Types.tvKind              (* UBOUND, equality mismatch -- never used *)

  exception Unify of unifyFail

  val failMessage: unifyFail -> string

  val unifyTy : Types.ty * Types.ty * SourceMap.region  * SourceMap.region -> unit

  val debugging : bool ref

end (* signature UNIFY *)

(*** type unification ***)
structure Unify: UNIFY =
struct

val debugging = ElabControl.unidebugging

local
  structure S = Symbol
  structure T = Types
  structure TU = TypesUtil
  structure OLC = OverloadClasses
  structure ED = ElabDebug
  open Types

  fun bug msg = ErrorMsg.impossible("Unify: "^msg)

  (* debugging *)
  val say = Control_Print.say
  fun debugMsg (msg: string) =
      ED.debugMsg debugging msg

  val ppType = PPType.ppType StaticEnv.empty
  fun debugPPType (msg,ty) =
      ED.debugPrint debugging (msg, ppType, ty)

in

(* for the time being, not region instrumenting the EQ and REC failures *)
datatype unifyFail
  = CIRC of tyvar * ty * SourceMap.region * SourceMap.region  (* circularity *)
  | EQ                               (* equality type required *)
  | TYC of tycon * tycon * SourceMap.region * SourceMap.region
                                     (* tycon mismatch *)
  | TYP of ty * ty * SourceMap.region * SourceMap.region     (* type mismatch *)
  | OVLD_F of string                 (* overload mismatch -- not a simple type *)
  | OVLD_UB of string                (* mismatch of OVLD and UBOUND tyvars *)
  | UBV of tvKind * ty * SourceMap.region * SourceMap.region  (* UBOUND match *)
  | UBVE of tvKind                   (* UBOUND, equality mismatch -- never used *)
  | REC                              (* record labels *)

fun failMessage (failure: unifyFail) =
    case failure
      of CIRC _ =>    "circularity"
       | EQ =>        "equality type required"
       | TYC _ =>     "tycon mismatch"
       | TYP _ =>     "type mismatch"
       | OVLD_F _ =>  "overload - bad instantiation"
       | OVLD_UB _ => "overload - user bound tyvar" (* DBM: fixes bug 145 *)
       | UBVE _ =>    "UBOUND, equality mismatch"
       | UBV _ =>     "UBOUND match"
       | REC =>       "record labels"

exception Unify of unifyFail


(*************** misc functions *****************************************)

val eqLabel = Symbol.eq

(*
 * tyconEqprop tycon:
 *
 *    This function returns the eqprop of tycon for use in determining
 * when a CONty is an equality type.
 *
 * Note: Calling this function on ERRORtyc produces an impossible
 * because an ERRORtyc should never occur in a CONty and hence an eqprop
 * of an ERRORtyc should never be needed.
 *
 * [GK 5/7/07] The above note is not true. See bug271. Since an error
 * was already flagged, it seems harmless to return YES for the eqprop
 * to avoid possibly spurious eqprop related warnings.
 *
 * Calling this function on a DEFtyc also produces an impossible because
 * the current eqprop scheme is insufficiently expressive to describe
 * the possibilities. (Eg: first argument must be an eq type but not
 * necessarily the second.)  Because of this, it is currently necessary to
 * expand DEFtyc's before checking for equality types.
 *)
fun tyconEqprop (GENtyc { eq, ... }) =
    (case !eq of ABS => NO | ep => ep)
  | tyconEqprop (RECORDtyc _)  = YES
  | tyconEqprop (DEFtyc _) = bug "tyconEqprop: DEFtyc"
  | tyconEqprop (ERRORtyc) = YES
  | tyconEqprop _ = bug "unexpected tycon in tyconEqprop"

(*
 * fieldwise(just1,just2,combine,fields1,fields2):
 *
 *    This function merges two sorted lists of (label, type) pairs
 * (sorted by label) into a single sorted list of (label, type) pairs.
 * If (l1,t1) occurs in fields1 but l1 doesn't occur in fields2 then
 * (l1, just1 t1) occurs in the output.  Similarly with just2.
 * If (l, t1) occurs in fields1 and (l,t2) in fields2, then
 * (l, combine t1 t2) occurs in the output.
 *)
fun fieldwise(_,just2,_,[],fields2) = map (fn (n,t) => (n,just2 t)) fields2
  | fieldwise(just1,_,_,fields1,[]) = map (fn (n,t) => (n,just1 t)) fields1
  | fieldwise(just1,just2,combine,r1 as ((n1,t1)::rest1),r2 as ((n2,t2)::rest2)) =
      if eqLabel(n1,n2) then
	(n1,combine(t1,t2))::(fieldwise(just1,just2,combine,rest1,rest2))
      else if TU.gtLabel(n2,n1) then
	(n1,just1 t1)::(fieldwise(just1,just2,combine,rest1,r2))
      else
	(n2,just2 t2)::(fieldwise(just1,just2,combine,r1,rest2))


(* filter out the non-equality-types from a list of types *)
fun filterEq (nil) = nil
  | filterEq (ty::tys) =
    if EqTypes.isEqType ty then ty :: filterEq tys
    else filterEq tys


(*************** adjust function *****************************************)

(* propagate depth and eq while checking for circularities in the
 * type ty that is going to unify with tyvar var, which is a ref(tvKind)  *)

fun adjustType (var,depth,eq,ty,region1,region2) =
    (* ASSERT: VARty var <> ty *)
    (* region1 is for var, region2 is for ty and may update through iter *)
    let val _ = debugPPType(">>adjustType: ",ty)
	fun iter (_, WILDCARDty, _) = ()
	  | iter (eq, MARKty(ty, region2'), _) = iter (eq, ty, region2')
	  | iter (eq, ty' as VARty(var' as ref(info)), region2) =
	      (case info
		 of INSTANTIATED ty =>
		      (debugMsg "adjustType INSTANTIATED";
		       iter (eq,ty,region2))
		  | OPEN{kind=k,depth=d,eq=e} =>
		      (* check for circularity; if not, propagage eq and depth *)
		      if TU.eqTyvar(var,var')
		      then raise Unify (CIRC(var,ty',region1,region2))
		      else (case k
			      of FLEX fields =>
				  (* recurse into FLEX field types *)
				  app (fn (l,t) => adjustType(var,depth,e,t,region1,region2))
				      fields
			       | _ => ();
			    var' := OPEN{depth=Int.min(depth,d),
					 eq=eq orelse e, kind=k})
		  | UBOUND{depth=d,eq=eq2,name} =>
		      (* check if eq is compatible and propagate depth *)
		      if eq andalso not eq2
		      then raise Unify EQ
		      else if depth < d
		      then var' := UBOUND{depth=depth,eq=eq2,name=name}
		      else ()
		  | OVLDV{sources,eq=eq2} =>
		      (* circularity can't happen, because var can't be OVLD* *)
		      var' := OVLDV{sources = sources, eq = eq orelse eq2}
		  | (OVLDI _ | OVLDW _) => () (* no eq propagation necessary *)
		      (* circularity can't happen, because var can't be OVLD* *)
		  | LBOUND _ => bug "unify:adjustType:LBOUND")
	  | iter (eq, ty as CONty(DEFtyc{tyfun=TYFUN{body,...},...}, args), region2) =
	      ((*app (fn t => iter (false, t, region2)) args; *)
	       iter (eq, TU.headReduceType ty, region2))
	      (* A headReduceType here may cause instTyvar to
	       * infinite loop if this CONty has a nonstrict arg
	       * against which we are unifying/instantiating
	       * Because we may be instantiating to nonstrict
	       * univariables, it is safer to do an occurrence
	       * check on all the arguments. (typing/tests/20.sml)
	       * [GK 4/28/07]
	       * iter should only do the occurrence check and
	       * not propagate eq to the args.
	       * MLRISC/library/dynamic-array.sml's checkArray
	       * is an example. [GK 2/24/08] *)
              (* Note that is involves redundancey -- iter may be, and in
               * general will be, applied to args twice -- rethink? *)
 	  | iter (eq, CONty(tycon,args), region2) =
              (* BUG?: should only iter on the "strict" args -- in case of OBJ
                 may only be one, strict, arg. But in case of YES or default?
	         But here tycon is not a DEFtyc because of previous case, hence
	         it should be ok -- basic tycons are strict. [DBM, 2020.04.12]*)
	      (case tyconEqprop tycon
		 of OBJ => app (fn t => iter (false, t, region2)) args
		  | YES => app (fn t => iter (eq, t, region2)) args
		  | _ =>
		    if eq then raise Unify EQ
		    else app (fn t => iter (false, t, region2)) args)
 (* BUG? why don't these cases blow up (in tyconEqprop) when iter is applied
    to arguments that are unreduced applications of DEFtycs? *)
          | iter (_, POLYty _, _) = bug "Unify.adjustType[POLYty]"
          | iter (_, IBOUND _, _) = bug "Unify.adjustType[IBOUND]"
	  | iter _ = bug "adjustType 3"
     in iter (eq, ty, region2); debugMsg "<<adjustType"
    end

(*************** unify functions *****************************************)

(* OVLD can be instantiated to a type that will have to (after overload
   resolution) turn out to be a primitive type in the appropriate overload class.
 * OVLD (when unified with another OVLD)
 * UBOUND cannot be instantiated, but it's depth property can be reduced
 * OPEN/FLEX can merge with another FLEX or instantiate a META
 * OPEN/META can be instantiated to anything
 *)

(* reorder two tyvars in descending order according to the ordering
 * OVLDI > OVLDW > OVLDV > UBOUND > OPEN/FLEX > OPEN/META *)
fun sortVars(v1 as ref i1, v2 as ref i2) =
    case (i1,i2)
      of (OVLDI _, _) => (v1,v2)
       | (_, OVLDI _) => (v2,v1)
       | (OVLDW _, _) => (v1,v2)
       | (_, OVLDW _) => (v2,v1)
       | (OVLDV _, _) => (v1,v2)
       | (_, OVLDV _) => (v2,v1)
       | (UBOUND _, _) => (v1,v2)
       | (_, UBOUND _) => (v2,v1)
       | (OPEN{kind=FLEX _,...}, _) => (v1,v2)
       | (_, OPEN{kind=FLEX _,...}) => (v2,v1)
       | _ => (v1,v2) (* both OPEN/META *)

(* unifyTy expects that there are no POLYtys with 0-arity;
   CONty(DEFtyc, _) are reduced only if absolutely necessary. *)
fun unifyTy(type1, type2, reg1, reg2) =
    let val type1 = TU.prune type1
	val type2 = TU.prune type2
	val _ = debugPPType(">>unifyTy: type1: ",type1)
	val _ = debugPPType(">>unifyTy: type2: ",type2)
	fun unifyRaw(type1, type2, reg1, reg2) =
	 case (TU.prune type1, TU.prune type2)
	  of (MARKty (ty1, reg1'), type2) => unifyRaw(TU.prune ty1, type2, reg1', reg2)
	   | (type1, MARKty (ty2, reg2')) => unifyRaw(type1, TU.prune ty2, reg1, reg2')
	      (* missing region args to unify, so MARKs are discarded *)
	   | (VARty var1, VARty var2) =>
	       unifyTyvars(var1, var2, reg1, reg2)
	         (* this used to take type1 and type2 as args *)
	   | (VARty var1, type2) =>       (* type2 may be WILDCARDty *)
	       instTyvar(var1, type2, reg1, reg2)
	   | (type1, VARty var2) =>       (* type1 may be WILDCARDty *)
	       instTyvar(var2, type1, reg2, reg1)
	   | (CONty(ERRORtyc, _), _) => ()
	   | (_, CONty(ERRORtyc, _)) => ()
	   | (ty1 as CONty(tycon1,args1), ty2 as CONty(tycon2,args2)) =>
	       if TU.eqTycon(tycon1,tycon2) then
		   (* Because tycons are equal (and not ERRORtyc), they must have the
		      same arity and strictness signatures. Thus lengths of args1 and
		      args2 are the same. Type abbrev. strictness
		      optimization. If tycons equal, then only check
		      strict arguments. [GK 4/28/07]
		    *)
		   (case tycon1
		     of DEFtyc{strict, ...} =>
			let fun unifyArgs ([],[],[]) = ()
			      | unifyArgs (true::ss, ty1::tys1, ty2::tys2) =
				(unifyTy(ty1,ty2,reg1,reg2); unifyArgs(ss,tys1,tys2))
			      | unifyArgs (false::ss, _::tys1, _::tys2) =
				unifyArgs(ss,tys1,tys2)
			      | unifyArgs _ = bug "unifyTy: arg ty lists wrong length"
			in unifyArgs (strict, args1, args2)
			end
		      | _ => ListPair.app (fn (t1,t2) => unifyTy(t1,t2,reg1,reg2))
					  (args1,args2))
	       else raise Unify (TYC(tycon1,tycon2,reg1,reg2))
	  (* if one of the types is WILDCARDty, propagate it down into the
	   * other type to eliminate tyvars that might otherwise cause
	   * generalizeTy to complain. *)
	   | (WILDCARDty, CONty(_, args2)) =>
                app (fn x => unifyTy(x, WILDCARDty, reg1, reg2)) args2
           | (CONty(_, args1), WILDCARDty) =>
                app (fn x => unifyTy(x, WILDCARDty, reg1, reg2)) args1
	   | (WILDCARDty,_) => ()
	   | (_,WILDCARDty) => ()
	   | (ty1,ty2) => raise Unify (TYP(ty1,ty2,reg1,reg2))
    in (* first try unifying without reducing CONty(DEFtycs) *)
       unifyRaw(type1, type2, reg1, reg2)
       handle Unify _ =>
         (* try head reducing type1 *)
         let val type1' = TU.headReduceType type1
         in unifyRaw(type1', type2, reg1, reg2)   (* regions? *)
            handle Unify _ => (* try head reducing type2 *)
                   unifyRaw(type1', TU.headReduceType type2, reg1,reg2) (* regions? *)
                   (* if unification still fails, then type1 and type2
                      really cannot be made to be equal *)
         end
    end

and unifyTyvars (var1: tyvar, var2: tyvar, reg1, reg2) =
    let fun unify(var1 as ref i1, var2 as ref i2) =
	    (* ASSERT: var1 <> var2  -- cf equality test in let-body of unifyTyvars, and
	     *         var1 =< var2 by sortVars, same reason
	     *         var1 and var2 are pruned, i.e. are not INSTANTIATED *)
	    case i1
	     of OVLDI sources1 =>
		 ((case i2
		    of OVLDI sources2 =>
		       (debugMsg "@unifyTyvars[OVLDI/OVLDV]";
			var1 := OVLDI (sources1 @ sources2))	
		     | OVLDW _ =>
		       (debugMsg "@unifyTyvars[OVLDI/OVLDW]";
			raise Unify (OVLD_F "OVLDI/OVLDW"))
		     | OVLDV{sources=sources2,eq=eq2} =>
		       (debugMsg "@unifyTyvars[OVLDI/OVLDV]";
			var1 := i1)
		     | OPEN{kind=FLEX _,...} =>
		       (debugMsg "@unifyTyvars[OVLDI/OPEN:FLEX]";
			raise Unify (OVLD_F "OVLDI/OPEN:FLEX"))
		     | OPEN{eq = eq2, ...} =>
		       (debugMsg ("@unifyTyvars[OVLDI/OPEN] eq: "^Bool.toString eq2);
			var1 := i1)  (* all Int types are equality types *)
		     | UBOUND _ => raise Unify (OVLD_UB "OVLDI")
		     | _ => bug "unifyTyvars OVLDI");
	          var2 := INSTANTIATED(MARKty(VARty var1, reg1)))
		 
              | OVLDW sources1 =>
		 ((case i2
		    of OVLDW sources2 =>
		       (debugMsg ("@unifyTyvars[OVLDW/OVLDW]");
			var1 := OVLDW (sources1 @ sources2))
		     | OVLDV{sources=sources2,eq=eq2} =>
		       (debugMsg ("@unifyTyvars[OVLDI/OVLDV]");
			var1 := i1)
		     | OPEN{kind=FLEX _,...} => raise Unify (OVLD_F "OVLDW/OPEN:FLEX")
		     | OPEN{eq = eq2, ...} =>
		       (debugMsg ("@unifyTyvars[OVLDW/OPEN] eq: "^Bool.toString eq2);
			var1 := i1)  (* all Word types are equality types *)
		     | UBOUND _ => raise Unify (OVLD_UB "OVLDW")
		     | _ => bug "unifyTyvars OVLDW");
	          var2 := INSTANTIATED(MARKty(VARty var1, reg1)))
		 
	      | OVLDV{sources,eq=eq1} =>
		 ((case i2
		    of OVLDV{sources=sources2,eq=eq2} =>
		       (debugMsg ("@unifyTyvars[OVLDV/OVLDV]");
			var1 := OVLDV{sources=sources@sources2, eq = eq1 orelse eq2})
		     | OPEN{kind=FLEX _,...} => raise Unify (OVLD_F "OVLDV/OPEN:FLEX")
		     | OPEN{eq = eq2, ...} =>
		       (debugMsg ("@unifyTyvars[OVLDV/OPEN] eq: "^Bool.toString eq2);
			if eq2 andalso not eq1
			then var1 := OVLDV{sources=sources, eq = true}
			else ())
		     | UBOUND _ => raise Unify (OVLD_UB "OVLDV")
		     | _ => bug "unifyTyvars OVLDI");
	          var2 := INSTANTIATED(MARKty(VARty var1, reg1)))

	       | UBOUND {depth=d1,eq=e1,name} =>
		 (* Note: UBOUND tyvars unify only if equal *)
		  (case i2
		     of OPEN{kind=META,eq=e2,depth=d2} =>
			 if e1 orelse (not e2)
			 then (if d2 < d1
				   then var1 := UBOUND{depth=d2,eq=e1,name=name}
				   else ();
			       var2 := INSTANTIATED (MARKty(VARty var1, reg1)))
			 else raise Unify (UBV(i1,VARty var2,reg1,reg2))
		      | _ => raise Unify (UBV(i1,VARty var2,reg1,reg2)))

	       | OPEN{kind=k1 as FLEX f1,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=k2,eq=e2,depth=d2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in case k2
			       of FLEX f2 =>
				   (app (fn (l,t) => adjustType(var1,d,e,t,reg1,reg2)) f2;
				    app (fn (l,t) => adjustType(var2,d,e,t,reg1,reg2)) f1;
				    var1 :=
				      OPEN{depth=d, eq=e,
					   kind=FLEX(merge_fields(true,true,f1,f2,reg1,reg2))};
				    var2 := INSTANTIATED(MARKty(VARty var1, reg1)))
			        | META =>
				   (app (fn (l,t) => adjustType(var2,d,e,t,reg1,reg2)) f1;
				    var1 := OPEN{kind=k1,depth=d,eq=e};
				    var2 := INSTANTIATED(MARKty(VARty var1, reg1)))
			 end
		      | _ => bug "unifyTyvars OPEN/FLEX")

	       | OPEN{kind=META,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=META,depth=d2,eq=e2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in var1 := OPEN{kind=META,depth=d,eq=e};
			     var2 := INSTANTIATED(MARKty(VARty var1, reg1))
			 end
		      | _ => bug "unifyTyvars OPEN/META")

	       | _ => bug "unifyTyvars tyvar1"
        val _ = debugMsg ">>unifyTyvars"
     in if TU.eqTyvar(var1,var2) then ()
        else unify(sortVars(var1,var2))
    end

(* instTyvar: tyvar * ty * srcloc * srcloc -> unit
 * instTyvar(tv,ty,reg1,reg2) -- instantiate tyvar tv to type ty.
 * ty is not necessarily head normal form.
 * ASSERT: ty is pruned and is not a VARty (otherwise unifyTyvars would
 * have been used instead). *)
and instTyvar (var as ref(OPEN{kind=META,depth,eq}), ty, reg1, reg2) =
      (case ty
         of WILDCARDty => ()
	  | MARKty(ty1, reg2') => instTyvar (var, ty1, reg1, reg2')
	  | _ => adjustType(var, depth, eq, ty, reg1, reg2);
       debugPPType("@instTyvar[OPEN] from:", VARty var);
       debugPPType("@instTyvar[OPEN] to:", ty);
       (* Also need to check for circularity with ty here -- done by adjustType. *)
       var := INSTANTIATED ty)

  | instTyvar (var as ref(OPEN{kind=FLEX fields,depth,eq}), ty, reg1, reg2) =
      let val ty' = TU.headReduceType ty (* try to reduce to a record type *)
       in case ty'
	   of CONty(RECORDtyc field_names, field_types) =>
                let val record_fields = ListPair.zip (field_names,field_types)
                 in app (fn t => adjustType(var,depth,eq,t,reg1,reg2)) field_types;
                    merge_fields(false, true, fields, record_fields, reg1, reg2);
                    var := INSTANTIATED ty
                end
	    | MARKty(ty1, reg2') => instTyvar (var, ty1, reg1, reg2')
            | WILDCARDty => (* propagate WILDCARDty to the fields *)
	       (app (fn (lab,ty) => unifyTy(WILDCARDty,ty,reg1,reg2)) fields)
            | _ => raise Unify (TYP(VARty(var), ty, reg1, reg2))
      end

  | instTyvar (var as ref(OVLDV{eq,...}), ty, reg1, reg2) =
      (debugPPType(">>instTyvar[OVLD]",ty);
       case TU.headReduceType ty
	 of WILDCARDty => ()  (* error survival *)
	  | MARKty(ty1, reg2') => instTyvar(var, ty1, reg1, reg2')
	  | (ty' as CONty(tycon,nil)) =>
	    (* checkiing that it is  a type constant, but not checking if
             * the instantiation is compatible with overload class until
             * overloading resolution *)
	    (debugPPType("instTyvar[OVLD] OK: ",ty');
	     if eq then adjustType(var, T.infinity, eq, ty', reg1, reg2) else ();
	     var := INSTANTIATED(ty'))
	  | ty' => (* not a type constant *)
	    (debugPPType ("instTyvar[OVLD] Fail: ", ty');
	     raise Unify (OVLD_F "OVLD tyvar instantiated to nonconstant type")))

  | instTyvar (var as ref(OVLDI _), ty, reg1, reg2) =
      (debugPPType(">>instTyvar[OVLD]",ty);
       case TU.headReduceType ty
	 of WILDCARDty => ()  (* error survival *)
	  | MARKty(ty1, reg2') => instTyvar(var, ty1, reg1, reg2')
	  | (ty' as CONty(tycon,nil)) =>
	    (* checkiing that it is  a type constant in Int overloading class *)
	    (debugPPType("instTyvar[OVLDI]: ",ty');
	     if OLC.inClass(ty', OLC.intClass)
	     then var := INSTANTIATED(ty')
	     else raise Unify (OVLD_F "INT tyvar instantiated to non-int type"))
	  | ty' => (* not a type constant *)
	    (debugPPType ("instTyvar[OVLDI] Fail: ", ty');
	     raise Unify (OVLD_F "INT tyvar instantiated to nonconstant type")))

  | instTyvar (var as ref(OVLDW _), ty, reg1, reg2) =
      (debugPPType(">>instTyvar[OVLD]",ty);
       case TU.headReduceType ty
	 of WILDCARDty => ()  (* error survival *)
	  | MARKty(ty1, reg2') => instTyvar(var, ty1, reg1, reg2')
	  | (ty' as CONty(tycon,nil)) =>
	    (* checkiing that it is  a type constant in Word overloading class *)
	    (debugPPType("instTyvar[OVLDW]: ",ty');
	     if OLC.inClass(ty', OLC.wordClass)
	     then var := INSTANTIATED(ty')
	     else raise Unify (OVLD_F "WORD tyvar instantiated to non-word type");
	     var := INSTANTIATED(ty'))
	  | ty' => (* not a type constant *)
	    (debugPPType ("instTyvar[OVLDW] Fail: ", ty');
	     raise Unify (OVLD_F "WORD tyvar instantiated to nonconstant type")))

  | instTyvar (var as ref(i as UBOUND _), ty, reg1, reg2) =
      (case ty
         of WILDCARDty => ()
	  | MARKty(ty1, reg2') => instTyvar(var, ty1, reg1, reg2')
          | _ => raise Unify (UBV(i, ty, reg1, reg2)))
              (* could return the ty for error msg*)

  | instTyvar (ref(INSTANTIATED _),_,_,_) = bug "instTyvar: INSTANTIATED"
  | instTyvar (ref(LBOUND _),_,_,_) = bug "instTyvar: LBOUND"

(*
 * merge_fields(extra1,extra2,fields1,fields2):
 *
 *    This function merges the 2 sorted field lists.  Fields occuring
 * in both lists have their types unified.  If a field occurs in only
 * one list, say fields{i} then if extra{i} is true, an Unify error
 * is raised.
 *)
and merge_fields(extra1, extra2, fields1, fields2, reg1, reg2) =
    let fun extra allowed t =
	if not allowed
	then raise Unify REC
	else t
     in fieldwise(extra extra1, extra extra2,
                  (fn (t1,t2) => (unifyTy(t1, t2, reg1, reg2); t1)),
		  fields1, fields2)
    end

end (* local *)
end (* structure Unify *)
