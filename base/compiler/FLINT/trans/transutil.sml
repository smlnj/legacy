(* transutil.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TransUtil =
struct

local  (* don't need all these structures *)
  structure DA = Access
  structure SP = SymPath
  structure V = VarCon
  structure T = Types
  structure TU = TypesUtil
  structure PL = PLambda
  open Absyn
in

val debugging = FLINT_Control.trdebugging
fun bug msg = ErrorMsg.impossible("TransUtil: " ^ msg)
val say = Control.Print.say

fun ppType ty =
    ElabDebug.withInternals
     (fn () => ElabDebug.debugPrint debugging
		("type: ", PPType.ppType StaticEnv.empty, ty))

fun ident x = x
val unitLexp = PL.RECORD []

(* pathToName would be a better name for this function *)
fun getNameOp p = if SP.null p then NONE else SOME(SP.last p)

type pid = PersStamps.persstamp
type compInfo = Absyn.dec CompInfo.compInfo

(** old-style fold for cases where it is partially applied *)
fun fold f l init = foldr f init l

(** sorting the record fields for record types and record expressions *)
fun elemgtr ((LABEL{number=x,...},_),(LABEL{number=y,...},_)) = (x>y)
fun sorted x = ListMergeSort.sorted elemgtr x
fun sortrec x = ListMergeSort.sort elemgtr x

(** check if an access is external *)
fun extern (DA.EXTERN _) = true
  | extern (DA.PATH(a, _)) = extern a
  | extern _ = false

(** an exception raised if coreEnv is not available *)
exception NoCore

(** instPoly : ty * ty list -> ty
 * instPoly(t,ts): the type t is instantiated with parameters ts.
 * Checked innvariant: ts <> nil <==>  t is polymophic (a POLYty) (DBM) *)
fun instPoly(ty: T.ty, tys : T.ty list) : T.ty =
    case tys
      of nil =>  (* no instantiation parameters *)
         (case ty
            of T.POLYty{tyfun=T.TYFUN{arity,body},...} =>
               if arity = 0 then body
               else (say "instPoly: polytype with no inst parameters\n";
                     ppType ty;
                     ty)
             | _ => ty)
       | _ =>    (* instantiation parameters *)
         (case ty
            of T.POLYty _ => TU.applyPoly(ty, tys)
             | _ => bug "instPoly: non-polytype with inst parameters")

(* aconvertPat: A.pat * compInfo -> A.pat * V.var list * V.var list
 *   "alpha convert" a pattern with respect to the lvar access values
 *   of the pattern variables. Original variables are replaced by
 *   new ones, with fresh LVAR accesses and new refs for the typ field.
 *   Returns the converted pattern, the list of the original pattern
 *   variables (VALvars), and the list of new variables (VALvars).
 *   Called only once, in mkVB inside mkVBs in Translate.
 *   DBM: why is this function needed? *)

fun aconvertPat (pat, {mkLvar=mkv, ...} : compInfo)
    : Absyn.pat * V.var list * V.var list =
    let val varmap : (V.var * V.var) list ref = ref nil
            (* association list mapping old vars to new *)
        (* ASSERT: any var in a VARpat will have an LVAR access. *)
        (* ASSERT: pat will not contain MARKpat *)
	fun mappat (VARpat(oldvar as V.VALvar{access=DA.LVAR(oldlvar),
                                            typ=ref oldtyp,prim,btvs,path})) =
              let fun find ((V.VALvar{access=DA.LVAR(lv),...}, newvar)::rest) =
                        if lv=oldlvar then newvar else find rest
			(* a variable could occur multiple times because
                           repetition in OR patterns *)
                    | find (_::rest) = bug "aconvertPat: bad varmap key"
		    | find nil =
		        let val (newtyp,_) = TypesUtil.instantiatePoly oldtyp
			    val newvar =
                                V.VALvar{access=DA.dupAcc(oldlvar,mkv), prim=prim,
					  typ=ref newtyp, path=path, btvs = btvs}
			 in varmap := (oldvar,newvar)::(!varmap); newvar
			end
	       in VARpat(find(!varmap))
	      end
	  | mappat (VARpat _) = bug "aconvertPat: bad variable"
	  | mappat (RECORDpat{fields,flex,typ}) =
	      RECORDpat{fields=map (fn(l,p)=>(l,mappat p)) fields,
                        flex=flex, typ=typ}
	  | mappat (VECTORpat(pats,t)) = VECTORpat(map mappat pats, t)
	  | mappat (APPpat(d,c,p)) = APPpat(d,c,mappat p)
	  | mappat (ORpat(a,b)) = ORpat(mappat a, mappat b)
	  | mappat (CONSTRAINTpat(p,t)) = CONSTRAINTpat(mappat p, t)
	  | mappat (LAYEREDpat(p,q)) = LAYEREDpat(mappat p, mappat q)
	  | mappat (MARKpat(p,_)) = bug "aconvertPat: MARKpat"
	  | mappat p = p

        val newpat = mappat pat

        val (oldvars,newvars) = ListPair.unzip (!varmap)

     in (newpat,oldvars,newvars)
    end (* aconvertPat *)

end (* local *)
end (* structure TransUtil *)
