(* lty.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Lty : LTY =
struct

structure PT = PrimTyc

fun bug s = ErrorMsg.impossible ("Lty:" ^ s)

(***************************************************************************
 *                UTILITY FUNCTIONS FOR HASHCONSING BASICS                 *
 ***************************************************************************)

(** hashconsing implementation basics *)
local (* hashconsing *)
  val MVAL = 10000     (* big enough? No more than MVAL variables per binder *)
  val BVAL = MVAL * 2  (* bound on encoded tvars of innermost binder *)
in

(* enc_tvar: encoded type variables = deBruijn indexes * binder arity indexes *)
(* Type lambda bindings (TC_FN) bind several variables at a time,
 * i.e. they are n-ary for some n, with each type variable given a kind.
 * A type variable is represented by a pair (d,k), where d is a
 * 1-based deBruijn index designating a lambda binder by its lambda
 * nesting level, counting inside out, and k is a 0-based index
 * into the list of the type variables bound by that binder.
 * These (d,k) pairs are encoded into a single integer by tvEncode,
 * and the pair can be recovered from its encoding by tvDecode.
 * ASSUMPTION: k < MVAL = 10000. *)

type enc_tvar = int  (* tv : enc_tvar => tv >= MVAL *)
fun tvEncode (d, k) = d * MVAL + k   (* d >= 1, k >= 0, k < MVAL *)
fun tvDecode x = ((x div MVAL), (x mod MVAL))

(* enc_tvars < BVAL are bound by the innermost TC_FN binder.
 * exitLevel takes a list of enc_tvars and eliminates those bound
 * by the innermost binder, and decrements the d-level of the remainder,
 * thus effectively popping out of the innermost binding context.
 *)
fun exitLevel (xs: enc_tvar list) : enc_tvar list =
    let fun h ([], x) = rev x
          | h (a::r, x) = if a < BVAL then h(r, x) else h(r, (a-MVAL)::x)
    in h(xs, [])
    end

(* tvar : "named"(?) tyc variables.
   For now(?), these share the same "namespace" with lvars. *)
(* [KM ???] Are these used at all? Yes, they are used after
 * translation into the flint language(?). Are these the
 * "run-time" type parameters? *)
type tvar = LambdaVar.lvar (* = int, coincidentally = enc_tvar *)
val mkTvar = LambdaVar.mkLvar

(* aus_info: auxiliary information maintained in hash_cells.
 * bool records whether the contents is fully normalized,
 * enc_tvar list and tvar list contain free type variables (both sorted) *)

(* for lists of free type variables, deBruijn indices are collapsed
   into a single integer using tvEncode/tvDecode, named variables use
   the tvar as an integer.  The deBruijn-indexed list is kept sorted,
   the named variables are in arbitrary order (for now) --league, 2 July 1998.

   [DBM,8/18/06]: but the mergeTvs function below is used to merge both
   enc_tvar (deBruijn) and tvar lists, and it assumes its argument lists
   are sorted.
 *)
datatype aux_info
  = AX_REG of bool           (* normalization flag *)
            * enc_tvar list  (* free debruijn-indexed type vars, sorted *)
            * tvar list      (* free named type vars, sorted? *)
  | AX_NO                    (* no aux_info available *)

(* Functions for merging lists of env_tvars and tvars. *)
local
  fun mergeLists cmp = let
	fun merge (l, []) = l
	  | merge ([], l) = l
	  | merge (xs as (x :: xr), ys as (y :: yr)) = (case cmp (x, y)
	       of LESS => x :: merge (xr, ys)
		| EQUAL => x :: merge (xr, yr)
		| GREATER => y :: merge (xs, yr)
	      (* end case *))
	in
	  merge
	end
in
val mergeTvs = mergeLists LambdaVar.compare	(* regular type variables *)
val mergeEncTvs = mergeLists Int.compare	(* deBruijn encoded type variables *)
end

(* fmergeTvs : tvar list list -> tvar list
 * merge a list of sorted lists of tvars into a single sorted list,
 * eliminating duplicates. *)
fun fmergeTvs [] = []
  | fmergeTvs (h :: t) = foldr mergeTvs h t

(* hash cells -- used to represent hash consed types plambda types *)
type 'a hash_cell = (int * 'a * aux_info) ref

end (* local of hashconsing implementation basics *)

(***************************************************************************
 *                 DATATYPE DEFINITIONS                                    *
 ***************************************************************************)

(** definition of kinds for all the lambda tycs *)
(* [KM???] TK_BOX does not appear to be used *)
datatype tkindI
  = TK_MONO                                    (* ground mono tycon *)
  | TK_BOX                                     (* boxed/tagged tycon *)
  | TK_SEQ of tkind list                       (* sequence of tycons *)
  | TK_FUN of tkind list * tkind               (* n-ary tycon function *)

withtype tkind = tkindI hash_cell              (* hash-consing-impl of tkind *)

(* an special extensible token key *)
type token = int

datatype fflag                                 (* calling conventions *)
  = FF_VAR of bool * bool                      (* is it fixed ? *)
  | FF_FIXED                                   (* used after rep. analysis *)

datatype rflag = RF_TMP                        (* tuple kind: a template *)
 (* [dbm] only one rflag value, so doesn't discriminate anything,
  * therefore redundant, could be removed *)

(** definitions of concrete plambda type constructors *)
datatype tycI
  = TC_VAR of DebIndex.index * int             (* tyc variables [why not enc_tvar?] *)
  | TC_NVAR of tvar                            (* "named" tyc variables *)
  | TC_PRIM of PrimTyc.primtyc                 (* primitive tyc *)

  | TC_FN of tkind list * tyc                  (* tyc abstraction, n-ary *)
  | TC_APP of tyc * tyc list                   (* tyc application, n-ary *)
  | TC_SEQ of tyc list                         (* tyc sequence *)
  | TC_PROJ of tyc * int                       (* tyc projection *)

  | TC_SUM of tyc list                         (* sum tyc *)
  | TC_FIX of				       (* datatype tyc *)
    {family :                                  (* recursive dt family *)
      {size : int,                             (* size of family *)
       names : string vector,                  (* datatype names for printing *)
       gen : tyc,                              (* common generator fn *)
       params: tyc list},                      (* parameters for generator *)
      index : int}                             (* index of this dt in family *)
     (* TC_FIX are built in trans/transtypes.sml*)

  | TC_TUPLE of rflag * tyc list               (* std record tyc *)
  | TC_ARROW of fflag * tyc list * tyc list    (* std function tyc *)
  | TC_PARROW of tyc * tyc                     (* special fun tyc [not used] *)

  | TC_BOX of tyc                              (* boxed tyc *)
  | TC_ABS of tyc                              (* abstract tyc  [not used] *)
  | TC_TOKEN of token * tyc                    (* extensible token tyc *)
  | TC_CONT of tyc list                        (* intern continuation tycon *)
  | TC_IND of tyc * tycI                       (* indirect tyc "thunk" *)
  | TC_ENV of tyc * int * int * tycEnv         (* tyc closure *)

withtype tyc = tycI hash_cell                  (* hash-consed tyc cell *)
     and tycEnv = tyc

(* tycEnv are lists of "type environment binders" (see datatype teBinder
 * below), but are encoded as tycs so that the hash-consing machinery
 * for tycs can be applied to them. [dbm, 1/25/07] *)

(** definitions of lambda types *)
datatype ltyI
  = LT_TYC of tyc                              (* monomorphic type *)
  | LT_STR of lty list                         (* structure record type *)
  | LT_FCT of lty list * lty list              (* functor arrow type *)
  | LT_POLY of tkind list * lty list           (* polymorphic type *)

  | LT_CONT of lty list                        (* internal cont type *)
  | LT_IND of lty * ltyI                       (* a lty thunk and its sig *)
  | LT_ENV of lty * int * int * tycEnv         (* lty closure *)

withtype lty = ltyI hash_cell                  (* hash-consed lty cell *)


(***************************************************************************
 *                   TOKEN TYC UTILITY FUNCTIONS                           *
 ***************************************************************************)

type token_info
  = {name      : string,
     abbrev    : string,
     reduce_one: token * tyc -> tyc,
     is_whnm   : tyc -> bool,
     is_known  : token * tyc -> bool}

local val token_key = ref 0
      val token_table_size = 10
      val default_token_info : token_info =
        {name="TC_GARBAGE",
         abbrev="GB",
         reduce_one=(fn _ => bug "token not implemented"),
         is_whnm=(fn _ => bug "token not implemented"),
         is_known=(fn _ => bug "token not implemented")}
      val token_array : token_info Array.array =
            Array.array(token_table_size,default_token_info)
      val token_validity_table = Array.array(token_table_size,false)
      fun get_next_token () =
        let val n = !token_key
         in if n > token_table_size then bug "running out of tokens"
            else (token_key := n+1; n)
        end
      fun store_token_info (x, k) = Array.update(token_array, k, x)
      fun get_is_whnm k = #is_whnm (Array.sub(token_array, k))
      fun get_reduce_one (z as (k, t)) =
            (#reduce_one (Array.sub(token_array, k))) z
      fun get_name k = #name (Array.sub(token_array, k))
      fun get_abbrev k = #abbrev (Array.sub(token_array, k))
      fun get_is_known (z as (k, t)) =
            (#is_known (Array.sub(token_array, k))) z
      fun is_valid k = Array.sub(token_validity_table, k)
      fun set_valid k = Array.update(token_validity_table, k, true)
in

val register_token: token_info -> token =
  (fn x => let val k = get_next_token ()
            in store_token_info(x, k); set_valid k; k
           end)

val token_name    : token -> string = get_name
val token_abbrev  : token -> string = get_abbrev
val token_whnm    : token -> tyc -> bool = get_is_whnm
val token_reduce  : token * tyc -> tyc = get_reduce_one
val token_isKnown : token * tyc -> bool = get_is_known
val token_isvalid : token -> bool = is_valid
val token_eq      : token * token -> bool = fn (x,y) => (x=y)
val token_int     : token -> int = fn x => x
val token_key     : int -> token = fn x => x

end (* end of all token-related hacks *)

(***************************************************************************
 *                   HASHCONSING IMPLEMENTATIONS                           *
 ***************************************************************************)

(** Hash-consing implementations of tyc, tkind, lty *)

local (* hashconsing impl *)
  structure Weak = SMLofNJ.Weak
  structure PT = PrimTyc
  structure DI = DebIndex

  fun bug msg = ErrorMsg.impossible("LtyKernel: "^msg)

  val itow = Word.fromInt
  val wtoi = Word.toIntX
  val andb = Word.andb

  val N = 2048 (* 1024 *)
  val NNdec = itow (N*N) - 0w1
  val P = 0w509 (* was 0w1019, a prime < 1024 so that N*N*P < maxint *)

  val tk_table : tkind Weak.weak list Array.array = Array.array(N,nil)
  val tc_table : tyc Weak.weak list Array.array = Array.array(N,nil)
  val lt_table : lty Weak.weak list Array.array = Array.array(N,nil)

  fun vector2list v = Vector.foldr (op ::) [] v

  fun revcat(a::rest,b) = revcat(rest,a::b)
    | revcat(nil,b) = b

  fun combine [x] = itow x
    | combine (a::rest) =
        andb(itow a +(combine rest)*P, NNdec)
    | combine _ = bug "unexpected case in combine"

  (*
   * Because of the "cmp" function below, it's necessary to keep
   * each bucket-list in a consistent order, and not reverse
   * or move-to-front or whatever.
   *)
  fun look(table, h, t, eq, mk) =
    let val i = wtoi(andb(itow h, itow(N-1)))

        fun g(l, z as (w::rest)) =
              (case Weak.strong w
                of SOME (r as ref(h',t',_)) =>
                    if (h=h') andalso (eq {new=t, old=t'})
                    then (Array.update(table, i, revcat(l,z)); r)
                    else g(w::l, rest)
                 | NONE => g(l, rest))
          | g(l, []) =
              let val r = mk(h, t)
               in Array.update(table, i, (Weak.weak r) :: rev l); r
              end

     in g([], Array.sub(table, i))
    end

  fun cmp(table, a as ref(ai,_,_), b as ref (bi,_,_)) =
    if ai < bi then LESS
    else if ai > bi then GREATER
    else if a = b then EQUAL (* pointer equality on refs *)
    else (* ai = bi, so a,b in same bucket of table, use order
          * a and b appear in the bucket *)
         let val index = wtoi (andb(itow ai,itow(N-1)))
             fun g [] = bug "unexpected case in cmp"
               | g (w::rest) =
                 (case Weak.strong w
                   of SOME r =>
                      if a=r then LESS
                      else if b=r then GREATER
                      else g rest
                    | NONE => g rest)
          in g(Array.sub(table,index))
         end


  fun getnum (ref(i,_,_)) = i

  fun tagnums nil = nil
    | tagnums ((i,t)::rest) = i::getnum t::tagnums rest

  fun tk_hash (TK_MONO) = 0w1
    | tk_hash (TK_BOX) = 0w2
    | tk_hash (TK_SEQ ks) = combine (3::map getnum ks)
    | tk_hash (TK_FUN(ks, k)) = combine (4::getnum k::(map getnum ks))

  fun tc_hash tc =
    case tc
     of (TC_VAR(d, i)) => combine [1, (DI.di_key d)*10, i]
      | (TC_NVAR v) => combine[15, LambdaVar.toId v]
      | (TC_PRIM pt) => combine [2, PT.pt_toint pt]
      | (TC_FN(ks, t)) => combine (3::(getnum t)::(map getnum ks))
      | (TC_APP(t, ts)) => combine (4::(getnum t)::(map getnum ts))
      | (TC_SEQ ts) => combine (5::(map getnum ts))
      | (TC_PROJ(t, i)) => combine [6, (getnum t), i]
      | (TC_SUM ts) => combine (7::(map getnum ts))
      | (TC_FIX{family={size=n,gen=t,params=ts,...},index=i}) =>
          (* names not involved the the hash *)
          combine (8::n::i::(getnum t)::(map getnum ts))
      | (TC_ABS t) => combine [9, getnum t]
      | (TC_BOX t) => combine [10, getnum t]
      | (TC_TUPLE (_, ts)) => combine (11::(map getnum ts))
      | (TC_ARROW(rw, ts1, ts2)) =>
          let fun h (FF_FIXED) = 10
                | h (FF_VAR(true,b2)) = if b2 then 20 else 30
                | h (FF_VAR(false,b2)) = if b2 then 40 else 50
          in combine (12::(h rw)::(map getnum (ts1@ts2)))
          end
      | (TC_PARROW (t1,t2)) => combine [13, getnum t1, getnum t2]
      | (TC_TOKEN (i, tc)) => combine [14, i, getnum tc]
      | (TC_CONT ts) => combine (15::(map getnum ts))
      | (TC_ENV(t,i,j,env)) =>
          combine[16, getnum t, i, j, getnum env]
      | (TC_IND _) => bug "unexpected TC_IND in tc_hash"

  fun lt_hash lt =
    case lt
     of (LT_TYC t) => combine [1, getnum t]
      | (LT_STR ts) => combine (2::(map getnum ts))
      | (LT_FCT(ts1, ts2)) =>
          combine (3::(map getnum (ts1@ts2)))
      | (LT_POLY(ks, ts)) =>
          combine (4::((map getnum ts)@(map getnum ks)))
      | (LT_CONT ts) => combine (5::(map getnum ts))
      | (LT_ENV(t,i,j,env)) =>
          combine [6, getnum t, i, j, getnum env]
      | (LT_IND _) => bug "unexpected LT_IND in lt_hash"

  fun tkI_eq {new: tkindI, old} = (new = old)

  (* the 1st is one being mapped; the 2nd is in the hash table *)
  fun tcI_eq {new : tycI, old=TC_IND(_,s)} = tcI_eq {new=new, old=s}
    | tcI_eq {new, old} = (new=old)

  fun ltI_eq {new : ltyI, old=LT_IND(_,s)} = ltI_eq {new=new, old=s}
    | ltI_eq {new, old} = (new=old)

  val baseAux = AX_REG (true, [], [])

  fun getAux (ref(i : int, _, x)) = x

  fun mergeAux(AX_NO, _) = AX_NO
    | mergeAux(_, AX_NO) = AX_NO
    | mergeAux(AX_REG(b1,vs1,nvs1), AX_REG(b2,vs2,nvs2)) =
        AX_REG(b2 andalso b1, mergeEncTvs(vs1, vs2),
               mergeTvs(nvs1, nvs2))

  fun fsmerge [] = baseAux
    | fsmerge [x] = getAux x
    | fsmerge xs =
        let fun loop([], z) = z
              | loop(_, AX_NO) = AX_NO
              | loop(a::r, z) = loop(r, mergeAux(getAux a, z))
         in loop(xs, baseAux)
        end

  fun exitAux(AX_REG(b, vs, nvs)) = AX_REG(b, exitLevel vs, nvs)
    | exitAux x = x

  fun tc_aux tc =
      case tc
       of (TC_VAR(d, i)) => AX_REG(true, [tvEncode(d, i)], [])
        | (TC_NVAR v) => AX_REG(true, [], [v])
        | (TC_PRIM pt) => baseAux
        | (TC_APP(ref(_, TC_FN _, AX_NO), _)) => AX_NO
        | (TC_PROJ(ref(_, TC_SEQ _, AX_NO), _)) => AX_NO
        | (TC_APP(ref(_, TC_FN _, AX_REG(_,vs,nvs)), ts)) =>
            mergeAux(AX_REG(false, vs, nvs), fsmerge ts) (* ? *)
        | (TC_PROJ(ref(_, TC_SEQ _, AX_REG(_,vs,nvs)), _)) =>
            AX_REG(false, vs, nvs) (* ? *)
        | (TC_FN(ks, t)) => exitAux(getAux t)
        | (TC_APP(t, ts)) => fsmerge (t::ts)
        | (TC_SEQ ts) => fsmerge ts
        | (TC_PROJ(t, _)) => getAux t
        | (TC_SUM ts) => fsmerge ts
        | (TC_FIX{family={gen,params,...},...}) =>
            let val ax = getAux gen
            in case ax
                of AX_REG(_,[],[]) => mergeAux(ax, fsmerge params)
                 | AX_REG _ => bug "unexpected TC_FIX freevars in tc_aux"
                 | AX_NO => AX_NO
            end
        | (TC_ABS t) => getAux t
        | (TC_BOX t) => getAux t
        | (TC_TUPLE (_, ts)) => fsmerge ts
        | (TC_ARROW(_, ts1, ts2)) => fsmerge (ts1@ts2)
        | (TC_PARROW(t1, t2)) => fsmerge [t1, t2]
        | (TC_TOKEN (k, (ref(_, t, AX_NO)))) => AX_NO
        | (TC_TOKEN (k, (x as ref(_, t, AX_REG(b,vs,nvs))))) =>
            AX_REG((token_whnm k x) andalso b, vs, nvs)
        | (TC_CONT ts) => fsmerge ts
        | (TC_IND _) => bug "unexpected TC_IND in tc_aux"
        | (TC_ENV _) => AX_NO

  fun lt_aux (LT_TYC t) = getAux t
    | lt_aux (LT_STR ts) = fsmerge ts
    | lt_aux (LT_FCT(ts1, ts2)) = fsmerge (ts1@ts2)
    | lt_aux (LT_POLY(ks, ts)) = exitAux(fsmerge ts)
    | lt_aux (LT_CONT ts) = fsmerge ts
    | lt_aux (LT_IND _) = bug "unexpected LT_IND in lt_aux"
    | lt_aux (LT_ENV _) = AX_NO

  fun tk_mk (i : int, k: tkindI) = ref (i, k, AX_NO)
  fun tc_mk (i : int, tc : tycI) = ref (i, tc, tc_aux tc)
  fun lt_mk (i : int, lt : ltyI) = ref (i, lt, lt_aux lt)

in (* body hashconsing impl *)

(** a temporary hack on getting the list of free tyvars *)
(* ignores named vars for now.  --CALeague, 1 Jul 1998 *)
fun tc_vs (ref(_ : int, _ : tycI, AX_NO)) = NONE
  | tc_vs (ref(_ : int, _ : tycI, AX_REG (_,x,_))) = SOME x

fun lt_vs (ref(_ : int, _ : ltyI, AX_NO)) = NONE
  | lt_vs (ref(_ : int, _ : ltyI, AX_REG (_,x,_))) = SOME x

(** converting from the hash-consing reps to the standard reps *)
fun tk_outX (r as ref(_ : int, t : tkindI, _ : aux_info)) = t
fun tc_outX (r as ref(_ : int, t : tycI, _ : aux_info)) = t
fun lt_outX (r as ref(_ : int, t : ltyI, _ : aux_info)) = t

(** converting from the standard reps to the hash-consing reps *)
fun tk_injX t = look(tk_table, wtoi(tk_hash t), t, tkI_eq, tk_mk)
fun tc_injX t = look(tc_table, wtoi(tc_hash t), t, tcI_eq, tc_mk)
fun lt_injX t = look(lt_table, wtoi(lt_hash t), t, ltI_eq, lt_mk)

(** key-comparison on tkind, tyc, lty *)
fun tk_cmp (k1, k2) = cmp(tk_table, k1, k2)
fun tc_cmp (t1, t2) = cmp(tc_table, t1, t2)
fun lt_cmp (t1, t2) = cmp(lt_table, t1, t2)

(** get the hash key of each lty, only used by reps/coerce.sml; a hack **)
fun lt_key (ref (h : int, _ : ltyI, _ : aux_info)) = h

(** checking if a tyc or an lty is in normal form *)
fun  tcp_norm ((ref(_, TC_IND _, AX_REG(true,_,_))) : tyc) =
       bug "TC_IND is norm?!"
   | tcp_norm ((ref(_, _, AX_REG(b,_,_))) : tyc) =  b
   | tcp_norm _ = false

fun ltp_norm ((ref(_, LT_TYC (ref (_,TC_IND _, _)), AX_REG(true,_,_))) : lty) =
      bug "LT_TYC(TC_IND) is norm?!"
  | ltp_norm ((ref(_, LT_IND _, AX_REG(true,_,_))) : lty) =
      bug "LT_IND is norm?!"
  | ltp_norm ((ref(_, _, AX_REG(b,_,_))) : lty) =  b
  | ltp_norm _ = false

(** accessing free named tyvars *)
fun tc_nvars (tyc:tyc) =
    case getAux tyc
     of AX_REG (_,_,tvs) => tvs
      | AX_NO => bug "unexpected case in tc_nvars"

fun lt_nvars (lty:lty) =
    case getAux lty
     of AX_REG (_,_,tvs) => tvs
      | AX_NO => bug "unexpected case in lt_nvars"

end (* local -- hashconsing impl *)

(***************************************************************************
 *            UTILITY FUNCTIONS ON TYC ENVIRONMENT                         *
 ***************************************************************************)

(* tycEnvs are represented by an encoding as tycs. The abstract representation
 * of tycEnvs would be given by:
 *
 *   datatype teBinder
 *     = Beta of int * tyc list * tkind list
 *     | Lamb of int * tkind list
 *
 *   type tycEnv = teBinder list
 *
 * Invariant: a tycEnv cannot terminate with a Lamb, i.e. the last binder
 *   in a tycEnv must be a Beta. tycEnvs are created when a closure is created
 *   when reducing a beta-redex (rule r1), and they are always initially of
 *   of the form Beta(0,args,ks)::nil.
 *)

datatype teBinder
  = Beta of int * tyc list * tkind list
      (* Beta(j,args,ks):
         created when reducing a beta redex (r1);
         j: the embedding level of the original redex -- 0 if the redex was
            created by r1, or the nesting level of the new closure if by r12;
         args: the tycs bound by the n-ary beta reduction, i.e. the arguments;
         ks: the operator domain kinds *)
  | Lamb of int * tkind list
      (* Lamb(j,ks):
         created when pushing a closure (Env) through a lambda (r10);
         j: the nesting level of the closure just before r10 is applied,
            i.e. the nesting level of the abstraction relative to the
            point where the closure was originally created;
         ks: the kinds of the abstraction parameters *)

val teEmpty : tycEnv = tc_injX(TC_SUM[])

(** utility functions for manipulating tycEnvs and teBinders **)

(* encoding teBinders as tycs:
 * Beta(j,args,ks) <=> TC_FN(ks,TC_PROJ(TC_SEQ args, j))
 * Lamb(j,ks) <=> TC_PROJ(TC_FN(ks,TC_SUM[]), j)
 *)
fun teEncodeBinder (Beta(j,args,ks)) : tyc =
      tc_injX(TC_FN(ks,tc_injX(TC_PROJ(tc_injX(TC_SEQ args), j))))
  | teEncodeBinder (Lamb(j,ks)) =
      tc_injX(TC_PROJ(tc_injX(TC_FN(ks,tc_injX(TC_SUM[]))), j))

fun teDecodeBinder (tyc : tyc) : teBinder =
    case tc_outX(tyc)
     of TC_FN(ks,tyc') =>
          (case tc_outX tyc'
             of TC_PROJ(tyc'',j) =>
                  (case tc_outX tyc''
                     of TC_SEQ(args) => Beta(j,args,ks)
                      | _ => bug "teDecodeBinder")
              | _ => bug "teDecodeBinder")
      | TC_PROJ(tyc',j) =>
          (case tc_outX tyc'
             of TC_FN(ks,_) => Lamb(j, ks)
              | _ => bug "teDecodeBinder")
      | _ => bug "teDecodeBinder"

fun teCons (b: teBinder, tenv: tycEnv) : tycEnv =
    tc_injX(TC_PARROW(teEncodeBinder b, tenv))

fun teDest (tenv: tycEnv) : (teBinder * tycEnv) option =
    case tc_outX tenv
     of TC_PARROW(b,tenv) => SOME(teDecodeBinder b, tenv)
      | TC_SUM [] => NONE
      | _ => bug "teDest"

fun teToBinders (tenv: tycEnv) =
    case teDest tenv
     of NONE => []
      | SOME(binder, tenvRest) => binder::(teToBinders tenvRest)

(* teLookup: tenv * int -> teBinder option
 * 1-based index lookup: assume i >= 1, return NONE if i > "length" of tenv *)
fun teLookup(tenv : tycEnv, i: int) : teBinder option =
      (case teDest tenv
        of SOME(binder, tenv') =>
             if i > 1 then teLookup(tenv',i-1)
             else if i = 1 then SOME binder
             else bug "index 0 in tycEnvLookup"
         | NONE => NONE)

fun teLength (tenv: tycEnv) : int =
    case teDest tenv
      of NONE => 0
       | SOME(_,tenv') => 1 + teLength tenv'


(***************************************************************************
 *            UTILITY FUNCTIONS ON TKIND ENVIRONMENT                       *
 ***************************************************************************)
(** tkind environment: maps each tyvar, i.e., its debindex, to its kind *)
type tkindEnv = tkind list list

(** utility functions for manipulating the tkindEnv *)
exception tkUnbound
val initTkEnv : tkindEnv = []

fun tkLookup (kenv, i, j) =
  let val ks = List.nth(kenv, i-1) handle Subscript => raise tkUnbound
   in List.nth(ks, j) handle Subscript => raise tkUnbound
  end

fun tkInsert (kenv, ks) = ks::kenv


(** testing the "pointer" equality on normalized tkind, tyc, and lty *)
fun tk_eq (x: tkind, y) = (x = y)

local
  fun stripIND tyc =
      (case tc_outX tyc
	of (TC_IND(new,_)) => stripIND new
	 | _ => tyc)

  fun verify(ref(_, TC_IND _, AX_REG(true,_,_))) =  bug "TC_IND is norm?!"
    | verify _ = ()
in
  fun tc_eq (x: tyc, y: tyc) =
      (verify x; verify y; x = y)
end

local
  fun verify(ref(_, LT_IND _, AX_REG(true,_,_))) =  bug "LT_IND is norm?!"
    | verify(ref(_, LT_TYC(ref(_,TC_IND _,_)), AX_REG(true,_,_))) =
        bug "LT_TYC (TC_IND) is norm?!"
    | verify(ref(_, _, AX_REG(true, _, _))) = ()
    | verify(ref(_, _, AX_REG(false, _, _))) =
        bug "Non-normalized (AX_REG false).\n"
    | verify(ref(_, _, AX_NO)) = bug "Non-normalized (AX_NO)\n"
in
  fun lt_eq (x: lty, y: lty) =
      (* ASSERT: x and y are in normal form *)
      (verify x; verify y;
       x = y)
end
(*
	     (case (tc_outX tyc1, tc_outX tyc2)
	       of (TC_PRIM pt1, TC_PRIM pt2) => print "PRIM\n"
		| (TC_FN _, _) => print "FN\n"
		| (TC_FIX _, _) => print "FIX\n"
		| (TC_VAR _, _) => print "VAR\n"
		| (TC_NVAR _, _) => print "NVAR\n"
		| (TC_APP _, _) => print "APP\n"
		| (TC_SEQ _, _) => print "SEQ\n"
		| (TC_PROJ _, _) => print "PROJ\n"
		| (TC_SUM _, _) => print "SUM\n"
		| (TC_TUPLE _, _) => print "TUPLE\n"
		| (TC_ARROW _, _) => print "ARROW\n"
		| (TC_PARROW _, _) => print "PARROW\n"
		| (TC_BOX _, _) => print "BOX\n"
		| (TC_ABS _, _) => print "ABS\n"
		| (TC_TOKEN _, _) => print "TOKEN\n"
		| (TC_CONT _, _) => print "CONT\n"
		| (TC_IND _, _) => print "IND\n"
		| (TC_ENV _, _) => print "ENV\n"
		| (TC_PRIM _, tyc2') =>
		    (print "unmatched PRIM\n";
		     case tyc2'
		      of (TC_FN _) =>
			 print "FN\n"
		       | (TC_FIX _) => print "FIX\n"
		       | (TC_VAR _) => print "VAR\n"
		       | (TC_NVAR _) => print "NVAR\n"
		       | (TC_APP _) => print "APP\n"
		       | (TC_SEQ _) => print "SEQ\n"
		       | (TC_PROJ _) => print "PROJ\n"
		       | (TC_SUM _) => print "SUM\n"
		       | (TC_TUPLE _) => print "TUPLE\n"
		       | (TC_ARROW _) => print "ARROW\n"
		       | (TC_PARROW _) => print "PARROW\n"
		       | (TC_BOX _) => print "BOX\n"
		       | (TC_ABS _) => print "ABS\n"
		       | (TC_TOKEN _) => print "TOKEN\n"
		       | (TC_CONT _) => print "CONT\n"
		       | (TC_IND _) => print "IND\n"
		       | (TC_ENV _) => print "ENV\n"
		       | (TC_PRIM _) => print "PRIM\n")
			 )); x = y)
    else (x = y) *)

(** utility functions for updating tycs and ltys *)
fun tyc_upd (tgt as ref(i : int, old : tycI, AX_NO), nt) =
      (tgt := (i, TC_IND (nt, old), AX_NO))
  | tyc_upd (tgt as ref(i : int, old : tycI, x as (AX_REG(false,_,_))), nt) =
      (tgt := (i, TC_IND (nt, old), x))
  | tyc_upd _ = bug "unexpected tyc_upd on already normalized tyc"

fun lty_upd (tgt as ref(i : int, old : ltyI, AX_NO), nt) =
      (tgt := (i, LT_IND (nt, old), AX_NO))
  | lty_upd (tgt as ref(i : int, old : ltyI, x as (AX_REG(false,_,_))), nt) =
      (tgt := (i, LT_IND (nt, old), x))
  | lty_upd _ = bug "unexpected lty_upd on already normalized lty"


(* tkSubkind returns true if k1 is a subkind of k2, or if they are
 * equivalent kinds.  it is NOT commutative.  tksSubkind is the same
 * thing, component-wise on lists of kinds.
 *)
fun tksSubkind (ks1, ks2) =
    ListPair.all tkSubkind (ks1, ks2)   (* component-wise *)

and tkSubkind (k1, k2) =
    tk_eq (k1, k2) orelse              (* reflexive *)
    case (tk_outX k1, tk_outX k2) of
        (TK_BOX, TK_MONO) => true (* ground kinds (base case) *)
      (* this next case is WRONG, but necessary until the
       * infrastructure is there to give proper boxed kinds to
       * certain tycons (e.g., ref : Omega -> Omega_b)
       *)
      | (TK_MONO, TK_BOX) => true
      | (TK_SEQ ks1, TK_SEQ ks2) =>
          tksSubkind (ks1, ks2)
      | (TK_FUN (ks1, k1'), TK_FUN (ks2, k2')) =>
          tksSubkind (ks2, ks1) andalso (* contravariant *)
          tkSubkind (k1', k2')
      | _ => false

(* TODO
 * There must be a better factoring of the dependencies
 * These functions are in either ltydefs or ltybasic *)
(** tkind constructors *)
val tkc_mono   : tkind = tk_injX (TK_MONO)
val tkc_box    : tkind = tk_injX (TK_BOX)
val tkc_seq    : tkind list -> tkind = tk_injX o TK_SEQ
val tkc_fun    : tkind list * tkind -> tkind = tk_injX o TK_FUN

(** tkind deconstructors *)
val tkd_mono   : tkind -> unit = fn _ => ()
val tkd_box    : tkind -> unit = fn _ => ()
val tkd_seq    : tkind -> tkind list = fn tk =>
      (case tk_outX tk of TK_SEQ x => x
                       | _ => bug "unexpected tkind in tkd_seq")
val tkd_fun    : tkind -> tkind list * tkind = fn tk =>
      (case tk_outX tk of TK_FUN x => x
                       | _ => bug "unexpected tkind in tkd_fun")

(** tkind predicates *)
val tkp_mono   : tkind -> bool = fn tk => tk_eq(tk, tkc_mono)
val tkp_box    : tkind -> bool = fn tk => tk_eq(tk, tkc_box)
val tkp_seq    : tkind -> bool = fn tk =>
      (case tk_outX tk of TK_SEQ _ => true | _ => false)
val tkp_fun    : tkind -> bool = fn tk =>
      (case tk_outX tk of TK_FUN _ => true | _ => false)

(** tkind one-arm switches *)
fun tkw_mono (tk, f, g) = if tk_eq(tk, tkc_mono) then f () else g tk
fun tkw_box (tk, f, g) = if tk_eq(tk, tkc_box) then f () else g tk
fun tkw_seq (tk, f, g) =
      (case tk_outX tk of TK_SEQ x => f x | _ => g tk)
fun tkw_fun (tk, f, g) =
      (case tk_outX tk of TK_FUN x => f x | _ => g tk)

(** utility functions for constructing tkinds *)
fun tkc_arg n =
  let fun h (n, r) = if n < 1 then r else h(n-1, tkc_mono::r)
   in h(n, [])
  end

val tkc_fn1 = tkc_fun(tkc_arg 1, tkc_mono)
val tkc_fn2 = tkc_fun(tkc_arg 2, tkc_mono)
val tkc_fn3 = tkc_fun(tkc_arg 3, tkc_mono)

fun tkc_int 0 = tkc_mono
  | tkc_int 1 = tkc_fn1
  | tkc_int 2 = tkc_fn2
  | tkc_int 3 = tkc_fn3
  | tkc_int i = tkc_fun(tkc_arg i, tkc_mono)

(* is a kind monomorphic? *)
fun tkIsMono k = tkSubkind (k, tkc_mono)

end (* structure Lty *)
