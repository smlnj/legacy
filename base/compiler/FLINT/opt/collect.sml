(* collect.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* monnier@cs.yale.edu *)

signature COLLECT =
sig
    type info

    (* Collect information about variables and function uses.
     * The info is accumulated in the map `m' *)
    val collect : FLINT.fundec -> FLINT.fundec

    val get : FLINT.lvar -> info

    (* query functions *)
    val escaping  : info -> bool	(* non-call uses *)
    val called    : info -> bool	(* known call uses *)
    val dead      : info -> bool	(* usenb = 0 ? *)
    val usenb     : info -> int		(* total nb of uses *)
    val callnb    : info -> int		(* total nb of calls *)

    (* self-referential (i.e. internal) uses *)
    val iusenb    : info -> int
    val icallnb   : info -> int
    (* reset to safe values (0 and 0) *)
    val ireset    : info -> unit

    (* inc the "true=call,false=use" count *)
    val use    : bool -> info -> unit
    (* dec the "true=call,false=use" count and return true if zero *)
    val unuse  : bool -> info -> bool
    (* transfer the counts of var1 to var2 *)
    val transfer : FLINT.lvar * FLINT.lvar -> unit
    (* add the counts of var1 to var2 *)
    (*  val addto  : info * info -> unit *)
    (* delete the last reference to a variable *)
    (*  val kill   : FLINT.lvar -> unit *)
    (* create a new var entry (SOME arg list if fun) initialized to zero *)
    val new    : FLINT.lvar list option -> FLINT.lvar -> info

    (* when creating a new var.  Used when alpha-renaming *)
    (* val copy   : FLINT.lvar * FLINT.lvar -> unit *)

    (* fix up function to keep counts up-to-date when getting rid of code.
     * the arg is called for *free* variables becoming dead. *)
    val unuselexp : (FLINT.lvar -> unit) -> FLINT.lexp -> unit
    (* function to collect info about a newly created lexp *)
    val uselexp : FLINT.lexp -> unit
    (* function to copy (and collect info) a lexp *)
    val copylexp : FLINT.lvar LambdaVar.Map.map  -> FLINT.lexp -> FLINT.lexp

    (* mostly useful for PPFlint *)
    val LVarString : FLINT.lvar -> string
end

(* Internal vs External references:
 * I started with a version that kept track separately of internal and external
 * uses.  This has the advantage that if the extuses count goes to zero, we can
 * consider the function as dead.  Without this, recursive functions can never
 * be recognized as dead during fcontract (they are still eliminated at the
 * beginning, tho).  This looks nice at first, but poses problems:
 * - when you do simple inlining (just moving the body of the procedure), you
 *   may inadvertently turn ext-uses into int-uses.  This only happens when
 *   inlining mutually recursive function, but this can be commen (think of
 *   when fcontract undoes a useless uncurrying of a recursive function).  This
 *   can be readily overcome by not using the `move body' optimization in
 *   dangerous cases and do the full copy+kill instead.
 * - you have to keep track of what is inside what.  The way I did it was to
 *   have an 'inside' ref cell in each fun.  That was a bad idea.  The problem
 *   stems from the fact that when you detect that a function becomes dead,
 *   you have to somehow reset those `inside' ref cells to reflect the location
 *   of the function before you can uncount its references.  In most cases, this
 *   is unnecessary, but it is necessary when undertaking a function mutually
 *   recursive with a function in which you currently are when you detect the
 *   function's death.
 * rather than fix this last point, I decided to give up on keeping internal
 * counts up-to-date.  Instead, I just compute them once during collect and
 * never touch them again:  this means that they should not be relied on in
 * general.  More specifically, they become potentially invalid as soon as
 * the body of the function is changed.  This still allows their use in
 * many cases.
 *)

structure Collect :> COLLECT =
struct
local
    structure F  = FLINT
    structure M  = LambdaVar.Tbl
    structure FU = FlintUtil
    structure LV = LambdaVar
    structure PP = PPFlint
    structure PO = Primop
in

val say = Control_Print.say
fun bug msg = ErrorMsg.impossible ("Collect: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; say " "; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; say " "; bug msg)

datatype info
  (* we keep track of calls and escaping uses *)
  = Info of {calls: int ref, uses: int ref, int: (int * int) ref}

exception NotFound

val m : info M.hash_table = M.mkTable(128, NotFound)

fun new args lv =
    let val i = Info{uses=ref 0, calls=ref 0, int=ref(0,0)}
    in M.insert m (lv, i); i
    end

(* map related helper functions *)
fun get lv = (M.lookup m lv)
		  handle x as NotFound =>
		  ((* say ("Collect: ERROR: get unknown var "^
			(LV.lvarName lv)^
			". Pretending dead...\n"); *)
		   (*  raise x; *)
		   new NONE lv)

fun LVarString lv =
    let val Info{uses=ref uses,calls=ref calls,...} = get lv
    in (LV.lvarName lv)^
	"{"^(Int.toString uses)^
	(if calls > 0 then ","^(Int.toString calls) else "")^"}"
    end

(* adds the counts of lv1 to those of lv2 *)
fun addto (Info{uses=uses1,calls=calls1,...},Info{uses=uses2,calls=calls2,...}) =
    (uses2 := !uses2 + !uses1; calls2 := !calls2 + !calls1)

fun transfer (lv1,lv2) =
    let val i1 = get lv1
	val i2 = get lv2
    in addto(i1, i2);
	(* note the transfer by redirecting the map *)
	M.insert m (lv1, i2)
    end

fun inc ri = (ri := !ri + 1)
fun dec ri = (ri := !ri - 1)

(* - first list is list of formal args
 * - second is list of `up to know known arg'
 * - third is args of the current call. *)
fun mergearg (NONE, _) = NONE
  | mergearg (SOME(fv, NONE), a) =
      if FU.valueIsVar fv a then SOME(fv, NONE) else SOME(fv, SOME a)
  | mergearg (SOME(fv, SOME b), a) =
      if FU.sameValue(a, b) orelse FU.valueIsVar fv a
	then SOME(fv, SOME b)
	else NONE

fun use call (Info{uses,calls,...}) = (inc uses; if call then inc calls else ())

fun unuse call (Info{uses,calls,...}) =
    (* notice the calls could be dec'd to negative values because a
     * use might be turned from escaping to known between the census
     * and the unuse.  We can't easily detect such changes, but
     * we can detect it happened when we try to go below zero. *)
    (dec uses;
     if (call (*  andalso !calls > 0 *)) then dec calls
     else ();
     if !uses < 0 then bug "decrementing too much" (* F.VAR lv) *)
     else !uses = 0)

fun usenb (Info{uses=ref uses,...}) = uses
fun callnb (Info{calls=ref calls,...}) = calls
fun used (Info{uses,...}) = !uses > 0
fun dead (Info{uses,...}) = !uses = 0
fun escaping (Info{uses,calls,...}) = !uses > !calls
fun called (Info{calls,...}) = !calls > 0
fun iusenb (Info{int=ref(u,_),...}) = u
fun icallnb (Info{int=ref(_,c),...}) = c
fun ireset (Info{int,...}) = int := (0,0)

(* Ideally, we should check that usenb = 1, but we may have been a bit
 * conservative when keeping the counts uptodate *)
fun kill lv = ignore (M.remove m lv) handle _ => ()

(* ********************************************************************** *)
(* ********************************************************************** *)

datatype usage
  = All
  | None
  | Some of bool list

fun usage bs =
    let fun ua [] = All
	  | ua (false::_) = Some bs
	  | ua (true::bs) = ua bs
	fun un [] = None
	  | un (true::_) = Some bs
	  | un (false::bs) = un bs
    in case bs
	of true::bs => ua bs
	 | false::bs => un bs
	 | [] => None
    end

fun impurePO (po:F.primop) = PrimopUtil.effect (#2 po)

val census = let
    (* val use = if inc then use else unuse *)
    fun call lv = use true (get lv)
    val use = fn F.VAR lv => use false (get lv) | _ => ()
    fun newv lv = new NONE lv
    fun newf args lv = new args lv
    fun id x = x

    (* here, the use resembles a call, but it's safer to consider it as a use *)
    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(use (F.VAR default); app (use o F.VAR o #2) table)
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) = use (F.VAR lv)
      | cdcon _ = ()

    (* the actual function:
     * `uvs' is an optional list of booleans representing which of
     * the return values are actually used *)
    fun cexp lexp =
	case lexp
	 of F.RET vs => app use vs

	  | F.LET (lvs,le1,le2) =>
	    let val lvsi = map newv lvs
	    in cexp le2; cexp le1
	    end

	  | F.FIX (fs,le) =>
	    let val fs = map (fn (_,f,args,body) =>
			      (newf (SOME(map #1 args)) f,args,body))
			     fs
		fun cfun (Info{uses,calls, int, ...}, args,body) =
		    (* census of a fundec.  We get the internal counts
		     * by examining the count difference between before
		     * and after census of the body. *)
		    let val (euses,ecalls) = (!uses,!calls)
		    in
			app (fn (v,t) => ignore(newv v)) args;
			cexp body;
			int := (!uses - euses, !calls - ecalls)
		    end
		fun cfix fs =	(* census of a list of fundecs *)
		    let val (ufs,nfs) = List.partition (used o #1) fs
		    in if List.null ufs then ()
		       else (app cfun ufs; cfix nfs)
		    end
	    in cexp le; cfix fs
	    end

	  | F.APP (F.VAR f,vs) =>
	    (call f; app use vs)

	  | F.TFN ((tfk,tf,args,body),le) =>
	    let val tfi = newf (SOME[]) tf
	    in cexp le; if used tfi then cexp body else ()
	    end

	  | F.TAPP (F.VAR tf,tycs) => call tf

	  | F.SWITCH (v,cs,arms,def) =>
	    (use v; Option.map cexp def;
	     app (fn (F.DATAcon(dc,_,lv),le) => (cdcon dc; newv lv; cexp le)
		   | (_,le) => cexp le)
		 arms)

	  | F.CON (dc,_,v,lv,le) =>
	    let val lvi = newv lv
	    in cdcon dc; cexp le; if used lvi then use v else ()
	    end

	  | F.RECORD (_,vs,lv,le) =>
	    let val lvi = newv lv
	    in cexp le; if used lvi then app use vs else ()
	    end

	  | F.SELECT (v,_,lv,le) =>
	    let val lvi = newv lv
	    in cexp le; if used lvi then use v else ()
	    end

	  | F.RAISE (v,_) => use v
	  | F.HANDLE (le,v) => (use v; cexp le)

	  | F.BRANCH (po,vs,le1,le2) =>
	    (app use vs; cpo po; cexp le1; cexp le2)

	  | F.PRIMOP (po,vs,lv,le) =>
	    let val lvi = newv lv
	    in  cexp le;
		if used lvi orelse impurePO po then (cpo po; app use vs) else ()
	    end

	  | le => buglexp("unexpected lexp", le)
in
    cexp
end

(* The code is almost the same for uncounting, except that calling
 * undertaker should not be done for non-free variables.  For that we
 * artificially increase the usage count of each variable when it's defined
 * (accomplished via the "def" calls)
 * so that its counter never reaches 0 while processing its scope.
 * Once its scope has been processed, we can completely get rid of
 * the variable and corresponding info (after verifying that the count
 * is indeed exactly 1 (accomplished by the "kill" calls) *)
fun unuselexp undertaker = let
    (* val use = if inc then use else unuse *)
    fun uncall lv = if unuse true (get lv) then undertaker lv else ()
    val unuse = fn F.VAR lv => if unuse false (get lv) then undertaker lv else ()
		 | _ => ()
    fun def i = (use false i)
    fun id x = x

    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(unuse(F.VAR default); app (unuse o F.VAR o #2) table)
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) = unuse(F.VAR lv)
      | cdcon _ = ()

    fun cfun (args,body) = (* census of a fundec *)
	(app (def o get) args; cexp body; app kill args)

    and cexp lexp =
	case lexp
	 of F.RET vs => app unuse vs

	  | F.LET (lvs,le1,le2) =>
	    (app (def o get) lvs; cexp le2; cexp le1; app kill lvs)

	  | F.FIX (fs,le) =>
	    let val fs = map (fn (_,f,args,body) => (get f, f, args, body)) fs
		val usedfs = (List.filter (used o #1) fs)
	    in app (def o #1) fs;
		cexp le;
		app (fn (_,_,args,le) => cfun(map #1 args, le)) usedfs;
		app (kill o #2) fs
	    end

	  | F.APP (F.VAR f,vs) =>
	    (uncall f; app unuse vs)

	  | F.TFN ((tfk,tf,args,body),le) =>
	     let val tfi = get tf
	     in if used tfi then cexp body else ();
		 def tfi; cexp le; kill tf
	     end

	  | F.TAPP (F.VAR tf,tycs) => uncall tf

	  | F.SWITCH (v,cs,arms,default) =>
	    (unuse v; Option.map cexp default;
	     (* here we don't absolutely have to keep track of vars bound within
	      * each arm since these vars can't be eliminated anyway *)
	     app (fn (F.DATAcon(dc,_,lv),le) =>
		  (cdcon dc; def(get lv); cexp le; kill lv)
		   | (_,le) => cexp le)
		 arms)

	  | F.CON (dc,_,v,lv,le) =>
	    let val lvi = get lv
	    in cdcon dc; if used lvi then unuse v else ();
		def lvi; cexp le; kill lv
	    end

	  | F.RECORD (_,vs,lv,le) =>
	    let val lvi = get lv
	    in if used lvi then app unuse vs else ();
		def lvi; cexp le; kill lv
	    end

	  | F.SELECT (v,_,lv,le) =>
	    let val lvi = get lv
	    in if used lvi then unuse v else ();
		def lvi; cexp le; kill lv
	    end

	  | F.RAISE (v,_) => unuse v
	  | F.HANDLE (le,v) => (unuse v; cexp le)

	  | F.BRANCH (po,vs,le1,le2) =>
	    (app unuse vs; cpo po; cexp le1; cexp le2)

	  | F.PRIMOP (po,vs,lv,le) =>
	    let val lvi = get lv
	    in if used lvi orelse impurePO po
	       then (cpo po; app unuse vs)
	       else ();
	       def lvi; cexp le; kill lv
	    end

	  | le => buglexp("unexpected lexp", le)
in
    cexp
end

val uselexp = census
fun copylexp alpha le =
    let val nle = FU.copy [] alpha le
    in uselexp nle; nle
    end

fun collect (fdec as (_,f,_,_)) =
    ((*  say "Entering Collect...\n"; *)
     M.clear m;				(* start from a fresh state *)
     PP.LVarString := LVarString;
     uselexp (F.FIX([fdec], F.RET[F.VAR f]));
     (*  say "...Collect Done.\n"; *)
     fdec)

end
end

