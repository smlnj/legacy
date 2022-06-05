(* Copyright 1989 by AT&T Bell Laboratories *)
(* environ.sml *)

structure Environment: ENVIRONMENT =
struct

local structure A = Access
      structure S  = Symbol
      structure M  = Modules
      structure V = VarCon
      structure T = Types
      structure MU = ModuleUtil
      structure B  = Bindings
      structure SE = StaticEnv
      structure DE = DynamicEnv
      structure SY = SymbolicEnv
      structure PP = PrettyPrint
in

type symbol = S.symbol
type staticEnv = SE.staticEnv
type dynenv  = DE.env
type symenv = SY.env

type environment = { static: staticEnv, dynamic: dynenv, symbolic: symenv }

fun bug msg = ErrorMsg.impossible("Environment: "^msg)

fun staticPart (e: environment) = #static e
fun dynamicPart (e: environment) = #dynamic e
fun symbolicPart (e: environment) = #symbolic e
      
fun mkenv (e as { static, dynamic, symbolic }) = e

val emptyEnv = {static   = SE.empty,
		dynamic  = DE.empty,
		symbolic = SY.empty}

fun layerEnv({static, dynamic, symbolic},
	       {static=sta, dynamic=dy, symbolic=sy}) =
      {static =  SE.atop (static, sta),
       dynamic = DE.atop (dynamic, dy),
       symbolic = SY.atop (symbolic, sy)}
  
val layerStatic = SE.atop
val layerSymbolic = SY.atop
  
fun consolidateEnv ({ static, dynamic, symbolic }) =
      {static = SE.consolidate static,
       dynamic = DE.consolidate dynamic,
       symbolic = SY.consolidate symbolic}

val consolidateStatic = SE.consolidate
val consolidateSymbolic = SY.consolidate

fun root(A.EXTERN pid) = SOME pid 
  | root(A.PATH(p,i)) = root p
  | root _ = NONE

(* getting the stamp from a binding *)
fun stampOf(B.VALbind (V.VALvar {access=a, ...})) = root a
  | stampOf(B.CONbind (T.DATACON {rep=A.EXN a, ...})) = root a
  | stampOf(B.STRbind (M.STR { access, ... })) = root access
  | stampOf(B.FCTbind (M.FCT { access, ... })) = root access
  | stampOf _ = NONE

(* functions to collect stale dynamic pids for unbinding in concatEnv *)

(* 
 * stalePids: takes a new environment and a base environment to which
 * it is to be added and returns a list of pids that are unreachable 
 * when the new environment is added to the base environment
 *
 * what we do instead:
 *  - count the number of occurences for each pid in baseEnv bindings
 *    that is going to be shadowed by deltaEnv
 *  - count the total number of total occurences for each such
 *    pids in baseEnv
 *  - the ones where the counts coincide are stale
 *
 * This code is ok, because deltaEnv is the output of `export'.  `export'
 * calls consolidateStatic, therefore we don't have duplicate bindings
 * of the same symbol.
 *)
fun stalePids (deltaEnv, baseEnv) = 
  let 

      (* any rebindings? *)
      val anyrebound = ref false

      (* counting map *)
      val countM = ref (PersMap.empty: int ref PersMap.map)
      fun look s = PersMap.find (!countM, s)

      (* initialize the counter map: for each new binding with stamp
       * check if the same symbol was bound in the old env and enter
       * the old stamp into the map *)
      fun initOne s =
        case look s 
         of NONE => countM := PersMap.insert (!countM, s, ref (~1))
          | SOME r => r := (!r) - 1

      fun initC (sy, _) =
	  (case stampOf (SE.look (baseEnv, sy))
	     of NONE => ()
	      | SOME s => (initOne s; anyrebound := true))
	  handle SE.Unbound => ()
      (* increment counter for a given stamp *)
      fun incr NONE = ()
	| incr (SOME s) = 
 	   case look s 
             of NONE => ()
 	      | SOME r => r := (!r) + 1

      fun incC (_, b) = incr (stampOf b)
      (* select the 0s *)
      fun selZero ((s, ref 0), zeros) = s :: zeros
	| selZero (_, zeros) = zeros
   in
      SE.app initC deltaEnv;		(* init counter map *)
      if !anyrebound then let		(* shortcut if no rebindings *)
	  (* count the pids *)
	  val _ = SE.app incC baseEnv
	  (* pick out the stale ones *)
	  val stalepids = foldl selZero [] (PersMap.listItemsi (!countM))
      in
	  stalepids
      end
      else []
  end

fun concatEnv ({ static = newstat, dynamic = newdyn, symbolic = newsym },
		 { static = oldstat, dynamic = olddyn, symbolic = oldsym }) =
  let val hidden_pids = stalePids (newstat, oldstat)
      val slimdyn = DE.remove (hidden_pids, olddyn)
      val slimsym = SY.remove (hidden_pids, oldsym)
   in {static=SE.consolidateLazy(SE.atop(newstat, oldstat)),
       dynamic=DE.atop(newdyn, slimdyn),
       symbolic=SY.atop(newsym, slimsym)}
  end

fun getbindings(static: staticEnv, symbols: S.symbol list) :
        (S.symbol * B.binding) list =
  let fun loop([], bindings) = bindings
        | loop(s::rest, bindings) =
            let val bindings' = (s,SE.look(static,s)) :: bindings
				  handle SE.Unbound => bindings
	     in loop (rest, bindings') 
            end
   in loop(symbols,[])
  end

fun copystat([], senv) = senv
  | copystat((s,b)::l, senv) = copystat(l,SE.bind(s, b, senv))

(*
fun filterStaticEnv(static: staticEnv, symbols: S.symbol list) : staticEnv =
      copystat(getbindings(static, symbols), SE.empty)
*)

local
    fun copydynsym (bindings, dynamic, symbolic) = let
	fun loop ([], denv, syenv) = (denv, syenv)
	  | loop ((_, b) :: l, denv, syenv) =
	    (case stampOf b
		 of NONE => loop (l, denv, syenv)
	       | SOME pid =>
		     let val dy = valOf (DE.look dynamic pid)
			 val denv = DE.bind (pid, dy, denv)
			 val sy = SY.look symbolic pid
			 val syenv = case sy
			     of NONE => syenv
			   | SOME sy => SY.bind (pid, sy, syenv)
		     in loop (l, denv, syenv)
		     end)
    in
	loop (bindings, DE.empty, SY.empty)
    end
in
    fun filterEnv({static, dynamic, symbolic}: environment, symbols) =
	let val sbindings = getbindings (static, symbols)
	    val senv = copystat(sbindings, SE.empty) 
	    val (denv, syenv) = copydynsym(sbindings, dynamic, symbolic)
	in {static =senv, dynamic = denv, symbolic = syenv}
	end

    fun trimEnv { static, dynamic, symbolic } = let
	val syms = BrowseStatEnv.catalog static
	val (dynamic, symbolic) =
	    copydynsym (getbindings (static, syms), dynamic, symbolic)
    in
	{ static = static, dynamic = dynamic, symbolic = symbolic }
    end
end

fun describe static (s: symbol) : unit =
      PP.with_default_pp
	  (fn ppstrm =>
	    (PP.openHVBox ppstrm (PP.Rel 0);
	      PPModules.ppBinding ppstrm
	        (s, SE.look(static,s), static, !Control.Print.printDepth);
	      PP.newline ppstrm;
	     PP.closeBox ppstrm))
      handle SE.Unbound => print (S.name s ^ " not found\n")

val primEnv = PrimEnv.primEnv

end (* local *)
end (* structure Environment *)
