(* spill-old.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: this code has been superseded by the implementation in spill-new.sml.
 * We have removed the RK_SPILL record kind from CPS, so it will no longer
 * compile.
 *)

signature SPILL = sig
  val spill : CPS.function list -> CPS.function list
end (* signature SPILL *)

functor SpillFn (MachSpec : MACH_SPEC) : SPILL = struct

local
  open CPS
  structure LV = LambdaVar
  val error = ErrorMsg.impossible
  val pr = Control.Print.say
  val maxgpfree = MachSpec.spillAreaSz div (2 * MachSpec.valueSize)
  val maxfpfree = MachSpec.spillAreaSz div (2 * MachSpec.realSize)
  val spillname = Symbol.varSymbol "spillrec"
  fun sortp x = ListMergeSort.sort (fn ((i:int,_),(j,_)) => i>j) x
  val app2 = ListPair.app
  val unboxedfloat = MachSpec.unboxedFloats
  structure CGoptions = Control.CG

in

(*****************************************************************************
 *                     MISC AND UTILITY FUNCTIONS                            *
 *****************************************************************************)
fun enter(new:int,l) =
  let fun f [] = [new]
	| f (l as h::t) = if new<h then new::l else if new>h then h::f t else l
  in  f l
  end

fun uniq l =
    let fun loop([],acc) = acc
	  | loop(a::r,acc) = loop(r,enter(a,acc))
    in loop(l,[])
    end

fun merge(a,[]) = a
  | merge([],a) = a
  | merge(l as (i:int)::a, m as j::b) =
      if j<i then j::merge(l,b) else i::merge(a,if i<j then m else b)

local fun loop (a::b::rest) = loop(merge(a,b)::loop rest)
        | loop l = l
in fun foldmerge l = hd(loop l) handle Hd => []
end

fun rmv (x : int,l) =
    let fun loop nil = nil
	  | loop (a::b) = if x=a then b else a::loop b
    in loop l
    end

fun member l (e:int) =
  let fun f [] = false
	| f (h::t) = if h<e then f t else e=h
  in  f l
  end

fun intersect(nil,_) = nil
  | intersect(_,nil) = nil
  | intersect(l as (a:int)::b,r as c::d) =
	if a=c then a::intersect(b,d)
	else if a<c then intersect(b,r)
	else intersect(l,d)

nonfix before
val \/ = merge and /\ = intersect
infix 6 \/   infix 7 /\

fun enterV(VAR v,l) = enter(v,l)
  | enterV(_,l) = l

fun unzip l =
  let fun h((a,b)::l,r1,r2) = h(l,a::r1,b::r2)
        | h([],r1,r2) = (rev r1,rev r2)
   in h(l,[],[])
  end

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl [] = []
   in subl
  end

fun spillLvar() = LV.namedLvar spillname

fun sayv(VAR v) = pr(LV.lvarName v)
  | sayv(LABEL v) = pr("(L)" ^ LV.lvarName v)
  | sayv(INT i) = pr(Int.toString i)
  | sayv(REAL r) = pr r
  | sayv(STRING s) = (pr "\""; pr s; pr "\"")
  | sayv _ = error "sayv in spill.sml"

val vallist = PrintUtil.printClosedSequence("[",",","]") sayv

fun cut(0,_) = []
  | cut(i,a::x) = a::cut(i-1,x)
  | cut(_,[]) = []

fun nextuse x =
 let val infinity = 1000000
     fun xin[] = false | xin(VAR y::r) = x=y orelse xin r | xin(_::r) = xin r
     fun g(level,a) =
     let val rec f =
      fn ([],[]) => infinity
       | ([],next) => g(level+1,next)
       | (SWITCH(v,c,l)::r,next) => if xin[v] then level else f(r,l@next)
       | (RECORD(_,l,w,c)::r,next) =>
	    if xin(map #1 l) then level else f(r,c::next)
       | (SELECT(i,v,w,_,c)::r,next) => if xin[v] then level else f(r,c::next)
       | (OFFSET(i,v,w,c)::r,next) => if xin[v] then level else f(r,c::next)
       | (SETTER(i,a,c)::r,next) => if xin a then level else f(r,c::next)
       | (LOOKER(i,a,w,_,c)::r,next) => if xin a then level else f(r,c::next)
       | (ARITH(i,a,w,_,c)::r,next) => if xin a then level else f(r,c::next)
       | (PURE(i,a,w,_,c)::r,next) => if xin a then level else f(r,c::next)
       | (BRANCH(i,a,c,e1,e2)::r,next) =>
			   if xin a then level else f(r,e1::e2::next)
       | (APP(v,vl)::r,next) => if xin(v::vl) then level else f(r,next)
       | _ => error "next use in spill.sml"
     in f(a,[])
     end
     fun h y = g(0,[y])
  in h
 end

fun sortdups(cexp,dups) =
  map #2 (sortp (map (fn dup as (v,w) => (nextuse v cexp, dup)) dups))

fun next_n_dups(0,cexp,dups) = []
  | next_n_dups(n,cexp,dups) =
      if (n >= length dups) then dups else cut(n,sortdups(cexp,dups))

fun clean l =
  let fun vars(l, VAR x :: rest) = vars(enter(x,l), rest)
        | vars(l, _::rest) = vars(l,rest)
        | vars(l, nil) = l
   in vars([], l)
  end

fun partition f l =
  foldr (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b)) ([], []) l


(***************************************************************************
 *   MAIN FUNCTION   spillit : CPS.function -> CPS.function                *
 ***************************************************************************)

exception SpillCtyMap
val ctymap : cty IntHashTable.hash_table = IntHashTable.mkTable(32,SpillCtyMap)
fun clearCtyMap() = IntHashTable.clear ctymap
fun getty v = getOpt (IntHashTable.find ctymap v, BOGt)
val addty = IntHashTable.insert ctymap
fun copyLvar v = let val p = (LV.dupLvar v, getty v) in addty p; p end
fun floatP v = case (getty v) of FLTt => true | _ => false

datatype spillkind = GPRSPILL | FPRSPILL
type spillinfo = (lvar list * value) option

fun spillit (fkind,func,vl,cl,body,skind) = let

val (varsP, nvarP, varLen, rkind, sregN) =
  case skind
   of FPRSPILL => (sublist floatP, not o floatP, maxfpfree, RK_RAW64BLOCK, 0)
    | GPRSPILL =>
        (if unboxedfloat then
              (sublist (not o floatP), floatP, maxgpfree, RK_SPILL, 1)
         else (fn x => x, fn _ => false, maxgpfree, RK_SPILL, 1))

val _ = clearCtyMap()
val _ = app2 addty (vl,cl)
val freevars =
 let exception SpillFreemap
     val m = IntHashTable.mkTable(32, SpillFreemap)
	     : lvar list IntHashTable.hash_table
     val _ = FreeMap.freemap (IntHashTable.insert m) body
  in fn x => ((IntHashTable.lookup m x) handle SpillFreemap =>
                    (pr "compiler bugs in spill.sml:  ";
                     (pr o Int.toString) x; pr "  \n";
                     raise SpillFreemap))
 end

(* INVARIANT: results, uniques have already been sifted through varsP *)
fun f(results : lvar list, uniques : lvar list, dups : (lvar*lvar) list,
      spill : spillinfo, cexp : cexp) =
  let val (before,after) =  (* variables free in this operation, and after
	  		       not including the newly-bound variables *)
	 let val rec free =
	      fn SWITCH(v,_,l) => foldmerge(clean[v] :: map free l)
	       | RECORD(_,l,w,c) =>  clean (map #1 l) \/ freevars w
	       | SELECT(i,v,w,_,c) => clean[v] \/ freevars w
	       | OFFSET(i,v,w,c) => clean[v] \/ freevars w
	       | SETTER(i,vl,c) => clean vl \/ free c
	       | LOOKER(i,vl,w,_,c) => clean vl \/ freevars w
	       | ARITH(i,vl,w,_,c) => clean vl \/ freevars w
	       | PURE(i,vl,w,_,c) => clean vl \/ freevars w
	       | BRANCH(i,vl,c,c1,c2) => clean vl \/ free c1 \/ free c2
	       | APP(f,vl) => clean(f::vl)
               | _ => error "free in spill 232"
	  in case cexp
	      of SWITCH(v,_,l) => (clean[v], foldmerge(map free l))
	       | RECORD(_,l,w,c) =>  (clean(map #1 l), freevars w)
	       | SELECT(i,v,w,_,c) => (clean[v], freevars w)
	       | OFFSET(i,v,w,c) => (clean[v], freevars w)
	       | SETTER(i,vl,c) => (clean vl, free c)
	       | LOOKER(i,vl,w,_,c) => (clean vl, freevars w)
	       | ARITH(i,vl,w,_,c) => (clean vl, freevars w)
	       | PURE(i,vl,w,_,c) => (clean vl, freevars w)
	       | BRANCH(i,vl,c,c1,c2) => (clean vl, free c1 \/ free c2)
	       | APP(f,vl) => (clean(f::vl), [])
               | _ => error "free in spill 233"
	 end

      val (before,after) = (varsP before, varsP after)
      val uniques = uniques \/ results (* is this line necessary? *)
      val uniques_after = uniques /\ after
      val uniques_before = (uniques /\ before) \/ uniques_after
      val spill_after =
        (case spill
          of NONE => NONE
           | SOME(contents,_) =>
               (case (uniq contents) /\ after of [] => NONE
                                               | _ => spill))

      val maxfree' = case spill of NONE => varLen
                                 | _ => varLen-sregN
      val avail = maxfree' - length(uniques_before \/ results)
      val dups = next_n_dups(avail,cexp,dups)

      val maxfreeafter = case spill_after of NONE => varLen
                                           | SOME _ => varLen-sregN

      fun getpath (VAR v) =
           if (member uniques_before v) orelse (nvarP v) then (VAR v, OFFp 0)
           else let fun find(i,w::l,sv) =
                          if (v=w) then (sv, SELp(i,OFFp 0))
                          else find(i+1,l,sv)
                      | find _ = error "not found in spill 001"

                    fun try((w,x)::l) = if v=w then (VAR x, OFFp 0) else try l
		      | try [] = (case spill
                                   of SOME(l,sv) => find(0,l,sv)
                                    | _ => error "not found in spill 002")

	         in try dups
	        end
	| getpath x = (x, OFFp 0)

      fun makeSpillRec args = (* args are already sift-ed *)
	let val contents = args \/ after
            val spillrec = map (getpath o VAR) contents
            val sv = spillLvar()
            val spinfo = SOME(contents,VAR sv)
	    val dups' = map (fn x => (x,x)) uniques_before @ dups
            val _ = CGoptions.spillGen := !CGoptions.spillGen + 1;
            val header = fn ce => RECORD(rkind,spillrec,sv,ce)
            val nce = f([],[],dups',spinfo,cexp)
 	 in header nce
	end

      (* here args and res are not sifted yet *)
      fun g(args,res,conts,temps,gen) =
        let val nargs = varsP (clean args)
            val nres = varsP (uniq res)
            val allargs = nargs \/ uniques_after
         in if ((length(allargs) + temps > maxfreeafter) orelse
                (length nres + length uniques_after + temps > maxfreeafter))
	    then makeSpillRec nargs
	    else let val paths =
                       map (fn x => (x, getpath (VAR x))) nargs
		     fun fetchit (_,(_,OFFp 0)) = false | fetchit _ = true
	          in case (sublist fetchit paths)
                      of (v,(w,SELp(i,OFFp 0)))::r =>
  		           let val (x,ct) = copyLvar v
                               val aftervars = case r of [] => spill_after
                                                       | _ => spill
 		            in (* pr "Fetching "; (pr o Int.toString) v;
		                  pr "\n"; *)
		               SELECT(i,w,x,ct,
                                        f([],uniques_before,(v,x)::dups,
                                          aftervars,cexp))
		           end
                       | _::r => error "unexpected access in g in spill"
	               | [] => let fun f' cexp = f(nres,uniques_after,
					       dups,spill_after,cexp)
		                in gen(map (#1 o getpath) args,
                                       res,map f' conts)
			       end
	         end
        end

   in case cexp
       of SWITCH(v,c,l) => g([v],[],l,0,fn([v],[],l)=>SWITCH(v,c,l))
        | RECORD(k,l,w,c) =>
	    if (sregN + length uniques_after > maxfreeafter)
	    then makeSpillRec(varsP(clean(map #1 l)))
	    else let val paths = map (fn (v,p) =>
					 let val (v',p') = getpath v
 					  in (v', combinepaths(p',p))
					 end) l
	          in RECORD(k,paths,w,
                       f(varsP [w],uniques_after,dups,spill_after,c))
		 end
        | SELECT(i,v,w,t,c) =>
            (addty(w,t); g([v],[w],[c],0,fn([v],[w],[c])=>SELECT(i,v,w,t,c)))
	| OFFSET(i,v,w,c) => g([v],[w],[c],0,fn([v],[w],[c])=>OFFSET(i,v,w,c))
	| SETTER(i,vl,c) => g(vl,[],[c],0,fn(vl,_,[c])=>SETTER(i,vl,c))
	| LOOKER(i,vl,w,t,c) =>
            (addty(w,t); g(vl,[w],[c],0, fn(vl,[w],[c])=>LOOKER(i,vl,w,t,c)))
	| ARITH(i,vl,w,t,c) =>
            (addty(w,t); g(vl,[w],[c],0, fn(vl,[w],[c])=>ARITH(i,vl,w,t,c)))
	| PURE(i,vl,w,t,c) =>
            (addty(w,t); g(vl,[w],[c],0, fn(vl,[w],[c])=>PURE(i,vl,w,t,c)))
	| RCC(p,vl,w,t,c) =>
	    (addty(w,t); g(vl,[w],[c],0, fn(vl,[w],[c])=>RCC(p,vl,w,t,c)))
        | BRANCH(i as P.streq,vl,c,c1,c2) =>
            g(vl,[],[c1,c2],sregN, fn(vl,_,[c1,c2])=>BRANCH(i,vl,c,c1,c2))
        | BRANCH(i as P.strneq,vl,c,c1,c2) =>
            g(vl,[],[c1,c2],sregN, fn(vl,_,[c1,c2])=>BRANCH(i,vl,c,c1,c2))
	| BRANCH(i,vl,c,c1,c2) =>
	    g(vl,[],[c1,c2],0, fn(vl,_,[c1,c2])=>BRANCH(i,vl,c,c1,c2))
	| APP(f,vl) => g(f::vl,[],[],0,fn(f::vl,[],[])=>APP(f,vl))
        | _ => error "spill 2394892"
  end

 in (fkind,func,vl,cl,f([],varsP(uniq vl),[],NONE,body))
end


(*****************************************************************************
 *              CHECK IF SPILLING IS NECESSARY                               *
 *****************************************************************************)
local
  exception TooMany
  exception FloatSet
  val floatset : bool IntHashTable.hash_table =
      IntHashTable.mkTable(32,FloatSet)
  fun fltM(v,FLTt) = IntHashTable.insert floatset (v,true)
    | fltM _ = ()
  fun fltP v = getOpt (IntHashTable.find floatset v, false)
  fun clearSet() = IntHashTable.clear floatset
  val dummyM = fn _ => ()
  val dummyP = fn _ => true
in

fun check((_,f,args,cl,body),skind) =
  let val (varM, varP, varLen) =
       (case skind
         of FPRSPILL => (fltM, fltP, maxfpfree)
          | GPRSPILL => if unboxedfloat then (fltM, not o fltP, maxgpfree)
                        else (dummyM, dummyP, maxgpfree))
      val _ = clearSet()
      val _ = app2 varM (args,cl)

      fun sift(l,vl) =
        let fun h((VAR x)::r,vl) =
                  if varP x then h(r,enter(x,vl)) else h(r,vl)
              | h(_::r,vl) = h(r,vl)
              | h([],vl) = vl
         in h(l,vl)
        end

      fun verify (w,vl) =
        let val nvl = rmv(w,vl)
         in if (length(nvl) >= varLen) then raise TooMany else nvl
        end

      val rec freevars =
        fn APP(v,args) => sift(v::args,[])
         | SWITCH(v,c,l) => sift([v],foldmerge(map freevars l))
         | SELECT(_,v,w,t,e) => (varM(w,t); sift([v],verify(w,freevars e)))
         | RECORD(_,l,w,e) => (sift((map #1 l),verify(w,freevars e)))
         | OFFSET(_,v,w,e) => (sift([v],verify(w,freevars e)))
         | SETTER(_,vl,e) => sift(vl,freevars e)
         | LOOKER(_,vl,w,t,e) => (varM(w,t); sift(vl,verify(w,freevars e)))
         | ARITH(_,vl,w,t,e) => (varM(w,t); sift(vl,verify(w,freevars e)))
         | PURE(_,vl,w,t,e) => (varM(w,t); sift(vl,verify(w,freevars e)))
	 | RCC(_,vl,w,t,e) => (varM(w,t); sift(vl,verify(w,freevars e)))
         | BRANCH(_,vl,c,e1,e2) => sift(vl,merge(freevars e1,freevars e2))
         | FIX _ => error "FIX in Freemap.freemap"

   in (freevars body; true) handle TooMany => false
  end

end (* local dec for the "check" function *)

(*****************************************************************************
 *  IMPROVE THE REGISTER USAGE BY SIMPLE RENAMING OF RECORD FIELDS           *
 *    (this procedure can be improved by reordering the cps expressions      *
 *     based on the lifetime of each variables; by doing this, we can avoid  *
 *     most of the big cluster of simultaneously live variables. --zsh)      *
 *****************************************************************************)
fun improve cexp =
  let exception Spillmap
      val m : (int ref*int*value) IntHashTable.hash_table =
	  IntHashTable.mkTable(32,Spillmap)
      val enter = IntHashTable.insert m
      val lookup = IntHashTable.lookup m
      fun get(VAR x) = (SOME(lookup x) handle Spillmap => NONE)
        | get _ = NONE
      fun kill(VAR v) = (ignore (IntHashTable.remove m v) handle _ => ())
        | kill _ = ()
      fun use v = case get v of SOME(r as ref 0,i,w) => r := 1
		              | SOME _ => kill v
		              | NONE => ()
      val rec pass1 =
	fn SELECT(i,v,w,_,e) => (kill v; enter(w,(ref 0,i,v)); pass1 e)
	 | OFFSET(i,v,w,e) => (kill v; pass1 e)
	 | RECORD(_,vl,w,e) => (app (use o #1) vl; pass1 e)
	 | APP(v,vl) => (kill v; app kill vl)
	 | FIX(l,e) => error "33832 in spill"
	 | SWITCH(v,_,el) => (kill v; app pass1 el)
	 | BRANCH(i,vl,c,e1,e2) => (app kill vl; pass1 e1; pass1 e2)
	 | SETTER(i,vl,e) => (app kill vl; pass1 e)
	 | LOOKER(i,vl,w,_,e) => (app kill vl; pass1 e)
	 | ARITH(i,vl,w,_,e) => (app kill vl; pass1 e)
	 | PURE(i,vl,w,_,e) => (app kill vl; pass1 e)
	 | RCC(p,vl,w,_,e) => (app kill vl; pass1 e)

      fun ren(v,p) = case get v of SOME(_,i,w) => (w,SELp(i,p))
			         | NONE => (v,p)

      val rec g =
        fn SELECT(i,v,w,t,e) =>
             (case get(VAR w) of SOME _ => g e
		               | NONE => SELECT(i,v,w,t,g e))
	 | OFFSET(i,v,w,e) => OFFSET(i,v,w, g e)
	 | RECORD(k,vl,w,e) => RECORD(k,map ren vl, w, g e)
	 | e as APP(v,vl) => e
	 | FIX(l,e) => error "33832 in spill"
	 | SWITCH(v,c,el) => SWITCH(v,c,map g el)
	 | BRANCH(i,vl,c,e1,e2) => BRANCH(i,vl,c, g e1, g e2)
	 | SETTER(i,vl,e) => SETTER(i,vl, g e)
	 | LOOKER(i,vl,w,t,e) => LOOKER(i,vl,w,t,g e)
	 | ARITH(i,vl,w,t,e) => ARITH(i,vl,w,t,g e)
	 | PURE(i,vl,w,t,e) => PURE(i,vl,w,t,g e)
	 | RCC(p,vl,w,t,e) => RCC(p,vl,w,t,g e)

      val count = (pass1 cexp; IntHashTable.numItems m)

      val _ = if (!CGoptions.debugcps) then
                (pr "count="; (pr o Int.toString) count; pr "\n")
              else ()
   in if count>0 then SOME(g cexp) else NONE
  end

(*****************************************************************************
 *                     THE EXPORTED "SPILL" FUNCTION                         *
 *****************************************************************************)
fun spillone arg =
  let val _ = (if (!CGoptions.printit)
               then (pr "^^^^^within the spill phase^^^^^^^^ \n";
                     PPCps.printcps0 arg;
                     pr "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ \n")
               else ())

      fun spillgpr (arg as (fkind,func,vl,cl,body)) =
        if check(arg,GPRSPILL) then arg
        else (if (!CGoptions.printit)
              then (pr "^^^^ need go through more rounds ^^^^ \n")
              else ();
              case improve body
               of SOME body' => spillgpr(fkind,func,vl,cl,body')
  	        | NONE => spillit(fkind,func,vl,cl,body,GPRSPILL))

      fun spillfpr (arg as (fkind,func,vl,cl,body)) =
        if check(arg,FPRSPILL) then spillgpr arg
        else (if (!CGoptions.printit)
              then (pr "^^^^ need go through more rounds ^^^^ \n")
              else ();
              case improve body
               of SOME body' => spillfpr(fkind,func,vl,cl,body')
                | NONE => spillgpr(spillit(fkind,func,vl,cl,body,FPRSPILL)))

   in if unboxedfloat then spillfpr arg
      else spillgpr arg
  end

val spill = map spillone

end (* local *)
end (* functor Spill *)

