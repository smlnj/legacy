(* resource-db.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure ResourceDB (* : sig

    exception BadSearchKey
    type rm_db_t
    val newRDB : unit -> rm_db_t
    val printRDB : (string * rm_db_t) -> unit

  end *) = struct

    exception BadSearchKey

    type name = string (* eventually, we will use hashed names *)

    datatype spec_binding = LOOSE | TIGHT

    datatype db_tbl = DB of hash_bucket list Array.array

    and hash_bucket = HB of {
	link : name option,		(* NONE corresponds to a "?" *)
	value : string option ref,
	tight : db_tbl option ref,
	loose : db_tbl option ref
      }

    datatype rm_db_t = RMDB of hash_bucket_t

    exception Found of string option
    fun return v = raise (Found v)

    val hashTblSz = 64 and hashTblMask = 63
    fun hash q = Bits.andb(hashQuark q, hashTblMask)

    fun newHashTbl () = HashTbl(Array.array(hashTblSz, []))
    fun newHB (q, v) = HB{quark=q, value=ref v, tight=ref NONE, loose=ref NONE}

(** NOTE: maybe this should be included in getEntry **)
    fun tblLook (NONE, _) = NONE
      | tblLook (SOME(HashTbl tbl), q) = let
	  fun look [] = NONE
	    | look ((h as HB{quark, ...}) :: r) =
		if (sameQuark(q, quark)) then (SOME h) else (look r)
	  in
	    look (Array.sub(tbl, hash q))
	  end (* tblLook *)

    fun putEntry (RMDB db, bindings, quarks, v) = let
	  fun findBucket (ref(SOME(HashTbl tbl)), q, v) = let
		val h = hash q
		val bucket = Array.sub(tbl, h)
		fun look [] = let val hb = newHB(q, v)
		      in
			Array.update(tbl, h, hb :: bucket);
			hb
		      end
		  | look ((hb as HB{quark, ...}) :: r) =
		      if (sameQuark(q, quark)) then hb else (look r)
		in
		  look bucket
		end
	    | findBucket (arg as (tblRef, _, _)) = (
		tblRef := SOME(newHashTbl());
		findBucket arg)
	  fun put (HB{loose, ...}, [LOOSE], [q]) = findBucket (loose, q, SOME v)
	    | put (HB{tight, ...}, [TIGHT], [q]) = findBucket (tight, q, SOME v)
	    | put (HB{loose, ...}, LOOSE::rb, q::rq) =
		put (findBucket (loose, q, NONE), rb, rq)
	    | put (HB{tight, ...}, TIGHT::rb, q::rq) =
		put (findBucket (tight, q, NONE), rb, rq)
	  in
	    put (db, bindings, quarks); ()
	  end (* putEntry *)

    fun getEntry (RMDB(HB{loose, tight, ...}), names, classes) = let
	  fun get (loose, tight, [name], [class]) = (
		  lookLeaf (tight, name);
		  lookLeaf (loose, name);
		  lookLeaf (tight, class);
		  lookLeaf (loose, class))
	    | get (loose, tight, name::names, class::classes) = let
		fun lookLoose ([name], [class]) = (
			lookLeaf(loose, name);
			lookLeaf(loose, class))
		  | lookLoose (name::names, class::classes) = (
			lookNode(loose, name, names, classes);
			lookNode(loose, class, names, classes))
		  | lookLoose _ = raise BadSearchKey
		in
		  lookNode (tight, name, names, classes);
		  lookNode (loose, name, names, classes);
		  lookNode (tight, class, names, classes);
		  lookNode (loose, class, names, classes);
		  lookLoose (names, classes)
		end
	    | get _ = raise BadSearchKey
	  and lookLeaf (tbl, q) = (case (tblLook(!tbl, q))
		 of SOME(HB{value, ...}) => return(!value)
		  | NONE => ())
	  and lookNode (tbl, q, names, classes) = (case (tblLook(!tbl, q))
		 of SOME(HB{loose, tight, ...}) => get(loose, tight, names, classes)
		  | NONE => ())
	  in
	    (get(loose, tight, names, classes); NONE) handle (Found v) => v
	  end

    fun emumerateRDB (RMDB(HB{loose, tight, ...}), f) = let
	  fun enumTbl (NONE, _, _) = ()
	    | enumTbl (SOME(HashTbl tbl), bindings, quarks) = let
		fun tblLoop i = if (i < hashTblSz)
		      then let
			fun buckLoop [] = ()
			  | buckLoop (HB{quark, value, loose, tight} :: r) = (
			      enumTbl (!tight, TIGHT::bindings, quark::quarks);
			      enumTbl (!loose, LOOSE::bindings, quark::quarks);
			      case (!value)
			       of (SOME v) => f (rev bindings, rev(quark::quarks), v)
				| NONE => ()
			      (* end case *);
			      buckLoop r)
			in
			  buckLoop (Array.sub(tbl, i));
			  tblLoop (i+1)
			end
		      else ()
		in
		  tblLoop 0
		end (* enumTbl *)
	  in
	    enumTbl(!tight, [TIGHT], []);
	    enumTbl(!loose, [LOOSE], [])
	  end (* emumerateRDB *)

    fun printRDB (fname, rdb) = let
	  val strm = CIO.open_out fname
	  val pr = CIO.outputc strm
	  fun prBindingList (bindings, quarks) = let
		fun loop ([], []) = ()
		  | loop (LOOSE::rb, q::rq) = (pr("*" ^ quarkToString q); loop(rb, rq))
		  | loop (TIGHT::rb, q::rq) = (pr("." ^ quarkToString q); loop(rb, rq))
		in
		  loop (bindings, quarks)
		end
(** NOTE: this won't work with embedded new-lines, etc... **)
	  fun prSpec (bindings, quarks, value) = (
		prBindingList(bindings, quarks);
		pr ":\t"; pr value; pr "\n")
	  in
	    emumerateRDB(rdb, prSpec);
	    CIO.close_out strm
	  end

  (* Create a new (empty) resource data-base *)
    fun newRDB () = RMDB(newHB (nullQuark, NONE))

  end (* XResourceDB *)
