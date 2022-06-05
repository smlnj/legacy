(* spill-table.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor SpillTable (MachSpec : MACH_SPEC) : sig

    val spillInit  : unit -> unit
    val getRegLoc  : RAGraph.spillLoc -> int
    val getFregLoc : RAGraph.spillLoc -> int

  end = struct

    structure G = RAGraph

    fun error msg = MLRiscErrorMsg.error(MachSpec.architecture^".SpillTable",msg)

    val itow = Word.fromInt

    exception RegSpills and FregSpills
    val spillOffset = ref MachSpec.initialSpillOffset
    val regspills : int G.SpillLocHashTable.hash_table =
	  G.SpillLocHashTable.mkTable(0, RegSpills)
    val fregspills : int G.SpillLocHashTable.hash_table =
	  G.SpillLocHashTable.mkTable(0, FregSpills)
    val findReg  = G.SpillLocHashTable.find regspills
    val enterReg   = G.SpillLocHashTable.insert regspills
    val findFreg = G.SpillLocHashTable.find fregspills
    val enterFreg  = G.SpillLocHashTable.insert fregspills

    fun spillInit() = (
	(* Reset the regspills/fregspills map by need. *)
	  if !spillOffset = MachSpec.initialSpillOffset
	    then ()
	    else (
	      G.SpillLocHashTable.clear regspills;
	      G.SpillLocHashTable.clear fregspills);
	  spillOffset := MachSpec.initialSpillOffset)

    fun newOffset offset = if offset >= MachSpec.spillAreaSz
	  then error "spill area too small"
	  else spillOffset := offset

  (* Get spill location for integer registers *)
    fun getRegLoc loc = (case findReg loc
	   of SOME offset => offset
	    | NONE => let
	        val offset = !spillOffset
		in
		  newOffset (offset + MachSpec.valueSize);
		  enterReg (loc, offset);
		  offset
		end
	  (* end case *))

(* REAL32: will float spills always be double precision? *)
  (* constants for aligning real spills *)
    val realSzMinus1 = MachSpec.realSize-1
    val realSzMask = itow(~MachSpec.realSize)

   (* Get spill location for floating point registers *)
    fun getFregLoc loc = (case findFreg loc
	   of SOME offset => offset
	    | NONE => let
	        val offset = !spillOffset
	        val aligned = Word.toIntX (Word.andb(itow(offset+realSzMinus1), realSzMask))
		in
		  newOffset (aligned + MachSpec.realSize);
		  enterFreg (loc, aligned);
		  aligned
		end
	  (* end case *))

  end
