signature X86STACKSPILLS = sig
  structure I : X86INSTR
  val init : unit -> unit
  val setAvailableOffsets : I.operand list -> unit
  val setAvailableFPOffsets : I.operand list -> unit
  val getRegLoc : int -> I.operand
  val getFregLoc : int -> I.operand
end

structure X86StackSpills : X86STACKSPILLS = 
struct
  exception RegSpills 
  structure I = X86Instr

  fun error msg = ErrorMsg.impossible ("X86StackSpills." ^ msg)

  val initialSpillOffset = X86Runtime.spillStart
  val spillOffset = ref initialSpillOffset
  val spillAreaSz = X86Runtime.spillAreaSz
  val availableOffsets = ref [] : I.operand list ref
  val availableFPOffsets = ref [] : I.operand list ref

  (* Indicate that memory some memory registers are not used and
   * can be used for spilling.
   *)
  fun setAvailableOffsets offsets = availableOffsets := offsets
  fun setAvailableFPOffsets offsets = availableFPOffsets := offsets

  fun newOffset n =
    if (n > spillAreaSz) then error "newOffset - spill area is too small"
    else spillOffset := n
  
  val spillTbl : I.operand IntHashTable.hash_table =
      IntHashTable.mkTable(0, RegSpills)
  val lookupTbl = IntHashTable.lookup spillTbl
  val addTbl    = IntHashTable.insert spillTbl

  fun init () = 
    (spillOffset:=initialSpillOffset; 
     availableOffsets := [];
     availableFPOffsets := [];
     IntHashTable.clear spillTbl
    )

  val toInt32 = Int32.fromInt

  fun getRegLoc reg = 
      lookupTbl reg
        handle _ => 
        let val operand = 
             case !availableOffsets of
               [] => let val offset = !spillOffset
                         val i32 = toInt32 offset
                     in  newOffset(offset+4); I.Immed i32 end
             | off::offs => (availableOffsets := offs; off) 
        in addTbl (reg,operand);
           operand
        end

  fun getFregLoc freg = 
      lookupTbl freg
        handle _ => 
        let val operand = 
             case !availableFPOffsets of
               [] =>
               let val offset = !spillOffset
                   val fromInt = Word.fromInt
                   val aligned = Word.toIntX(Word.andb(fromInt offset+0w7, fromInt ~8))
               in  newOffset(aligned+8); I.Immed(toInt32 aligned)
               end
             | off::offs => (availableFPOffsets := offs; off)
        in  addTbl (freg, operand);
            operand
        end
end
