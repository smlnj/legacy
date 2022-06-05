(* xcvtfns.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * Conversion routines between X types and their representation in messages.
 *)

structure XCvtFuns : sig

    val graphOpToWire	: XProtTypes.graphics_op -> word
    val gravityToWire	: XProtTypes.gravity -> word
    val boolToWire	: bool -> word
    val stackModeToWire	: XProtTypes.stack_mode -> word

    val doValList : int -> (word option Array.array -> 'a -> unit)
	  -> 'a list
	    -> XProtTypes.value_list

  end = struct

    structure XTy = XProtTypes

  (* Process a configuration value list, producing an value_list. *)
    fun doValList n f lst = let
	  val arr = Array.array(n, NONE)
	  in
	    List.app (f arr) lst; XTy.VALS arr
	  end

    fun graphOpToWire XTy.OP_Clr		= 0w0
      | graphOpToWire XTy.OP_And		= 0w1
      | graphOpToWire XTy.OP_AndNot		= 0w2
      | graphOpToWire XTy.OP_Copy		= 0w3
      | graphOpToWire XTy.OP_AndInverted	= 0w4
      | graphOpToWire XTy.OP_Nop		= 0w5
      | graphOpToWire XTy.OP_Xor		= 0w6
      | graphOpToWire XTy.OP_Or			= 0w7
      | graphOpToWire XTy.OP_Nor		= 0w8
      | graphOpToWire XTy.OP_Equiv		= 0w9
      | graphOpToWire XTy.OP_Not		= 0w10
      | graphOpToWire XTy.OP_OrNot		= 0w11
      | graphOpToWire XTy.OP_CopyNot		= 0w12
      | graphOpToWire XTy.OP_OrInverted		= 0w13
      | graphOpToWire XTy.OP_Nand		= 0w14
      | graphOpToWire XTy.OP_Set		= 0w15

    fun gravityToWire XTy.ForgetGravity		= 0w0	(* bit gravity only *)
      | gravityToWire XTy.UnmapGravity		= 0w0	(* window gravity only *)
      | gravityToWire XTy.NorthWestGravity	= 0w1
      | gravityToWire XTy.NorthGravity		= 0w2
      | gravityToWire XTy.NorthEastGravity	= 0w3
      | gravityToWire XTy.WestGravity		= 0w4
      | gravityToWire XTy.CenterGravity		= 0w5
      | gravityToWire XTy.EastGravity		= 0w6
      | gravityToWire XTy.SouthWestGravity	= 0w7
      | gravityToWire XTy.SouthGravity		= 0w8
      | gravityToWire XTy.SouthEastGravity	= 0w9
      | gravityToWire XTy.StaticGravity		= 0w10

    fun boolToWire false = 0w0
      | boolToWire true = 0w1

    fun stackModeToWire XTy.Above	= 0w0
      | stackModeToWire XTy.Below	= 0w1
      | stackModeToWire XTy.TopIf	= 0w2
      | stackModeToWire XTy.BottomIf	= 0w3
      | stackModeToWire XTy.Opposite	= 0w4

  end (* XCvtFuns *)
