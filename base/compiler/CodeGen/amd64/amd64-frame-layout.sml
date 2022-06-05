(* amd64-frame-layout.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module defines symbolic names for the stack-pointer offsets
 * used to access various bits of information from the SML stack
 * frame.  See dev-notes/stack-frame.numbers for the layout.
 *)

structure AMD64FrameLayout =
  struct

  (* offset of register-spill area *)
    val spillAreaOffset = 0	(* was 80 *)

  (* size in bytes of spill area *)
    val spillAreaSzb = 8 * 1024

  (* location of exception pointer *)
    val exnPtrOffset = 8224	(* was 40 *)

  (* location of var pointer *)
    val varPtrOffset = 8232	(* was 56 *)

  (* location of ML State pointer *)
    val mspOffset = 8200	(* was 8 *)

  (* location of `saveregs` address *)
    val saveregsOffset = 8240	(* was 64 *)

  (* location of the base pointer *)
    val basePtrOffset = 8216	(* was 32 *)

  (* location to save the GC return address (gc-link) *)
    val gcLinkOffset = 8208	(* was 48 *)

  (* location of double-precision sign-bit mask *)
    val signBitOffset = 8256	(* was 16 *)

  (* location of double-precision negated sign-bit mask *)
    val negSignBitOffset = 8264	(* was 24 *)

  (* location of Overflow exception (used by LLVM backend) *)
    val overflowExnOffset = 8248

  end
