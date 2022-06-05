(* load.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * constructs an SML/NJ  C interface for a specific C compiler
 *)

val cd = OS.FileSys.chDir;

val _ = print "loading info about C types...\n";
app use ["cc-info.sig.sml",
	 "cc-info.defaults.sml",
	 "cc-info.cc-mipseb-irix5.sml",
	 "cc-info.gcc-x86-linux.sml",
	 "cc-info.gcc-sparc-sunos.sml"
];

val _ = print "loading C interface...\n";
app use ["c-calls.sig.sml",
	 "c-calls.sml",
	 "cutil.sig.sml",
	 "cutil.sml"];

(*
val _ = print "instantiating CCalls for CC on MipsebIrix5\n";
structure CI = CCalls(structure CCInfo = CCInfoMipsebIrix5);
*)
(*
val _ = print "instantiating CCalls for GCC on X86Linux\n";
structure CI = CCalls(structure CCInfo = GCCInfoX86Linux);
*)
(*
val _ = print "instantiating CCalls for GCC on SparcSunOS\n";
structure CI = CCalls(structure CCInfo = GCCInfoSparcSunOS);
*)
(* *)
val _ = print "instantiating CCalls for a default compiler\n";
structure CI = CCalls(structure CCInfo = CCInfoDefaults);
(* *)

val _ = print "instantiating CUtil\n";
structure CU = CUtil(structure C = CI);
