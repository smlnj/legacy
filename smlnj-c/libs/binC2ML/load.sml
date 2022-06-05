(* COPYRIGHT (c) 1996  Bell Laboratories, Lucent Technologies
 *
 * loads C interface and the binaryC convertor
 *)

val cd = Posix.FileSys.chdir;
val getcwd = Posix.FileSys.getcwd;

val CIdir = "../../";
fun CIuse s = use (CIdir^s);

val _ = print "loading info about C types...\n";

app CIuse ["cc-info.sig.sml",
	   "cc-info.defaults.sml",
	   "cc-info.cc-mipseb-irix5.sml",
	   "cc-info.gcc-x86-linux.sml",
	   "cc-info.gcc-sparc-sunos.sml"
];

val _ = print "loading C interface...\n";
app CIuse ["c-calls.sig.sml",
	   "c-calls.sml",
	   "cutil.sig.sml",
	   "cutil.sml"];


val _ = print "instantiating CCalls for a default compiler\n";
structure CI = CCalls(structure CCInfo = CCInfoDefaults);
val _ = print "instantiating CUtil\n";
structure CU = CUtil(structure C = CI);

val _ = app use ["binary-C-file.sig.sml",
		 "binary-C-file.sml"
];

structure BCF = BinaryC(structure C = CI);


