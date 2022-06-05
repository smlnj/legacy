(* control.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FLINT_Control :> FLINTCONTROL =
struct
   local
       val priority = [10, 11, 1]
       val obscurity = 5
       val prefix = "flint"

       val registry = ControlRegistry.new
			  { help = "optimizer (FLINT) settings" }

       val _ = BasicControl.nest (prefix, registry, priority)

       val flag_cvt = ControlUtil.Cvt.bool
       val int_cvt = ControlUtil.Cvt.int
       val sl_cvt = ControlUtil.Cvt.stringList

       val nextpri = ref 0

       fun new (c, n, h, d) = let
	   val r = ref d
	   val p = !nextpri
	   val ctl = Controls.control { name = n,
					pri = [p],
					obscurity = obscurity,
					help = h,
					ctl = r }
       in
	   nextpri := p + 1;
	   ControlRegistry.register
	       registry
	       { ctl = Controls.stringControl c ctl,
		 envName = SOME (ControlUtil.EnvName.toUpper "FLINT_" n) };
	   r
       end
   in

    val print	        = new (flag_cvt, "print", "show IR", false)
    val printPhases	= new (flag_cvt, "print-phases", "show phases", false)
    val printFctTypes   = new (flag_cvt, "print-fct-types",
			       "show function types", false)
    val plchk           = new (flag_cvt, "plchk", "typecheck plambda", false)
    val nmdebugging     = new (flag_cvt, "nmdebugging",
			       "PLambda normalization debugging", false)
    val redebugging     = new (flag_cvt, "redebugging",
			       "reify phase debugging", false)
    val rtdebugging     = new (flag_cvt, "rtdebugging",
			       "runtime types(?) debugging", false)
    (* `split' should probably be called just after `fixfix' since
     * fcontract might eliminate some uncurry wrappers which are
     * locally unused but could be cross-module inlined. *)
    val phases =
	new (sl_cvt, "phases", "FLINT phases",
	     ["lcontract", (* Cruder but quicker than fcontract *)
	      "fixfix", "fcontract",
	      "specialize",
	      "loopify", "fixfix", "split", "fcontract",
	      "wrap", "fcontract", "reify",
	      (*"abcopt",*) "fcontract", "fixfix", "fcontract+eta"])

    val tmdebugging = new (flag_cvt, "tmdebugging", "TransTypes debugging", false)
    val trdebugging = new (flag_cvt, "trdebugging", "Translate debugging", false)

    val inlineThreshold = new (int_cvt, "inline-theshold",
			       "inline threshold", 16)
    (* val splitThreshold  = ref 0 *)
    val unrollThreshold = new (int_cvt, "unroll-threshold",
			       "unroll threshold", 20)
    val maxargs = new (int_cvt, "maxargs", "max number of arguments", 6)
    val dropinvariant = new (flag_cvt, "dropinvariant", "dropinvariant", true)

    val specialize = new (flag_cvt, "specialize",
			  "whether to specialize", true)
    (* val liftLiterals	= ref false *)
    val sharewrap = new (flag_cvt, "sharewrap",
			 "whether to share wrappers", true)
    val saytappinfo = new (flag_cvt, "saytappinfo",
			   "whether to show typelifting stats", false)

    (* only for temporary debugging *)
    val misc = ref 0

    (* FLINT internal type-checking controls *)
    val check = new (flag_cvt, "check", "whether to typecheck the IR", false)
        (* fails on MLRISC/*/*RegAlloc.sml *)
    val checkDatatypes = new (flag_cvt, "check-datatypes",
			      "typecheck datatypes", false)
	(* loops on the new cm.sml *)
    val checkKinds = new (flag_cvt, "check-kinds",
			  "check kinding information", true)

    (* exported for use in FLINT/main/flintcomp.sml *)
    val recover : (LambdaVar.lvar -> unit) ref = ref(fn x => ())

   end
end
