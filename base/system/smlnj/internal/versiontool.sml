(* versiontool.sml
 *
 *   A CM tool for automatically generating file version.sml
 *   from a template, incorporating current version and release.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure VersionTool = struct
  local
    val bump_release =
	ref (Option.isSome (OS.Process.getEnv "VERSIONTOOL_BUMP_RELEASE"))

    fun getVersion file =
	let val s = TextIO.openIn file
	in case TextIO.inputLine s of
	       SOME l =>
	       let val _ = TextIO.closeIn s
		   val fl = String.tokens
				(fn c => Char.isSpace c orelse c = #".") l
	       in map (fn f => getOpt (Int.fromString f, 0)) fl
	       end
	     | NONE => [0, 0]
	end handle _ => [0, 0]

    fun getRelease file =
	let val s = TextIO.openIn file
	in case TextIO.inputLine s of
	       SOME l => (TextIO.closeIn s; Int.fromString l)
	     | NONE => (TextIO.closeIn s; NONE)
	end handle _ => NONE

    fun putRelease (file, r) =
	let val s = TextIO.openOut file
	in TextIO.output (s, Int.toString r ^ "\n");
	   TextIO.closeOut s
	end

    fun bumpRelease (file, r) =
	if !bump_release then putRelease (file, r + 1) else ()

    fun gen { template, target, vfile, release } =
	let val version = getVersion vfile
	    val version' = case release of
			       NONE => version
			     | SOME r => version @ [r]
	    val vstring = String.concatWith ", " (map Int.toString version')
	    val ss = TextIO.openIn template
	    val ts = TextIO.openOut target
	    fun loop () =
		case TextIO.input1 ss of
		    NONE => ()
		  | SOME #"%" =>
		     (case TextIO.input1 ss of
			  SOME #"V" => (TextIO.output (ts, vstring); loop ())
			| SOME #"F" => (TextIO.output (ts, OS.Path.file target);
					TextIO.output (ts, " generated from");
					loop ())
			| SOME c => (TextIO.output1 (ts, c); loop ())
			| NONE => TextIO.output1 (ts, #"%"))
		  | SOME c => (TextIO.output1 (ts, c); loop ())
	in 
	   loop ();
	   TextIO.closeIn ss;
	   TextIO.closeOut ts
	end

    val tool = "versiontool"
    val class = "version"

    val kw_target = "target"
    val kw_versionfile = "versionfile"
    val kw_releasefile = "releasefile"
    val keywords = [kw_target, kw_versionfile, kw_releasefile]

    fun versiontoolrule { spec: Tools.spec,
			  native2pathmaker,
			  context: Tools.rulecontext,
			  defaultClassOf,
			  sysinfo } : Tools.partial_expansion =
	let fun dogen (targetpp, versionfilepp, releasefilepp) () =
		let val templatep = Tools.srcpath (#mkpath spec ())
		    val targetp = Tools.srcpath targetpp
		    val target = Tools.nativeSpec targetp
		    val template = Tools.nativeSpec templatep
		    val vfile = Tools.nativePreSpec versionfilepp
		    val rfile = Tools.nativePreSpec releasefilepp
		    val release = getRelease rfile
		    fun newerThanTarget f = Tools.outdated tool ([target], f)
		in if List.exists newerThanTarget [template, vfile, rfile] then
		       gen { template = template, target = target,
			     vfile = vfile, release = release }
		   else ();
		   bumpRelease (rfile, getOpt (release, ~1));
		   ({ smlfiles = [(targetp, { share = Sharing.DONTCARE,
					      setup = (NONE, NONE),
					      split = NONE,
					      noguid = false,
					      locl = false,
					      controllers = [] })],
		      cmfiles = [],
		      sources = [(templatep, { class = class,
					       derived = #derived spec })] },
		    [])
		end
	    fun complain l =
		raise Tools.ToolError { tool = tool, msg = concat l }
	in case #opts spec of
	       NONE => complain ["missing parameters"]
	     | SOME to =>
	       let val { matches, restoptions } =
		       Tools.parseOptions { tool = tool,
					    keywords = keywords,
					    options = to }
		   fun match kw =
		       case matches kw of
			   NONE => complain ["missing parameter \"", kw, "\""]
			 | SOME [Tools.STRING { mkpath, ... }] => mkpath ()
			 | _ => complain ["invalid parameter \"", kw, "\""]
	       in context (dogen (match kw_target,
				  match kw_versionfile,
				  match kw_releasefile))
	       end
	end
  in
  val bump_release = bump_release
  val _ = Tools.registerClass (class, versiontoolrule)
  end
end
