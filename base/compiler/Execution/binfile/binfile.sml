(* binfile.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * author: Matthias Blume
 *
 * See dev-notes/binfile.adoc for a description of the binfile format.
 * This file must be kept in sync with runtime/kernel/boot.c.
 *)

structure Binfile :> BINFILE =
  struct

    structure Pid = PersStamps
    structure W8V = Word8Vector

    exception FormatError = CodeObj.FormatError

    type pid = Pid.persstamp

    type csegments = CodeObj.csegments

    type executable = CodeObj.executable

    type stats = { env: int, inlinfo: int, data: int, code: int }

    type pickle = { pid: pid, pickle: W8V.vector }

    datatype bfContents = BF of {
	imports: ImportTree.import list,
	exportPid: pid option,
	cmData: pid list,
	senv: pickle,
	lambda: pickle,
	guid: string,
	csegments: csegments,
	executable: executable option ref
      }

    fun unBF (BF x) = x

    val bytesPerPid = Pid.persStampSize
    val magicBytes = 16

    val exportPidOf = #exportPid o unBF
    val cmDataOf = #cmData o unBF
    val senvPickleOf = #senv o unBF
    val staticPidOf = #pid o senvPickleOf
    val lambdaPickleOf = #lambda o unBF
    val lambdaPidOf = #pid o lambdaPickleOf

    val guidOf = #guid o unBF

    fun error msg = (
	  Control_Print.say (concat ["binfile format error: ", msg, "\n"]);
	  raise FormatError)

    val fromInt = Word32.fromInt
    val fromByte = Word32.fromLargeWord o Word8.toLargeWord
    val toByte = Word8.fromLargeWord o Word32.toLargeWord
    val >> = Word32.>>
    infix >>

    fun bytesIn (s, 0) = Byte.stringToBytes ""
      | bytesIn (s, n) = let
	  val bv = BinIO.inputN (s, n)
	  in
	    if n = W8V.length bv
	      then bv
	      else error (concat[
		  "expected ", Int.toString n, " bytes, but found ",
		  Int.toString(W8V.length bv)
		])
	  end

    fun readInt32 s = LargeWord.toIntX(PackWord32Big.subVec(bytesIn(s, 4), 0))

    fun readPackedInt32 s = let
	  fun loop n = (case BinIO.input1 s
		 of NONE => error "unable to read a packed int32"
	          | SOME w8 => let
		      val n' = n * 0w128 + Word8.toLargeWord (Word8.andb (w8, 0w127))
		      in
		        if Word8.andb (w8, 0w128) = 0w0 then n' else loop n'
		      end
		(* end case *))
	  in
	    LargeWord.toIntX (loop 0w0)
	  end

    fun readPid s = Pid.fromBytes (bytesIn (s, bytesPerPid))
    fun readPidList (s, n) = List.tabulate (n, fn _ => readPid s)

    fun readImportTree s = (case readPackedInt32 s
	   of 0 => (ImportTree.ITNODE [], 1)
	    | cnt => let
		fun readImportList 0 = ([], 0)
		  | readImportList cnt = let
		      val selector = readPackedInt32 s
		      val (tree, n) = readImportTree s
		      val (rest, n') = readImportList (cnt - 1)
		      in
			((selector, tree) :: rest, n + n')
		      end
		val (l, n) = readImportList cnt
		in
		  (ImportTree.ITNODE l, n)
		end
	  (* end case *))

    fun readImports (s, n) = if n <= 0
	  then []
	  else let
	    val pid = readPid s
	    val (tree, n') = readImportTree s
	    val rest = readImports (s, n - n')
	    in
	      (pid, tree) :: rest
	    end

    fun pickleInt32 i = let
	  val w = fromInt i
	  fun out w = toByte w
	  in
	    W8V.fromList [
		toByte (w >> 0w24), toByte (w >> 0w16),
		toByte (w >> 0w8), toByte w
	      ]
	  end
    fun writeInt32 s i = BinIO.output (s, pickleInt32 i)

    fun picklePackedInt32 i = let
	  val n = LargeWord.fromInt i
	  val // = LargeWord.div
	  val %% = LargeWord.mod
	  val !! = LargeWord.orb
	  infix // %% !!
	  val toW8 = Word8.fromLargeWord
	  fun r (0w0, l) = W8V.fromList l
	    | r (n, l) = r (n // 0w128, toW8 ((n %% 0w128) !! 0w128) :: l)
	  in
	    r (n // 0w128, [toW8 (n %% 0w128)])
	  end

    fun writePid (s, pid) = BinIO.output (s, Pid.toBytes pid)
    fun writePidList (s, l) = app (fn p => writePid (s, p)) l

    local
      fun pickleImportSpec ((selector, tree), (n, p)) = let
	    val sp = picklePackedInt32 selector
	    val (n', p') = pickleImportTree (tree, (n, p))
	    in
	      (n', sp :: p')
	    end
      and pickleImportTree (ImportTree.ITNODE [], (n, p)) = (n + 1, picklePackedInt32 0 :: p)
	| pickleImportTree (ImportTree.ITNODE l, (n, p)) = let
	    val (n', p') = foldr pickleImportSpec (n, p) l
	    in
	      (n', picklePackedInt32 (length l) :: p')
	    end

      fun pickleImport ((pid, tree), (n, p)) = let
	    val (n', p') = pickleImportTree (tree, (n, p))
	    in
	      (n', Pid.toBytes pid :: p')
	    end
    in
    fun pickleImports l = let
	  val (n, p) = foldr pickleImport (0, []) l
	  in
	    (n, W8V.concat p)
	  end
    end (* local *)

  (* The "magic string" is a 16-byte string that is formed from the architecture
   * and version number.  The basic format is "<arch>-<version>", with the architecture
   * limited to 7 characters and the verion limited to 8 (one character for the "-").
   * It is padded with spaces as necessary to fill out 16 bytes.
   *)
    fun mkMAGIC (arch, version) = let
	  val vbytes = 8			(* version part; allow for xxxx.y.z *)
	  val abytes = magicBytes - vbytes - 1  (* arch part *)
	  fun trim (i, s) = if (size s > i) then substring (s, 0, i) else s
	(* use at most the first three components of version_id *)
	  fun vers2s [] = []
	    | vers2s [x] = [Int.toString x]
	    | vers2s [x, y] = [Int.toString x, ".", Int.toString y]
	    | vers2s (x :: y :: z :: _) = [Int.toString x, ".", Int.toString y, ".", Int.toString z]
	  val v = trim (vbytes, concat (vers2s version))
	  val a = trim (abytes, arch)
	  in
	     StringCvt.padRight #" " magicBytes (concat[a, "-", v])
	    (* assert (W8V.length (MAGIC <arch>) = magicBytes *)
	  end

  (* calculate size of code objects (including lengths and entrypoints) *)
    fun codeSize (csegs: csegments) =
	  CodeObj.size(#code csegs) + W8V.length(#data csegs) + 16

  (* This function must be kept in sync with the "write" function below.
   * It calculates the number of bytes written by a corresponding
   * call to "write".
   *)
    fun size { contents, nopickle } = let
	  val { imports, exportPid, senv, cmData, lambda,  csegments, guid, ... } =
		unBF contents
	  val (_, picki) = pickleImports imports
	  val hasExports = isSome exportPid
	  fun pickleSize { pid, pickle } = if nopickle then 0 else W8V.length pickle
	  in
	    magicBytes +
	    9 * 4 +
	    W8V.length picki +
	    (if hasExports then bytesPerPid else 0) +
	    bytesPerPid * (length cmData + 2) + (* 2 extra: stat/sym *)
	    String.size guid +
	    pickleSize lambda +
	    codeSize csegments +
	    pickleSize senv
	  end

    fun create { imports, exportPid, cmData, senv, lambda, csegments, guid } = BF {
	    imports = imports,
	    exportPid = exportPid,
	    cmData = cmData,
	    senv = senv,
	    lambda = lambda,
	    guid = guid,
	    csegments = csegments,
	    executable = ref NONE
	  }

  (* must be called with second arg >= 0 *)
    fun readCSegs (strm, nbytes) = let
	  val dataSz = readInt32 strm
	  val _ = readInt32 strm (* ignore entry point field for data segment *)
	  val avail = nbytes - dataSz - 8
	  val data = if avail < 0 then error "data size" else bytesIn (strm, dataSz)
	  val codeSz = readInt32 strm
	  val ep = readInt32 strm
	  val avail = avail - codeSz - 8
	  val code = if avail < 0 then error "code size" else CodeObj.input(strm, codeSz)
	  in
	    CodeObj.set_entrypoint (code, ep);
	    { code = code, data = data }
	  end

    fun readGUid s = let
	  val _ = bytesIn (s, magicBytes)
	  val _ = readInt32 s
	  val ne = readInt32 s
	  val importSzB = readInt32 s
	  val cmInfoSzB = readInt32 s
	  val nei = cmInfoSzB div bytesPerPid
	  val lambdaSz = readInt32 s
	  val g = readInt32 s
	  val _ = bytesIn (s, importSzB + 3 * 4)
	  val _ = bytesIn (s, ne * bytesPerPid)
	  val _ = readPidList (s, nei)
	  val _ = bytesIn (s, lambdaSz)
	  in
	    Byte.bytesToString (bytesIn (s, g))
	  end

    fun read { arch, version, stream = s } = let
	  val MAGIC = mkMAGIC (arch, version)
	  val magic = bytesIn (s, magicBytes)
	  val _ = if Byte.bytesToString magic <> MAGIC
		then error (concat[
		    "bad magic number \"", String.toString(Byte.bytesToString magic),
		    "\", expected \"", String.toString MAGIC, "\""
		  ])
		else ()
	  val leni = readInt32 s
	  val ne = readInt32 s
	  val importSzB = readInt32 s
	  val cmInfoSzB = readInt32 s
	  val nei = cmInfoSzB div bytesPerPid
	  val lambdaSz = readInt32 s
	  val g = readInt32 s
	  val pad = readInt32 s
	  val cs = readInt32 s
	  val es = readInt32 s
	  val imports = readImports (s, leni)
	  val exportPid = (case ne
		 of 0 => NONE
		  | 1 => SOME(readPid s)
		  | _ => error "too many export PIDs"
		(* end case *))
	  val envPids = readPidList (s, nei)
	  val (staticPid, lambdaPid, cmData) = (case envPids
		 of st :: lm :: cmData => (st, lm, cmData)
		  | _ => error "env PID list"
		(* end case *))
	  val plambda = bytesIn (s, lambdaSz)
	  val guid = Byte.bytesToString (bytesIn (s, g))
	(* skip padding *)
	  val _ = if pad <> 0 then ignore (bytesIn (s, pad)) else ()
	(* now get the code *)
	  val code = readCSegs (s, cs)
	  val penv = bytesIn (s, es)
	  in {
	    contents = create {
		imports = imports,
		exportPid = exportPid,
		cmData = cmData,
		senv = { pid = staticPid, pickle = penv },
		lambda = { pid = lambdaPid, pickle = plambda },
		guid = guid,
		csegments = code
	      },
	    stats = {
		env = es, inlinfo = lambdaSz, code = cs,
		data = W8V.length (#data code)
	      }
	  } end

    fun writeCSegs (s, {code, data}) = (
	  writeInt32 s (W8V.length data);
	  writeInt32 s 0;  (* dummy entry point for data segment *)
	  BinIO.output(s, data);
	  writeInt32 s (CodeObj.size code);
	  writeInt32 s (CodeObj.entrypoint code);
	  CodeObj.output (s, code))

    fun write { arch, version, stream = s, contents, nopickle } = let
	(* Keep this in sync with "size" (see above). *)
	  val { imports, exportPid, cmData, senv, lambda, csegments, guid, ... } = unBF contents
	  val { pickle = senvP, pid = staticPid } = senv
	  val { pickle = lambdaP, pid = lambdaPid } = lambda
	  val envPids = staticPid :: lambdaPid :: cmData
	  val (leni, picki) = pickleImports imports
	  val importSzB = W8V.length picki
	  val (ne, epl) = (case exportPid
		 of NONE => (0, [])
	          | SOME p => (1, [p])
		(* end case *))
	  val nei = length envPids
	  val cmInfoSzB = nei * bytesPerPid
	  fun pickleSize { pid, pickle } = if nopickle then 0 else W8V.length pickle
	  val lambdaSz = pickleSize lambda
	  val g = String.size guid
	  val pad = 0			(* currently no padding *)
	  val cs = codeSize csegments
	  val es = pickleSize senv
	  val writeEnv = if nopickle
		then fn () => ()
		else fn () => BinIO.output (s, senvP)
	  val datasz = W8V.length (#data csegments)
	  val MAGIC = mkMAGIC (arch, version)
	  in
	    BinIO.output (s, Byte.stringToBytes MAGIC);
	    app (writeInt32 s) [leni, ne, importSzB, cmInfoSzB,
				lambdaSz, g, pad, cs, es];
	    BinIO.output (s, picki);
	    writePidList (s, epl);
	    (* arena1 *)
	    writePidList (s, envPids);
	    (* arena2 -- pickled flint stuff *)
	    if lambdaSz = 0 then () else BinIO.output (s, lambdaP);
	    (* GUID area *)
	    BinIO.output (s, Byte.stringToBytes guid);
	    (* padding area is currently empty *)
	    writeCSegs (s, csegments);
	    writeEnv ();
	    { env = es, inlinfo = lambdaSz, data = datasz, code = cs }
	  end

    fun exec (BF {imports, exportPid, executable, csegments, ... }, dynEnv, exnWrapper) = let
	  val executable = (case !executable
		 of SOME e => e
		  | NONE => let
		      val e = Isolate.isolate (
			        Execute.mkExec { cs = csegments, exnWrapper = exnWrapper })
		      in
			executable := SOME e; e
		      end
		(* end case *))
	  in
	    Execute.execute {
		executable = executable,
		imports = imports,
		exportPid = exportPid,
		dynEnv = dynEnv
	      }
	  end

  end (* structure Binfile *)
