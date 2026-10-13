(* internal-timer.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure InternalTimer : sig

    include TIMER

    val resetTimers : unit -> unit
    val zeroTimers : unit -> unit

  end = struct

    structure PB = PreBasis
    structure Int = IntImp
    structure Int32 = Int32Imp
    structure Time = TimeImp

    type time = { usr: PB.time, sys: PB.time }

    datatype cpu_timer = CPUT of { nongc: time, gc: time }
    datatype real_timer = RealT of PB.time

    local
      val gettime' : unit -> Int64.int * Int64.int * Int64.int =
	    CInterface.c_function "SMLNJ-Time" "gettime"
      fun mkTime ns = Time.fromNanoseconds (Int64Imp.toLarge ns)
    in
    fun getTime () = let
	  val (usr, sys, gc) = gettime' ()
	  in {
	    nongc = { usr = mkTime usr, sys = mkTime sys },
	    gc    = { usr = mkTime gc, sys = Time.zeroTime }
	  } end
    end (* local *)

    fun startCPUTimer () = CPUT (getTime())
    fun startRealTimer () = RealT (Time.now ())

    local
	val initCPUTime = ref (startCPUTimer ())
	val initRealTime = ref (startRealTimer ())
    in
    fun totalCPUTimer () = !initCPUTime
    fun totalRealTimer () = !initRealTime
    fun resetTimers () =
	(initCPUTime := startCPUTimer ();
	 initRealTime := startRealTimer ())
    (* set the timers to zero; this is used when exporting an image, so that
     * the image does not record the time at which it was built (the timers
     * are reset when the image is resumed anyway)
     *)
    fun zeroTimers () = let
	  val zero = { usr = Time.zeroTime, sys = Time.zeroTime }
	  in
	    initCPUTime := CPUT { nongc = zero, gc = zero };
	    initRealTime := RealT Time.zeroTime
	  end
    end (* local *)

    local
	infix -- ++
	fun usop timeop (t: time, t': time) =
	    { usr = timeop (#usr t, #usr t'), sys = timeop (#sys t, #sys t') }
	val op -- = usop Time.-
	val op ++ = usop Time.+
    in

    fun checkCPUTimes (CPUT t) = let
	val t' = getTime ()
    in
	{ nongc = #nongc t' -- #nongc t, gc = #gc t' -- #gc t }
    end

    fun checkCPUTimer tmr = let
	val t = checkCPUTimes tmr
    in
	#nongc t ++ #gc t
    end

    fun checkGCTime (CPUT t) = Time.- (#usr (#gc (getTime ())), #usr (#gc t))

    end (* local *)

    fun checkRealTimer (RealT t) = Time.-(Time.now(), t)

end (* InternalTimer *)
