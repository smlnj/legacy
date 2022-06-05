(* xserver-addr.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *)

structure XServerAddr : sig

    datatype server_addr
      = UNIX of string				(* ":dpy.scr" *)
      | INET_Hostname of (string * int)		(* "hostname:dpy.scr" *)
      | INET_Addr of (string * int)		(* "ddd.ddd.ddd.ddd:dpy.scr" *)

    exception BadAddr of string

    val getServerAddr : string -> {
	    addr : server_addr,
	    dpy_name : string,
	    screen : int
	  }

  end = struct

    structure SS = Substring
    structure F = Format

    datatype server_addr
      = UNIX of string				(* ":dpy.scr" *)
      | INET_Hostname of (string * int)		(* "hostname:dpy.scr" *)
      | INET_Addr of (string * int)		(* "ddd.ddd.ddd.ddd:dpy.scr" *)

    exception BadAddr of string

    val xTCPPort = 6000
    val xUNIXPath = "/tmp/.X11-unix/X"

    fun findChar c (s, j)= let
	  fun find i = if (String.sub(s, i) = c) then i else find(i+1)
	  in
	    (find j)
	  end

    fun mkUNIXAddr (dpyNum, scr) = {
	    addr = UNIX(xUNIXPath ^ (Int.toString dpyNum)),
	    dpy_name = F.format "unix:%d.%d" [F.INT dpyNum, F.INT scr],
	    screen = scr
	  }
    fun mkINETAddr (host, dpyNum, scr) = if (Char.isDigit(String.sub(host, 0)))
	  then {
	      addr = INET_Addr(host, xTCPPort+dpyNum),
	      dpy_name = F.format "%s:%d.%d" [F.STR host, F.INT dpyNum, F.INT scr],
	      screen = scr
	    }
	  else {
	      addr = INET_Hostname(host, xTCPPort+dpyNum),
	      dpy_name = F.format "%s:%d.%d" [F.STR host, F.INT dpyNum, F.INT scr],
	      screen = scr
	    }

    fun getServerAddr "" = mkUNIXAddr(0, 0)
      | getServerAddr s = let
	  fun cvtInt ss = (case (Int.scan StringCvt.DEC Substring.getc ss)
		 of NONE => raise BadAddr "expected integer"
		  | SOME(n, _) => n
		(* end case *))
	  val (hostname, rest) = let
		val (a, b) = SS.splitl (fn #":" => false | _ => true) (SS.full s)
		in
		  (SS.string a, SS.triml 1 b)
		end
	  val (dpy, scr) = (case SS.tokens (fn #"." => true | _ => false) rest
		 of [dpy] => (cvtInt dpy, 0)
		  | [dpy, scr] => (cvtInt dpy, cvtInt scr)
		  | [] => raise BadAddr "missing display"
		  | _ => raise BadAddr "badly formed address"
		(* end case *))
	  in
	    case hostname
	     of "" => mkUNIXAddr(dpy, scr)
	      | "unix" => mkUNIXAddr(dpy, scr)
	      | name => mkINETAddr(name, dpy, scr)
	    (* end case *)
	  end

  end (* XServerAddr *)
