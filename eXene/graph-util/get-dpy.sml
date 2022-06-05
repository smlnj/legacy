(* get-dpy.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Utility code for getting the display name and authentication information.
 *)

structure GetDpy : GET_DPY =
  struct

    structure EXB = EXeneBase
    structure SS = Substring

    fun getDpyName NONE = (case (OS.Process.getEnv "DISPLAY")
       of NONE => ""
        | (SOME dpy) => dpy
      (* end case *))
      | getDpyName (SOME dpy) = dpy

  (* parse a string specifying a X display into its components. *)
    fun parseDisplay "" = {host="",dpy="0",screen="0"}
      | parseDisplay d = let
          val (host,rest) = SS.splitl (fn c => c <> #":") (SS.full d)
          val (dpy,scr) = SS.splitl (fn c => c <> #".") rest
          in
            if SS.size dpy < 2 then raise EXB.BadAddr "No display field"
            else if SS.size scr = 1 then raise EXB.BadAddr "No screen number"
            else {host=SS.string host,
                  dpy=SS.string(SS.triml 1 dpy),
                  screen=SS.string(SS.triml 1 scr)}
          end

  (* given an optional display name, return the display and authentication
   * information.  If the argument is NONE, then we use the DISPLAY environment
   * variable if it is defined, and "" if it is not defined.
   *)
    fun getDpy dpyOpt = 
      let
      val dpy = getDpyName dpyOpt
      val auth = 
        (case dpy of 
            "" =>   XAuth.getAuthByAddr {
                        (* modified ddeboer, spring 2005:
                         * this condition should probably throw exception;
                         * xlib XOpenDisplay code "/* Oops! No DISPLAY environment variable..."
                         * returns NULL from XOpenDisplay (fails to open connection).
                         * was:
                        family = XAuth.familyLocal,
                        addr = "", *)
                        family = XAuth.familyWild,
                        addr = NetHostDB.getHostName(), (* necessary to look up xrdb record *)
                        dpy = "0"
                    }
          | d => let
              (* following line modified, ddeboer, Jan 2005. was:
              val {dpy,...} = parseDisplay d *)
              val {dpy,host,...} = parseDisplay d
              (* added ddeboer, spring 2005 *)
              fun mkXA fam addr = XAuth.getAuthByAddr{
                                  family = fam,
                                  addr   = addr,
                                  dpy    = dpy
                                }
              in
                (* added ddeboer, spring 2005, was:
                    XAuth.getAuthByAddr {
                        family = XAuth.familyInternet,
                        addr = "",
                        dpy = dpy
                    }
                *)
                (* we must obtain the string to be used for comparison in getAuthByAddr.
                 * for familyLocal, this is the local hostname.
                 * for familyInternet, this is the IP address (as a string) *)
                case host of
                    ""          => mkXA XAuth.familyLocal (NetHostDB.getHostName())
                  | "localhost" => mkXA XAuth.familyLocal (NetHostDB.getHostName())
                  | _           => mkXA XAuth.familyInternet 
                    (* this should more properly be set to the peer address of the
                     * connection, *after* the connection has been made. However,
                     * that would be a bit difficult with this architecture. *)
                        (case (NetHostDB.getByName host) of 
                          SOME e => (NetHostDB.toString (NetHostDB.addr e))
                        | _ => "")
              end
        (* end case *))
      in
        (dpy, auth)
      end

  end;

