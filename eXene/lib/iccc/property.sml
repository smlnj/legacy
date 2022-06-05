(* property.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * An interface to the property management routines.
 *)

structure Property : PROPERTY =
  struct

    structure XTy = XProtTypes
    structure PropS = PropertyServer
    structure XErr = XErrors
    structure Dpy = Display
    structure DT = DrawTypes

  (* raised, if there is not enough space to store a property value
   * on the server.
   *)
    exception PropAlloc

  (* given message encode and reply decode functions, send and receive a query *)
    fun query (encode, decode) dpy = let
          val requestReply = Dpy.dpyRequestReply dpy
          fun ask msg = (decode (CML.sync (requestReply (encode msg))))
                handle XIo.LostReply => raise (MLXError.XERROR "[reply lost]")
                     | (XIo.ErrorReply err) =>
                        raise (MLXError.XERROR(XPrint.xerrorToString err))
          in
            ask
          end

 (* Various protocol requests that we need *)
    val reqGetProperty = query (
      XRequest.encodeGetProperty, XReply.decodeGetPropertyReply)
    fun rotateProps dpy arg =
      Dpy.dpyRequest dpy (XRequest.encodeRotateProperties arg)
    fun deleteProp dpy arg =
      Dpy.dpyRequest dpy (XRequest.encodeDeleteProperty arg)
    fun changeProperty dpy arg = let
      val ack = Dpy.dpyRequestAndChk dpy (XRequest.encodeChangeProperty arg)
      in
        (CML.sync ack)
          handle XIo.ErrorReply(XErr.XErr{kind=XErr.BadAlloc, ...}) =>
            raise PropAlloc
           | ex => raise ex
      end

    structure XTy' : sig
    type atom
      (* raw data from server (in ClientMessage, property values, ...) *)
    datatype raw_format = Raw8 | Raw16 | Raw32
    datatype raw_data = RAW_DATA of {
        format : raw_format,
        data : Word8Vector.vector
      }
      (* X property values.  A property value has a name and type, which are atoms,
       * and a value.  The value is a sequence of 8, 16 or 32-bit items, represented
       * as a format and a string.
       *)
    datatype prop_val = PROP_VAL of {
        typ : atom,
        value : raw_data
      }
      end = XTy;
    open XTy

  (* an abstract interface to a property on a window *)
    datatype property = PROP of {
    dpy : Dpy.display,
    name : atom,
    window : XTy.win_id,
    is_unique : bool
      }

  (* get the property server and window ID from a window *)
    fun infoOfWin (DT.WIN{id, scr=Dpy.SCREEN{dpy, ...}, ...}) = (dpy, id)

  (* get the property server of a display *)
    fun propServer (Dpy.DPY{property_server, ...}) = property_server

  (* get the display, window id and atom from a property *)
    fun infoOfProp (PROP{dpy, name, window, ...}) =
      (dpy, window, name)

  (* return the abstract representation of the named property on
   * the specified window.
   *)
    fun property (win, name) = let
      val (dpy, winId) = infoOfWin win
      in
        PROP{dpy=dpy, name=name, window=winId, is_unique=false}
      end

  (* generate a property on the specified window that is guaranteed
   * to be unused.
   *)
    fun unusedProperty win = let
      val (dpy, winId) = infoOfWin win
      val propName = PropS.unusedProperty (propServer dpy, winId)
      in
        PROP{dpy=dpy, name=propName, window=winId, is_unique=true}
      end

  (* return the atom that names the given property *)
    fun nameOfProp (PROP{name, ...}) = name

  (* update a proiperty *)
    fun updateProp mode (prop, value) = let
      val (dpy, winId, name) = infoOfProp prop
      in
        changeProperty dpy {win= winId, name= name, prop= value, mode= mode}
      end

  (* set the value of the property *)
    val setProperty = updateProp XTy.ReplaceProp

  (* append the property value to the property; the types
   * must match.
   *)
    val appendToProperty = updateProp XTy.AppendProp

  (* prepend the property value to the property; the types
   * must match.
   *)
    val prependToProperty = updateProp XTy.PrependProp

  (* delete the named property *)
    fun deleteProperty prop = let
      val (dpy, wid, name) = infoOfProp prop
      in
        deleteProp dpy {win = wid, prop = name}
      end

  (* create a new property initialized to the given value *)
    fun mkProperty (win, value) = let
      val prop = unusedProperty win
      in
        setProperty (prop, value); prop
      end

    exception RotateProps

  (* rotate the list of properties *)
    fun rotateProperties ([], _) = ()
      | rotateProperties (l as prop::r, n) = let
      val (dpy, wid, _) = infoOfProp prop
      fun chkProp prop = let
        val (_, w, name) = infoOfProp prop
        in
          if (w <> wid)
            then raise RotateProps
            else name
        end
      in
        rotateProps dpy {win= wid, delta= n, properties= map chkProp l}
      end

  (* get a property value, which may require several requests *)
    fun getProperty prop = let
      val (dpy, win, name) = infoOfProp prop
      fun sizeOf (XTy.RAW_DATA{data, ...}) = (Word8Vector.length data) div 4
      fun getChunk wordsSoFar = reqGetProperty dpy {
                  win = win, prop = name,
                  typ = NONE, (* AnyPropertyType *)
                  offset = wordsSoFar, len = 1024,
          delete = false
                }
      fun extendData (data', XTy.RAW_DATA{data, ...}) = data :: data'
      fun flattenData (data', XTy.RAW_DATA{format, data}) = XTy.RAW_DATA{
          format=format,
          data=Word8Vector.concat(rev (data :: data'))
        }
      fun getProp () = (case (getChunk 0)
         of NONE => NONE
          | (SOME{typ, bytes_after, value as XTy.RAW_DATA{data, ...}}) =>
              if (bytes_after = 0)
                then (
              SOME(PROP_VAL{typ=typ, value=value}))
                else getRest (sizeOf value, [data])
        (* end case *))
      and getRest (wordsSoFar, data') = (case (getChunk wordsSoFar)
         of NONE => NONE
          | (SOME{typ, bytes_after, value}) => if (bytes_after = 0)
              then SOME(PROP_VAL{typ=typ, value=flattenData(data', value)})
              else getRest(
            wordsSoFar + sizeOf value,
            extendData (data', value))
        (* end case *))
      in
        getProp ()
      end

  (* inherit the prop_change datatype *)
    structure PropertyServer' : sig
    datatype prop_change = NewValue | Deleted
      end = PropertyServer
    open PropertyServer'

  (* returns an event for monitoring changes to a property's
   * state.
   *)
    fun watchProperty (PROP{dpy, name, window, is_unique}) =
      PropS.watchProperty (propServer dpy, name, window, is_unique)

    (* Additions by ddeboer, May 2004. 
     * Dusty deBoer, KSU CIS 705, Spring 2004. *)
     
    (* xrdbOfScr: return the list of strings contained in the
     * XA_RESOURCE_MANAGER property of the root screen of the
     * specified screen. 
     * This should properly belong some other place than in ICCC,
     * as it has nothing to do with ICCC, except that it accesses
     * data in the screen type, and uses the GetProperty functions
     * of ICCC.
     *)
    fun xrdbOfScr (s: Display.screen) = 
        let
        val dpy = Display.displayOfScr(s)
        val rtw = Display.rootWinOfScr(s)
        in
            case getProperty(PROP{dpy=dpy,name=StdAtoms.atom_RESOURCE_MANAGER,window=rtw,is_unique=false})
            of  SOME(PROP_VAL{typ=typ,value=RAW_DATA{format=Raw8,data=data}}) => 
                    (String.tokens (fn c => case ord(c) of 13=>true|10=>true|_=>false) (Byte.bytesToString(data)))
              | _ => []
        end
    (* end additions by ddeboer *)
  end (* Property *)
