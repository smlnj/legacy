(* widget.sml
 *
 * COPYRIGHT (c) 1991,1994 by AT&T Bell Laboratories.
 *
 * Definitions for basic graphical objects.
 *)

signature WIDGET =
  sig

    include WIDGET_BASE
    include ROOT

    structure Interact : INTERACT
    structure EXW : EXENE_WIN

    datatype relief = Flat | Raised | Sunken | Groove | Ridge

    include WIDGET_ATTRS

    type widget

    exception AlreadyRealized

    (* type realize_fn = {
     *      env : Interact.in_env,
     *      win : EXB.window,
     *      sz : G.size
     *   } -> unit
     *)
    type realize_fn

    val mkWidget : {
        root : root,
        args : unit -> win_args,
        boundsOf : unit -> bounds,
        realize : realize_fn
      } -> widget
    val rootOf : widget -> root
    val boundsOf : widget -> bounds
    val argsOf : widget -> win_args
    val argsFn : widget -> unit -> win_args
    val realizeFn : widget -> realize_fn
    val sameWidget : widget * widget -> bool
    val natSize : widget -> G.size
    val boundsFn : widget -> unit -> bounds
    val okaySize : widget * G.size -> bool

    val filterMouse : widget -> 
      (widget * 
        ((Interact.mouse_msg Interact.addr_msg CML.event * Interact.mouse_msg Interact.addr_msg CML.chan) CML.event))

    val filterKey : widget -> 
      (widget * 
        ((Interact.kbd_msg Interact.addr_msg CML.event * Interact.kbd_msg Interact.addr_msg CML.chan) CML.event))

    val filterCmd : widget -> 
      (widget * 
        ((Interact.cmd_in Interact.addr_msg CML.event * Interact.cmd_in Interact.addr_msg CML.chan) CML.event))

    val ignoreMouse : widget -> widget
    val ignoreKey : widget -> widget

  end (* WIDGET *)

structure Widget : WIDGET =
  struct

    open WidgetBase
    open Root
    open WidgetAttrs

    structure Interact = Interact
    structure EXW = EXeneWin

    open Geometry Interact EXeneWin

    type arg = Attrs.attr_name * Attrs.attr_value

    type realize_fn = {
         env : Interact.in_env,
         win : EXB.window,
         sz : G.size
      } -> unit

    exception AlreadyRealized

    datatype widget = Widget of {
	root : root,
        id : int,
        args : unit -> win_args,
	bounds_of : unit -> bounds,
        realized : unit SyncVar.ivar,
	realize : realize_fn
      }

    fun mkWidget {root=root as Root{idGen,...}, args, boundsOf,realize} =
          Widget {
	    root = root,
	    args = args,
            realized = SyncVar.iVar (),
            id = idGen(),
	    bounds_of = boundsOf,
	    realize = realize
          }
    fun rootOf (Widget{root,...}) = root
    fun boundsOf (Widget{bounds_of,...}) = bounds_of ()
    fun argsOf (Widget{args,...}) = args ()
    fun argsFn (Widget{args,...}) = args
    fun boundsFn (Widget{bounds_of,...}) = bounds_of
    fun realizeFn (Widget{realize,realized,...}) arg = (
          (SyncVar.iPut(realized,())) handle _ => raise AlreadyRealized; 
          realize arg)
    fun sameWidget (Widget{id,root,...},Widget{id=id',root=root',...}) =
          id = id' andalso sameRoot(root,root')
    fun natSize (Widget{bounds_of,...}) = let
          val {x_dim,y_dim} = bounds_of ()
          in
            SIZE{wid = natDim x_dim, ht = natDim y_dim}
          end
    fun okaySize (widget, sz) = compatibleSize(boundsOf widget, sz)

    fun filterWidget (selfn,repfn) (Widget{root,realize,bounds_of,args,...}) = let
      open CML
      val realizeCh = channel ()
      fun realize' {win,env,sz} = let
        val evt = selfn env
        val echan = channel ()
        val env' = repfn (env, recvEvt echan)
        in
          send(realizeCh, (evt,echan));
          realize{win=win,sz=sz,env=env'}
        end
      in
	(mkWidget {
          root = root,
          args = args,
	  boundsOf = bounds_of,
	  realize = realize'
        },
        recvEvt realizeCh)
      end
    val filterMouse = filterWidget (fn (InEnv{m,...}) => m, replaceMouse)
    val filterKey = filterWidget (fn (InEnv{k,...}) => k, replaceKey)
    val filterCmd = filterWidget (fn (InEnv{ci,...}) => ci, replaceCI)

    fun ignoreWidget (selfn,repfn) (Widget{root,realize,bounds_of,args,...}) = let
      open CML
      fun realize' {win,env,sz} = let
        fun loop evt () = (sync evt; loop evt ())
        val env' = repfn (env, SyncVar.iGetEvt (SyncVar.iVar()))
        in
          spawn (loop (selfn env));
          realize{win=win,sz=sz,env=env'}
        end
      in
	mkWidget {
          root = root,
          args = args,
	  boundsOf = bounds_of,
	  realize = realize'
        }
      end
    val ignoreMouse = ignoreWidget (fn (InEnv{m,...}) => m, replaceMouse)
    val ignoreKey = ignoreWidget (fn (InEnv{k,...}) => k, replaceKey)

    datatype relief = datatype ThreeD.relief
  
    type attr_spec = Attrs.attr_name * Attrs.attr_type * Attrs.attr_value
  end (* Widget *)
