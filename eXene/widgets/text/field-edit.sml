(* field-edit.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories
 *
 * String edit widget with arrow buttons for scrolling.
 *)

signature FIELD_EDIT = 
  sig

    structure W : WIDGET

    type field_edit

    val mkFieldEdit : W.root -> {
      foregrnd : W.EXB.color option,
      backgrnd : W.EXB.color option,
      initval : string,
      minlen : int
    } -> field_edit

    val setString : field_edit -> string -> unit
    val getString : field_edit -> string
    val widgetOf : field_edit -> W.widget

  end (* FIELD_EDIT *)

structure FieldEdit : FIELD_EDIT = struct

  structure W = Widget

  open CML Geometry EXeneBase EXeneWin Interact Widget StrEdit
  structure B = Button

  datatype field_edit = 
    FieldEdit of (widget * (unit -> string) * (string -> unit))

  fun mkFieldEdit root (arg as {foregrnd, backgrnd, initval, minlen}) = let

    val scr = screenOf root

    val stredit = mkStrEdit root arg
    val cwidget = widgetOf stredit
    val cbnds = boundsFn cwidget
    val cRealize = realizeFn cwidget
    val {y_dim,...} = cbnds ()
    val naty = natDim y_dim
    val shift = shiftWin stredit

    val lefta = B.mkArrowBtn root {dir=B.AD_Left, sz=naty, 
      foregrnd=foregrnd, backgrnd=backgrnd}
    val leftw = B.widgetOf lefta
    val lbnds = boundsFn leftw
    val lRealize = realizeFn leftw
    val leftevt = B.evtOf lefta

    val righta = B.mkArrowBtn root {dir=B.AD_Right, sz=naty, 
      foregrnd=foregrnd, backgrnd=backgrnd}
    val rightw = B.widgetOf righta
    val rbnds = boundsFn rightw
    val rRealize = realizeFn rightw
    val rightevt = B.evtOf righta

    fun sizer () = let
      val {x_dim,y_dim} = cbnds ()
    in
      fixBounds ((natDim x_dim)+4,(natDim y_dim))
    end

    fun wontFit (SIZE{wid,...}) = let
      val {x_dim,...} = cbnds ()
    in
      (natDim x_dim) > wid
    end

    fun layout (sz as SIZE{wid,ht}) = let
      val {x_dim,...} = cbnds ()
      val {x_dim=ldim,...} = lbnds ()
      val {x_dim=rdim,...} = rbnds ()
      val lx = natDim ldim
      val rx = natDim rdim
    in
      if (natDim x_dim) <= wid then (
        false,
        RECT{x=0,y=0,wid=lx,ht=ht},
        RECT{x=0,y=0,wid=wid,ht=ht},
        RECT{x=wid-rx,y=0,wid=rx,ht=ht}
      )
      else (
        true,
        RECT{x=0,y=0,wid=lx,ht=ht},
        RECT{x=lx,y=0,wid=Int.max(1,wid-lx-rx),ht=ht},
        RECT{x=wid-rx,y=0,wid=rx,ht=ht}
      )
    end

    fun listener evt action = let
      val t = timeOutEvt (Time.fromReal 0.05)
      fun down_loop () = (
        sync t;
        case sync evt of
          B.BtnDown _ => (action();down_loop ())
        | _ => ()
      )
      fun loop () =
        loop(sync evt; action (); down_loop ())
    in
      loop ()
    end

    val pr = fn _ => ()

    fun realizeFieldEdit {env, win, sz} = let

      val lo as (active, lrect, crect, rrect) = layout sz

      val lwin = wrapCreate (win, lrect,argsOf leftw)
      val (linenv, loutenv as OutEnv{co=lco,...}) = createWinEnv ()
      val cwin = wrapCreate (win, crect,argsOf cwidget)
      val (cinenv, coutenv as OutEnv{co=cco,...}) = createWinEnv ()
      val rwin = wrapCreate (win, rrect,argsOf rightw)
      val (rinenv, routenv as OutEnv{co=rco,...}) = createWinEnv ()

      val (my_inenv, my_outenv) = createWinEnv ()
      (* modified by ddeboer: testing *)
      (*val InEnv{ci=myci,...} = ignoreInput my_inenv*)
      val InEnv{ci=myci,m=mym,k=myk,...} = my_inenv
      (* end modification *)
      val router = Router.mkRouter (env, my_outenv, [])

      val childco = wrapQueue cco

      fun doLayout ((a,lr,cr,rr), (a', lr',cr',rr')) = (
        if a' <> a then
          if a' then (mapWin lwin; mapWin rwin)
          else (unmapWin lwin; unmapWin rwin)
        else ();
        if lr' <> lr then moveAndResizeWin lwin lr'
        else ();
        if cr' <> cr then (pr "resize stredit\n";moveAndResizeWin cwin cr')
        else ();
        if rr' <> rr then moveAndResizeWin rwin rr'
        else ()
      )

      fun main sz lo = let
        fun handleCI (CI_Resize (RECT{x,y,wid,ht}), lo) =
            let
              val sz' = SIZE{wid=wid,ht=ht}
              val lo' = layout sz'
            in
              doLayout (lo,lo');
              main sz' lo'
            end
          | handleCI (_,lo) = lo
  
        fun handleC (CO_ResizeReq, lo as (a,_,_,_)) =
            let
              val _ = pr "resize req\n"
              val a' = wontFit sz
            in
              if a <> a' then 
                let
                  val lo' = layout sz
                  val _ = pr "newlayout\n"
                in
                  doLayout (lo,lo');
                  lo'
                end
              else lo
            end
          | handleC (CO_KillReq, lo) = lo
        
        (* added by ddeboer: *)
        fun handleK ((KEY_Press _),lo) = 
                ((TextIO.print " [field-edit received KEY_Press]\n"); lo)
          | handleK ((_),lo) = lo
        fun handleM ((MOUSE_Enter _),lo) =
                let
                val a = grabKeyboard cwin
                (* val _ = (TextIO.print (" [field-edit received MOUSE_Enter: "^(Int.toString(a))^"]\n")) *)
                in lo end
          | handleM ((_),lo) = lo
        (* end addition *)
        
        fun loop lo =
          loop (select [
            wrap (myci, fn evt => handleCI (msgBodyOf evt,lo)),
            (* added by ddeboer: *)
            wrap (myk, fn evt => handleK (msgBodyOf evt,lo)),
            wrap (mym, fn evt => handleM (msgBodyOf evt,lo)),
            (* end addition *)
            wrap (lco, fn _ => lo),
            wrap (rco, fn _ => lo),
            wrap (childco, fn evt => handleC(evt,lo))
          ])
      in
        loop lo
      end

    in
      Router.addChild router (lwin, loutenv);
      Router.addChild router (cwin, coutenv);
      Router.addChild router (rwin, routenv);
      spawn (fn () => listener leftevt (fn () => shift ~1));
      spawn (fn () => listener rightevt (fn () => shift 1));
      spawn (fn () => (main sz (active,lrect,crect,rrect);()));
      lRealize {env=linenv, win=lwin, sz=sizeOfRect lrect};
      cRealize {env=cinenv, win=cwin, sz=sizeOfRect crect};
      rRealize {env=rinenv, win=rwin, sz=sizeOfRect rrect};
      if active then (mapWin lwin; mapWin rwin) else ();
      mapWin cwin
    end
  in
    FieldEdit (
      mkWidget{
        root=root, 
        args= fn () => {background = NONE}, 
        boundsOf=sizer, 
        realize=realizeFieldEdit
      },
      fn () => getString stredit,
      setString stredit
    )
  end

  fun widgetOf (FieldEdit(w,_,_)) = w
  fun getString (FieldEdit(w,g,_)) = g ()
  fun setString (FieldEdit(_,_,s)) = s

end (* FieldEdit *)

