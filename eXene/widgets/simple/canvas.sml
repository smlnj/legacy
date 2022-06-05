(* canvas.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Simple canvas widget, serving as a template for
 * an application-dependent widget.
 *
 * NOTE: this probably needs rewriting.  It would be nice to avoid the extra
 * threads on the input streams, and the realize function should be called
 * directly.
 *)

structure Canvas : CANVAS = 
  struct

    structure W = Widget
    structure D = Drawing
    structure G = Geometry
    structure EXB = EXeneBase
    structure I = Interact

    datatype req_msg
      = GetSize of G.size SyncVar.ivar
      | DoRealize of ({
	  env : I.in_env,
	  win : EXB.window,
	  sz : G.size
	} * unit SyncVar.ivar)

    datatype canvas = Canvas of {
	widget : W.widget,
	reqc : req_msg CML.chan,
	winvar : EXB.window SyncVar.ivar
      }

    val attrs = [
        (Attrs.attr_background,     Attrs.AT_Color,    Attrs.AV_NoValue)
      ]

    fun mkCanvas root bounds = let
          val reqCh = CML.channel () and newSzCh = CML.channel()
          val winVar = SyncVar.iVar ()
          val initSize = G.SIZE{
		  wid = W.natDim (#x_dim bounds),
		  ht = W.natDim (#y_dim bounds)
		}
          val (canvasInenv, I.OutEnv{m=om,k=ok,ci=oci,co=oco}) = I.createWinEnv ()
	(* realize the canvas widget *)
          fun realize {env=I.InEnv{m,k,ci,co}, win, sz as G.SIZE{wid,ht}} = let
		fun ciServer () = let
		      val msg = CML.sync ci
		      in
			case (I.msgBodyOf msg)
			 of (I.CI_Resize (G.RECT{wid,ht,...})) => let
			      val newSz = G.SIZE{wid=wid, ht=ht}
			      in
			        CML.send(newSzCh, newSz);
			        CML.sync(oci msg)
			      end
		          | _ => CML.sync(oci msg)
		        (* end case *);
			ciServer ()
		      end
                fun mkPipe (inc, outc) = let
                      fun loop () = loop (CML.sync (outc (CML.sync inc)))
                      in
                        XDebug.xspawn ("Canvas.mkPipe", loop)
                      end
                in
                  mkPipe(m,om); mkPipe(k,ok); mkPipe(oco,co);
                  SyncVar.iPut (winVar, win);
		  CML.spawn ciServer
                end (* realize *)
	(* the thread that manages the window's size state *)
	  fun sizeThread () = let
		val reqEvt = CML.recvEvt reqCh
		val newSzEvt = CML.recvEvt newSzCh
		fun loop sz = let
		      fun handleReq (DoRealize _) = sz
			| handleReq (GetSize cv) = (SyncVar.iPut(cv, sz); sz)
		      in
			loop (CML.select [
			    CML.wrap (reqEvt, handleReq),
			    newSzEvt
			  ])
		      end
		fun initLoop () = (case (CML.recv reqCh)
		       of (GetSize cv) => (SyncVar.iPut(cv, initSize); initLoop())
			| (DoRealize (arg,ack)) => (realize arg; SyncVar.iPut(ack,());loop (#sz arg))
		      (* end case *))
		in
		  initLoop ()
		end
          fun realize arg = let
                val ack = SyncVar.iVar()
                in
		  CML.send (reqCh, DoRealize (arg,ack));
                  SyncVar.iGet ack
                end
          val canvas = Canvas {
                  widget = W.mkWidget {
		      root = root,
                      args = fn () => {background = NONE},
		      boundsOf = fn () => bounds,
		      realize = realize
		    },
                  reqc = reqCh,
                  winvar = winVar
                }
          in
            XDebug.xspawn ("Canvas.sizeThread", sizeThread);
            (canvas, initSize, canvasInenv)
          end

    fun canvas (root, view, args) bounds = let
          val (canvas as Canvas{widget, reqc, winvar}, sz, env) =
		mkCanvas root bounds
          val canvas' = Canvas {
		  widget = Background.background (root, view, args) widget,
		  reqc = reqc,
		  winvar = winvar
		}
          in
            (canvas', sz, env)
          end

    fun widgetOf (Canvas{widget,...}) = widget

    fun sizeOf (Canvas{reqc, ...}) = let
	  val cv = SyncVar.iVar()
	  in
	    CML.send (reqc, GetSize cv);
	    SyncVar.iGet cv
	  end

    fun drawableOfCanvas (Canvas{winvar,...}) =
	  D.drawableOfWin (SyncVar.iGet winvar)

  (* set the background color of a canvas *)
    fun setBackground (Canvas{winvar, ...}) color =
	  EXeneWin.setBackground (SyncVar.iGet winvar) color

  (* set the cursor of a canvas *)
    fun setCursor (Canvas{winvar, ...}) cursor =
	  EXeneWin.setCursor (SyncVar.iGet winvar) cursor

  end; (* Canvas *)
