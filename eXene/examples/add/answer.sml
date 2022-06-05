(* answer.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Answer : sig
    structure W : WIDGET

    type answerbox

    val dbtm : TraceCML.trace_module

    val mkAnswer : W.root * string -> answerbox
    val showAnswer : answerbox * W.EXB.window * int * int * string * int 
           -> unit SyncVar.ivar

  end =
  struct
    structure W = Widget

val dbtm = TraceCML.traceModule (XDebug.eXeneTM, "debug");

    open Geometry

    datatype answerbox = 
        A of {
          root : W.root,
          font : Font.font,
          lead : int
        }

    fun mkAnswer (root, fontname) = let
          val font = Font.openFont (W.displayOf root) fontname
          val {ascent,descent} = Font.fontHt font
          val lead = ascent + descent
          in
            A{root=root,font=font,lead=lead}
          end

    fun ansBox (ansbox,arg1,arg2,signString,ans) = let
          val A{root,font,lead} = ansbox
          val s1 = Int.toString arg1
          val s2 = signString^"   "^(Int.toString arg2)
          val s3 = Int.toString ans
          val l1 = Font.textWidth font s1
          val l2 = Font.textWidth font s2
          val l3 = Font.textWidth font s3
          val wd = Int.max(l1,Int.max(l2,l3))
          val s1y = lead
          val s2y = s1y + lead
          val liney = s2y + 4
          val s3y = liney + lead
          val bnds = {x_dim = W.fixDim wd, y_dim = W.fixDim (s3y + 4)}
          val (canvas,sz,inenv) = Canvas.mkCanvas root bnds
          val Interact.InEnv{ci,...} = Interact.ignoreInput inenv
          val black = W.EXB.blackOfScr (W.screenOf root)
          val pen = Drawing.newPen [Drawing.PV_Foreground black]
          fun main sz = let
                val draww = Canvas.drawableOfCanvas canvas
                val drawString = Drawing.drawString draww pen font
                val drawLine = Drawing.drawLines draww pen
                fun draw (SIZE{wid,ht}) = (
TraceCML.trace(dbtm,fn () => ["in draw\n"]);
                      Drawing.clearDrawable draww;
                      drawString (PT{x=wid-l1,y=s1y},s1);
                      drawString (PT{x=wid-l2,y=s2y},s2);
                      drawLine [PT{x=0,y=liney},PT{x=wid,y=liney}];
                      drawString (PT{x=wid-l3,y=s3y},s3)
                    )
                fun loop sz =
                      case Interact.msgBodyOf(CML.sync ci) of
                        Interact.CI_Redraw _ => (draw sz;loop sz)
                      | Interact.CI_Resize (RECT{wid,ht,...}) => 
                          loop (SIZE{wid=wid,ht=ht})
                      | _ => loop sz
                in loop sz end
          in
            CML.spawn (fn () => main sz);
            Canvas.widgetOf canvas
          end
          
    fun showAnswer (ansbox,win,arg1,arg2,signString,ans) = let
open TraceCML
val _ = trace(dbtm,fn () => ["in showAnswer\n"])
          val A{root,...} = ansbox
          val iv = SyncVar.iVar ()
          val iv' = SyncVar.iVar ()
          val answer = ansBox (ansbox,arg1,arg2,signString,ans)
val _ = trace(dbtm,fn () => ["created ansbox\n"])
          val cancel = Button.mkTextCmd root {
              action = fn () => SyncVar.iPut (iv',()),
              rounded = true,
              label = "Cancel",
              foregrnd = NONE,
              backgrnd = NONE
            }
          val layout = Box.widgetOf(Box.mkLayout root (Box.VtCenter [
              Box.HzCenter [
                Box.Glue {nat=3,min=3,max=NONE},
                Box.WBox answer,
                Box.Glue {nat=3,min=3,max=NONE}
              ],
              Box.Glue {nat=5,min=5,max=NONE},
              Box.HzCenter [
                Box.Glue {nat=5,min=5,max=NONE},
                Box.WBox (Shape.mkRigid (Button.widgetOf cancel)),
                Box.Glue {nat=5,min=5,max=NONE}
              ],
              Box.Glue {nat=5,min=5,max=NONE}
            ]))
val _ = trace(dbtm,fn () => ["created layout\n"])
          val shell = Shell.mkTransientShell win (layout,NONE,
                       {win_name = SOME "Answer", icon_name = SOME "Answer" })
val _ = trace(dbtm,fn () => ["created shell\n"])
          fun main () = (
trace(dbtm,fn () => ["main await event\n"]);
                CML.select (map SyncVar.iGetEvt [iv,iv']);
trace(dbtm,fn () => ["main got event; destroy shell\n"]);
                Shell.destroy shell
              )
          in
trace(dbtm,fn () => ["spawn main\n"]);
            CML.spawn main;
trace(dbtm,fn () => ["init shell\n"]);
            Shell.init shell;
trace(dbtm,fn () => ["return iv\n"]);
            iv
          end
  end
