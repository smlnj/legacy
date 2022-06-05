signature VIEW = sig

  structure VG : ATTR_GRAPH
  structure W : WIDGET
  structure VF : VIEW_FONT

  type view

  val mkView : VF.font_server * W.root -> VG.graph -> view
  val widgetOf : view -> W.widget

end (* VIEW *)

structure View : VIEW = struct

  structure V = GrViewer
  structure VG = VGraph
  structure F = Frame
  structure SB = Scrollbar
  structure SBL = ScrollLayout
  structure W = Widget
  structure VF = ViewFont
  open CML Geometry

  datatype view = View of {
    widget : W.widget,
    viewer : V.viewer
  }

  type vport = {min : int, sz : int, total : int}
  type state = vport * vport

  val scrollBarSize = 10

  fun mkView (fs, root) graph = let
    val viewer = V.mkViewer (fs, root) graph 
    val vp_evt = V.evtOf viewer
    val hsb = SB.mkHScrollbar root {color=NONE,sz=scrollBarSize}
    val hsb_evt = SB.evtOf hsb
    val vsb = SB.mkVScrollbar root {color=NONE,sz=scrollBarSize}
    val vsb_evt = SB.evtOf vsb
    val scr = W.screenOf root
    val black = W.EXB.blackOfScr scr
    val widget = SBL.mkSBLayout root {
        widget = V.widgetOf viewer,
        hsb = SOME {
          sb = F.widgetOf (F.mkFrame {
                 color=SOME black,
                 width=1,
                 widget=Box.widgetOf (Box.mkLayout root (Box.VtCenter[
                   Box.Glue{nat=1,min=1,max=SOME 1},
                   Box.WBox(SB.widgetOf hsb),
                   Box.Glue{nat=1,min=1,max=SOME 1}
                 ]))
               }),
          pad = 1, 
          top = false
        },
        vsb = SOME {
          sb = F.widgetOf (F.mkFrame {
                 color=SOME black,
                 width=1,
                 widget=Box.widgetOf (Box.mkLayout root (Box.HzCenter[
                   Box.Glue{nat=1,min=1,max=SOME 1},
                   Box.WBox(SB.widgetOf vsb),
                   Box.Glue{nat=1,min=1,max=SOME 1}
                 ]))
               }),
          pad = 1, 
          left = false
        }
      }
    val initstate = ({min=0,sz=0,total=0}, {min=0,sz=0,total=0})
 
    fun handleHSB (SB.ScrUp r, state : state) = state
      | handleHSB (SB.ScrDown r, state) = state
      | handleHSB (SB.ScrEnd r, state as ({min,sz,total},v)) =
          let
            val min' = floor(r * (real total))
          in
            if min' <> min
              then (V.setHorzView viewer min';({min=min',sz=sz,total=total}, v))
              else state
          end
      | handleHSB (_, state) = state

    fun handleVSB (SB.ScrUp r, state : state) = state
      | handleVSB (SB.ScrDown r, state) = state
      | handleVSB (SB.ScrEnd r, state as (h, {min,sz,total})) =
          let
            val min' = floor(r * (real total))
          in
            if min' <> min
              then (V.setVertView viewer min';(h, {min=min',sz=sz,total=total}))
              else state
          end
      | handleVSB (_, state) = state

    fun handleVP ({horz=V.VDIM hz, vert=V.VDIM vt}, (h,v)) = let
      fun change (sb, {min,sz,total} : vport) = let
        val total = real total
      in
        SB.setVals sb {sz=SOME ((real sz)/total), top=SOME((real min)/total)}
      end
    in
      if hz <> h then change (hsb, hz) else ();
      if vt <> v then change (vsb, vt) else ();
      (hz,vt)
    end

    fun loop state =
      loop (select [
        wrap (vp_evt, fn e => handleVP(e,state)),
        wrap (hsb_evt, fn e => handleHSB(e,state)),
        wrap (vsb_evt, fn e => handleVSB(e,state))
      ])
  in
    spawn (fn () => loop initstate);
    View {
      widget = Box.widgetOf widget,
      viewer = viewer
    }
  end

  fun widgetOf (View{widget,...}) = widget

end (* View *)
