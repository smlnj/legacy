(* scrollbar.sml
 *
 * COPYRIGHT (c) 1994, 2002 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Scrollbar widget.
 *
 * CHANGE LOG
 *
 * 12 Mar 02 - Allen Stoughton - Changed widget so that, when it's
 * trying to communicate a value to the application on the scroll_evt
 * channel, it's still willing to process the application's setvals
 * operations.  (This was necessary to avoid deadlock.)  Also modified
 * widget to cope with resize events during ScrStart, ..., ScrMove, ...,
 * ScrEnd, sequences.  (Previously, the user would lose control of the
 * mouse, and a ScrEnd event wouldn't be generated.)
 *)

structure Scrollbar : SCROLLBAR =
  struct

    structure CML = CML
    structure W = Widget

    open CML Geometry EXeneBase Interact Widget ScrollView

    type scroll_state = {  (* since not exported from ScrollView; the variable "data" *)
      size : int,          (* ranges over this type *)
      coord : Geometry.point -> int,
      draw : int * int -> unit,
      move : int * int * int * int -> unit
    }

    val min = Int.min
    val max = Int.max

    datatype scroll_evt
      = ScrUp of real
      | ScrDown of real
      | ScrStart of real
      | ScrMove of real
      | ScrEnd of real

    datatype scrollbar = Scrollbar of {
        widget : Widget.widget,
        evt : scroll_evt CML.event,
        setvals : {top : real option, sz : real option} -> unit
      }

    datatype mseMsg
      = Grab of point 
      | Move of point
      | Ungrab of point
      | UpGrab of point
      | UpUngrab of point
      | DownGrab of point
      | DownUngrab of point

    datatype rqst
      = SetVals of {top : real option, sz : real option }
      | DoRealize of {
          env : in_env,
          win : window,
          sz : size
        }

    type scroll = {  (* the variable "me" ranges over this type *)
      curx : int,
      swid : int
    }

    val initSize = 1000
    val minSwid = 8

    fun newVals (me as {curx, swid}, size, arg) = (case arg
          of {top=NONE, sz=NONE} => me
           | {top=SOME top, sz=NONE} => {
                 curx=min(size-swid,max(0,floor(top * (real size)))),
                 swid=swid
               }
           | {top=NONE, sz=SOME sz} => {
                 curx=curx,
                 swid=min(size-curx,max(minSwid,ceil(sz * (real size))))
               }
           | {top=SOME top, sz=SOME sz} => let
               val sz'=min(size,max(minSwid,ceil(sz * (real size))))
               val top'=min(size-sz',max(0,floor(top * (real size))))
               in
                 {curx=top',swid=sz'}
               end
          (* end case *))

    fun mkScroll (root, dim, color, bg, {bounds_of, realize} : scroll_view) = let
          val _ = if dim < 4
                  then LibBase.failure {module="Scrollbar", func="mkScroll", msg="dim < 4"}
                  else ()
          val scr = screenOf root
          val msechan = channel ()  (* mouse to scrollbar *)
          val valchan = channel ()  (* scrollbar to user *)
          val reqchan = channel ()  (* user to scrollbar *)
          val mevt = recvEvt msechan
          val reqevt = recvEvt reqchan

          (* mouse reader *)
          fun mseProc m = let
                fun downLoop (movef, upf) = let
                      fun loop () = (case msgBodyOf (sync m)
                            of MOUSE_LastUp {pt,...} => upf pt
                             | MOUSE_Motion {pt,...} => (movef pt;loop ())
                             | _ => loop ()
                            (* end case *))
                      in
                        loop ()
                      end

                fun loop () = (case msgBodyOf (sync m)
                      of MOUSE_FirstDown {but=btn as MButton 1,pt,...} => (
                           send (msechan, UpGrab pt);
                           downLoop (fn _ => (), fn p => send(msechan, UpUngrab p));
                           loop ())
                       | MOUSE_FirstDown {but=btn as (MButton 2),pt,...} => (
                           send (msechan, Grab pt);
                           downLoop (
                             fn p => send(msechan, Move p),
                             fn p => send(msechan, Ungrab p)
                           );
                           loop ())
                       | MOUSE_FirstDown {but=btn as MButton 3,pt,...} => (
                           send (msechan, DownGrab pt);
                           downLoop (fn _ => (),fn p => send(msechan, DownUngrab p));
                           loop ())
                       | _ => loop ()
                      (* end case *))
                in
                  loop ()
                end

          val config = realize (root,color)

          fun realizeScroll {env=inenv, win, sz=winsz} me = let
                val InEnv{m,ci,...} = Interact.ignoreKey inenv
                val config = config (Drawing.drawableOfWin win)

                (* returns (me, data) *)
                fun reconfig ({curx, swid}, size, sz, redraw) = let
                      val data as {size=size', draw, ...} = config sz
                      val scale = 1.0 / real size
                      val size' = real size'
                      val curx' = floor((scale * real curx) * size')
                      val swid' = ceil((scale * real swid) * size')
                      in
                        if redraw then draw (curx',swid') else ();
                        ({curx=curx', swid=swid'}, data)
                      end

                (* returns (b, me', data'), where b is true iff scrollbar has been reconfigured *)
                fun handleCIEvt (evt, me : scroll, data as {size, draw, ...} : scroll_state) = (
                      case msgBodyOf evt
                        of CI_OwnDeath => (false, me, data)
                         | CI_Redraw _ => (draw (#curx me, #swid me); (false, me, data))
                         | CI_Resize (RECT{wid,ht,...}) => let
                             val (me', data') = reconfig (me, size, SIZE{wid=wid,ht=ht}, true)
                             in (true, me', data') end
                         | _ => (false, me, data)
                        (* end case *))

                fun handleReqEvt (SetVals arg,
                                  me as {curx, swid},  (* application's version *)
                                  me' as {curx = curx', swid = swid'},  (* scrollbar's version *)
                                  {size, move, ...} : scroll_state) = let
                      val me'' as {curx=curx'', swid=swid''} = newVals (me, size, arg)
                      in
                        if curx' <> curx'' orelse swid' <> swid''
                        then move (curx', swid', curx'', swid'')
                        else ();
                        me''
                      end
                  | handleReqEvt (DoRealize _, _, me, _) = me

                fun sendValAbortOnReq (v, f, me,
                                       data as {size, ...} : scroll_state) = let
                      val v = min(size - 1, max(0, v))
                      val valevt = sendEvt (valchan, f (real v / real size))
                      in select [
                              wrap (valevt, fn () => me),
                              wrap (reqevt, fn evt => handleReqEvt (evt, me, me, data))
                           ]
                      end

                (* xoff, me is widget's view;
                   x is new position of mouse pointer, relative to beginning of widget's window;
                   returns (xoff', me') *)
                fun moveSlide (xoff, me as {curx, swid}, {size, move, ...} : scroll_state, x) = let
                      val curx' = x - xoff
                      val maxx = size - swid
                      val (xoff', curx'') =
                            if curx' < 0
                              then (x - curx, 0)
                            else if curx' > maxx
                              then (x - curx, maxx)
                            else (xoff, curx')
                      in
                        if curx'' <> curx
                        then (move (curx, swid, curx'', swid);
                              (xoff', {curx=curx'',swid=swid}))
                        else (xoff', me)
                      end

                (* returns (me', data') *)
                fun handleMEvt (Grab p, me as {curx,swid}, data as {size, coord, ...}) = let
                    val x = coord p
                    val maxx = size - swid
                    val (xoff, me') =
                          if curx <= x andalso x < curx + swid
                          then ((x - curx), me)
                          else
                            let
                              val curx' = min (maxx, max (0, x - (swid div 2)))
                            in
                              (x - curx', #2(moveSlide (0 (* irrelevant *), me, data, curx')))
                            end

                    (* xoff, me are scrollbar's view, and tell us where mouse pointer was;
                       me' is what application has asked that scroll be;
                       returns xoff relative to me' *)
                    fun newxoff(xoff, me : scroll, me' : scroll) =
                          #curx me + xoff - #curx me'

                    (* me is application's view;
                       xoff, me' are scrollbar's view;
                       force is true iff insist on communication with application, even if
                         it makes request;
                       returns (xoff', me''), shared by application and scrollbar *)
                    fun sendVal (me, xoff, me', f, force, data as {size, ...}) = let
                          val v = #curx me'
                          val valevt = sendEvt (valchan, f (real v / real size))

                          fun loop (me, xoff, me', valevt) =
                                select [
                                    wrap (valevt, fn () => (xoff, me')),
                                    wrap (reqevt, fn evt => let
                                      val me'' = handleReqEvt (evt, me, me', data)
                                      val xoff' = newxoff(xoff, me', me'')
                                      in if force
                                         then let val v' = #curx me''
                                                  val valevt' =
                                                        sendEvt (valchan, f (real v' / real size))
                                              in loop(me'', xoff', me'', valevt') end
                                         else (xoff', me'')
                                      end)
                                  ]
                          in loop(me, xoff, me', valevt) end

                    (* xoffOpt is NONE when we've lost track of where mouse was - which is
                         when a CI_Resize has been processed;
                       returns (b, (xoffOpt', me')), where b is true iff an Ungrab has been processed *)
                    fun hMEvt (Ungrab x, xoffOpt, me, data) = (case xoffOpt
                          of NONE => (false,
                                      let val (_, me') =
                                                sendVal (me, 0 (* irrelevant *), me, ScrEnd, true, data)
                                      in (NONE (* irrelevant *), me') end)
                           | SOME xoff => let
                               val me' = #2(moveSlide (xoff, me, data, coord x))
                               in (false,
                                   let val (_, me'') =
                                             sendVal (me, 0 (* irrelevant *), me', ScrEnd, true, data)
                                   in (NONE (* irrelevant *), me'') end)
                               end
                          (* end case *))
                      | hMEvt (Move x, xoffOpt, me, data) = (case xoffOpt
                          of NONE => (true, (SOME(coord x - #curx me), me))
                           | SOME xoff => let
                               val (xoff', me') = moveSlide (xoff, me, data, coord x)
                               in if #curx me <> #curx me'
                                  then (true,
                                        let val (xoff'', me'') =
                                                  sendVal (me, xoff', me', ScrMove, false, data)
                                        in (SOME xoff'', me'') end)
                                  else (true, (SOME xoff', me'))
                               end
                          (* end case *))
                      | hMEvt (_, xoffOpt, me, _) = (true, (xoffOpt, me))  (* protocol error *)

                    (* xoffOpt is NONE when we've lost track of where mouse was - which is
                         when a CI_Resize has been processed;
                       returns (me', data') *)
                    fun loop (xoffOpt, me, data) = select [
                            wrap (reqevt, fn evt => let
                                    val me' = handleReqEvt (evt, me, me, data)
                                    in case xoffOpt
                                         of NONE => loop(NONE, me, data)
                                          | SOME xoff => loop(SOME(newxoff(xoff, me, me')), me', data)
                                       (* end case *)
                                    end),
                            wrap (ci, fn evt => let
                                    val (reconf, me', data') = handleCIEvt (evt, me, data)
                                    in if reconf
                                       then loop (NONE, me', data')
                                       else loop (xoffOpt, me', data')
                                    end),
                            wrap (mevt, fn evt => 
                              case hMEvt (evt, xoffOpt, me, data)
                                of (true, (xoffOpt, me)) => loop(xoffOpt, me, data)
                              |    (false, (_, me)) => (me, data)
                              (* end case *))
                          ]

                    val (xoff', me'') = sendVal(me, xoff, me', ScrStart, true, data)
                    in
                      loop (SOME xoff', me'', data)
                    end
                  | handleMEvt (UpGrab _, me, data) = let
                      fun hMEvt (UpUngrab x, me, data as {coord, ...}) =
                            (false, sendValAbortOnReq (coord x, ScrUp, me, data))
                        | hMEvt (_, me, _) = (true, me)  (* protocol error *)

                      fun loop (me, data) = 
                            select [
                              wrap (reqevt, fn evt => loop (handleReqEvt (evt, me, me, data), data)),
                              wrap (ci, fn evt => let
                                      val (_, me', data') = handleCIEvt (evt, me, data)
                                      in loop (me', data') end),
                              wrap (mevt, fn evt =>
                                case hMEvt (evt, me, data)
                                  of
                                     (true, me) => loop (me, data)
                                   | (false, me) => (me, data)
                                  (* end case *))
                            ]
                      in
                        loop (me, data)
                      end
                  | handleMEvt (DownGrab p, me, data) = let
                      fun hMEvt (DownUngrab x, me, data as {coord, ...}) =
                            (false, sendValAbortOnReq (coord x, ScrDown, me, data))
                        | hMEvt (_, me, _) = (true, me)  (* protocol error *)

                      fun loop (me, data) = 
                            select [
                              wrap (reqevt, fn evt => loop (handleReqEvt (evt, me, me, data), data)),
                              wrap (ci, fn evt => let
                                      val (_, me', data') = handleCIEvt (evt, me, data)
                                      in loop(me', data') end),
                              wrap (mevt, fn evt => 
                                case hMEvt (evt, me, data)
                                  of (true, me) => loop (me, data)
                                   | (false, me) => (me, data)
                                  (* end case *))
                            ]
                      in
                        loop (me, data)
                      end
                  | handleMEvt (_, me, data) = (me, data)   (* protocol error *)

              fun cmdProc (me, data) = cmdProc (select [
                      wrap (reqevt, fn evt => (handleReqEvt (evt, me, me, data), data)),
                      wrap (mevt, fn evt => handleMEvt (evt, me, data)),
                      wrap (ci, fn evt => let
                              val (_, me', data') = handleCIEvt (evt, me, data)
                              in (me', data') end)
                    ])
              in
                spawn (fn () => mseProc m);
                spawn (fn () => (cmdProc(reconfig (me, initSize, winsz, false)); ()));
                ()
              end (* realizeScroll *)
          fun initLoop vals = case recv reqchan
                 of SetVals arg => initLoop (newVals (vals, initSize, arg))
                  | DoRealize arg => realizeScroll arg vals
                 (* end case *)
          in
            spawn (fn () => initLoop {curx=0,swid=initSize});
            Scrollbar {
              widget = 
                mkWidget{
                  root=root,
                  args=fn () => {background = bg},
                  boundsOf=bounds_of dim, 
                  realize=fn arg => send(reqchan, DoRealize arg)
                }, 
              evt = recvEvt valchan,
              setvals = (fn arg => send (reqchan, SetVals arg))
            }
          end (* mkScroll *)

    val attrs = [
        (Attrs.attr_width,          Attrs.AT_Int,     Attrs.AV_Int 12),
        (Attrs.attr_background,     Attrs.AT_Color,   Attrs.AV_Str "gray"),
        (Attrs.attr_color,          Attrs.AT_Color,   Attrs.AV_NoValue)
      ]

    fun scrollbar scrollView (root,view,args) = let
          val attrs = W.findAttr (W.attrs(view,attrs,args))
          val sz = Attrs.getInt(attrs Attrs.attr_width)
          val bg = Attrs.getColor(attrs Attrs.attr_background)
          val color = (case Attrs.getColorOpt(attrs Attrs.attr_color)
                of NONE => bg
                 | SOME c => c
                (* end case *))
          in
            mkScroll(root, sz, color, SOME bg, scrollView)
          end

    val hScrollbar = scrollbar horzScrollbar
    val vScrollbar = scrollbar vertScrollbar

    fun mk scrollView root {sz, color} = let
          val color = (case color
                of SOME c => c
                 | NONE => colorOfScr (screenOf root) (CMS_Name "gray")
                (* end case *))
          in
            mkScroll (root, sz, color, NONE, scrollView)
          end

    val mkHScrollbar = mk horzScrollbar
    val mkVScrollbar = mk vertScrollbar

    fun widgetOf (Scrollbar {widget,...}) = widget
    fun evtOf (Scrollbar {evt,...}) = evt
    fun setVals (Scrollbar{setvals,...}) arg = setvals arg

  end (* ScrollBar *)
