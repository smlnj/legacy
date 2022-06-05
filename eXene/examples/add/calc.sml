(* calc.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

signature CALC =
  sig
    structure W : WIDGET

    datatype answer = Right | Wrong
    datatype difficulty = Single | Easy | Medium | Hard
    datatype function = Add | Subtract | Multiply
    val functionList : (function * bool) list
    val funcString : function -> string

    type calc

    val mkCalc : W.root -> calc
    val startGame : calc -> (difficulty * function) -> unit
    val reset : calc -> unit
    val widgetOf : calc -> W.widget
    val eventOf : calc -> answer CML.event

  end

structure Calc : CALC =
  struct
    structure W = Widget

    open Geometry W Box
    
    fun windowOf w = let
          val winv = SyncVar.iVar ()
          val rf = realizeFn w
          fun rfn (arg as {win,...}) = (SyncVar.iPut(winv,win); rf arg)
          val w' = mkWidget{
		  root= rootOf w,
		  boundsOf= boundsFn w,
		  realize= rfn
		}
          in
            (w',winv)
          end

    val fontname = 
      "-b&h-lucidatypewriter-bold-r-normal-sans-24-240-75-75-m-140-iso8859-1"
      (* "-sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1" *)

    datatype answer = Right | Wrong
    datatype difficulty = Single | Easy | Medium | Hard
    fun diffString Easy = "Easy"
      | diffString Medium = "Medium"
      | diffString Hard = "Hard"
      | diffString Single = "Single"
    datatype function = Add | Subtract | Multiply
    fun funcString Add = " +"
      | funcString Subtract = " -"
      | funcString Multiply = " x"
    fun funcOp Add = Int.+
      | funcOp Subtract = Int.-
      | funcOp Multiply = Int.*
    val functionList = [(Add,true), (Subtract,false), (Multiply,false)]

    datatype rqst = Start of (difficulty * function) | Reset

    datatype calc = CALC of {
      widget : widget,
      reqChan : rqst CML.chan,
      answerEvt : answer CML.event
    }

    fun fixVert w = let
      val SIZE{ht,...} = natSize w
      val ydim = fixDim ht
      fun bndfn bounds_of = let
        val {x_dim,y_dim} = bounds_of ()
        in
          {x_dim=x_dim,y_dim=ydim}
        end
      in
        Shape.mkShape {
          widget=w, 
          bounds_fn = bndfn,
          resize_fn = fn _ => true
        }
      end

    fun getSeed () = Time.toReal(Time.now())

    fun genVals (random,d) = let
      val maxrange = 
        case d of
          Single => 9
        | Easy => 99
        | Medium => 999
        | Hard => 9999

      fun gen () = let
        val v1 = Rand.range (1,maxrange) (random())
        val v2 = Rand.range (1,maxrange) (random())
        in
          if v1 < v2 then (v2,v1) else (v1, v2)
        end
      in
        gen
      end

    fun doInput (kbd,label,anschan) = let
      open Interact
      val lookup = lookupString defaultTranslation
      fun isErase c = c = #"\^H"
      fun isNewline c = (c = #"\^M") orelse (c = #"\^J")

      fun addDigit (c,s) = let
        val s' = String.str c ^ s
        in
          Label.setLabel label (Label.Text s');
          s'
        end

      fun erase "" = ""
        | erase s = let
          val s' = substring(s,1,size s - 1)
          in
            Label.setLabel label (Label.Text s'); s'
          end

      val (kbdevt,_) = CML.sync kbd
      fun restart cv = let
        fun handleKbd(KEY_Press key,s) = (let
            val c = String.sub(lookup key, 0)
            in
              if isErase c then erase s
              else if isNewline c andalso size s > 0 then (
                (SyncVar.iPut(cv, valOf(Int.fromString s)))
		  handle _ => SyncVar.iPut(cv,0);
                initLoop())
              else if Char.isDigit c then addDigit(c,s)
              else s
            end handle _ => s)
              
          | handleKbd(_,s) = s
            
        fun loop s = 
          CML.select[
            CML.wrap(CML.recvEvt anschan, restart),
            CML.wrap(kbdevt, fn k => loop(handleKbd(msgBodyOf k,s)))
          ]
      in
        Label.setLabel label (Label.Text "");
        loop ""
      end

      and initLoop () = 
        CML.select[
          CML.wrap(CML.recvEvt anschan, restart),
          CML.wrap(kbdevt, fn _ => initLoop())
        ]
      in
        initLoop ();
        ()
      end

    fun mkCalc root = let
      val reqChan = CML.channel ()
      val answerChan = CML.channel ()
      val ansChan = CML.channel ()
      val seed = getSeed ()
      val random = Rand.mkRandom seed
      val ansbox = Answer.mkAnswer (root,fontname)
      val val1 = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val val2 = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val sign = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "  ",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val answer = Label.mkLabel root {
          align = HRight,
          font = SOME fontname,
          label = "",
          foregrnd = NONE,
          backgrnd = NONE
        }
      val layout = mkLayout root (HzCenter[
         Glue{nat=10,min=10,max=SOME 20},
         VtCenter [
           WBox (fixVert (Label.widgetOf val1)),
           HzCenter[
             WBox (Shape.mkRigid (Label.widgetOf sign)),
             WBox (fixVert (Label.widgetOf val2))
           ],
           Box.WBox (Divider.mkHorzDivider root {color=NONE,width=2}),
           WBox (fixVert (Label.widgetOf answer))
         ],
         Glue{nat=10,min=10,max=SOME 20}
        ])
      val (layout,kbd) = filterKey (widgetOf layout)
      val (layout,win) = windowOf layout

      fun resetAns ans = CML.send(ansChan, ans)

      fun fire NONE = ()
        | fire (SOME cv) = SyncVar.iPut(cv,())

val dbtm = Answer.dbtm
val trace = TraceCML.trace

      fun startGame (d,f) = let
        val getVals = genVals (random,d)
        val evalFn = funcOp f
        val signString = funcString f
        fun doReq (Start d) = startGame d
          | doReq Reset = calc NONE
        fun round () = let
              val (v1,v2) = getVals ()
              val ansv = SyncVar.iVar ()
              val ans = evalFn(v1,v2)
              fun chk v =
                    if ans = v then (CML.send(answerChan,Right);round ()) 
                    else let
                      val w = SyncVar.iGet win
val _ = trace(dbtm,fn () => ["calc show answer\n"])
                      val av = Answer.showAnswer(ansbox,w,v1,v2,signString,ans)
                      in
trace(dbtm,fn () => ["answer up\n"]);
                        CML.send(answerChan,Wrong); 
                        calc (SOME av)
                      end
              in
                Label.setLabel val1 (Label.Text(Int.toString v1));
                Label.setLabel val2 (Label.Text(Int.toString v2));
                resetAns ansv;
                CML.select [
                  CML.wrap(CML.recvEvt reqChan, doReq),
                  CML.wrap(SyncVar.iGetEvt ansv, chk)
                ]
              end
        in
          Label.setLabel sign (Label.Text signString);
          round ()
        end

      and calc cvo = let
            fun loop () = 
              case CML.recv reqChan of
                Start d => (trace(dbtm,fn () => ["fire cv\n"]);fire cvo; startGame d)
              | Reset => loop ()
            in
              Label.setLabel val1 (Label.Text "");
              Label.setLabel val2 (Label.Text "");
              Label.setLabel answer (Label.Text "");
              loop ()
            end
      in
(* TraceCML.traceOn Answer.dbtm; *)
        CML.spawn (fn () => doInput(kbd,answer,ansChan));
        CML.spawn (fn () => calc NONE);
        CALC{
          widget = layout,
          reqChan = reqChan,
          answerEvt = CML.recvEvt answerChan
        }
      end

    fun startGame (CALC{reqChan,...}) d = CML.send(reqChan,Start d)
    fun reset (CALC{reqChan,...}) = CML.send(reqChan,Reset)
    fun widgetOf (CALC{widget,...}) = widget
    fun eventOf (CALC{answerEvt,...}) = answerEvt

  end
