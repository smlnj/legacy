(* add.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Add : sig val add : string -> unit end =
  struct

    open CML Geometry Widget
    
    datatype command = NewGame of Calc.difficulty | NewOp of Calc.function

    fun counter (ch, setl) = let
          fun loop cnt = (
                accept ch;
                setl (Makestring.intToStr cnt);
                loop (cnt+1)
              )
          in loop 1 end

    fun main server = let
      val root = mkRoot server
      val scr = screenOf root
      fun doGC () = System.Runtime.gc 2

      val calcBox = Calc.mkCalc root
      val boxEvt = Calc.eventOf calcBox

      val rounds = 3
      val scene = Scene.mkScene root rounds

      fun quitGame () = (delRoot root; RunCML.shutdown())
      val quitButton = Button.mkTextCmd root {
          action = quitGame,
          rounded = false,
          label = "Quit",
          foregrnd = NONE,
          backgrnd = NONE
        }

      val wonChan = channel ()
      fun gameWon () = send(wonChan,())

      val newChan = channel ()
      val newGame = receive newChan

      val gamesWonLabel = Label.mkLabel root {
          label = "Games won :",
          font = NONE,
          foregrnd = NONE,
          backgrnd = NONE,
          align = HRight
        }

      val gamesWonCnt = Label.mkLabel root {
          label = "    0",
          font = NONE,
          foregrnd = NONE,
          backgrnd = NONE,
          align = HRight
        }

      val singleButton = Button.mkTextCmd root {
          action = fn () => send(newChan, NewGame Calc.Single),
          rounded = false,
          label = "Single",
          foregrnd = NONE,
          backgrnd = NONE
        }

      val easyButton = Button.mkTextCmd root {
          action = fn () => send(newChan, NewGame Calc.Easy),
          rounded = false,
          label = "Easy",
          foregrnd = NONE,
          backgrnd = NONE
        }

      val mediumButton = Button.mkTextCmd root {
          action = fn () => send(newChan, NewGame Calc.Medium),
          rounded = false,
          label = "Medium",
          foregrnd = NONE,
          backgrnd = NONE
        }

      val hardButton = Button.mkTextCmd root {
          action = fn () => send(newChan, NewGame Calc.Hard),
          rounded = false,
          label = "Hard",
          foregrnd = NONE,
          backgrnd = NONE
        }

      val opItems = 
        map (fn (f,ison) => (Calc.funcString f,f,Active ison)) Calc.functionList
      val opList = TextList.mkHList root {
          items = opItems,
          mode = TextList.OneSet,
          foregrnd = NONE,
          backgrnd = NONE
        }

      fun oplisten () = let
        open TextList
        val opevt = evtOf opList
        fun loop () =
          loop(case sync opevt of
            Set f => send(newChan, NewOp f)
          | _ => ()
          )
        in
          loop ()
        end

      val buttons = Box.widgetOf(Box.mkLayout root (Box.VtCenter[
          Box.Glue {nat=5,min=5,max=SOME 5},
          (Box.HzCenter[
            Box.Glue {nat=10,min=5,max=SOME 20},
            Box.WBox (Shape.mkRigid (Button.widgetOf quitButton)),
            Box.Glue {nat=10,min=5,max=SOME 10},
            Box.WBox (Shape.mkRigid (Button.widgetOf singleButton)),
            Box.Glue {nat=10,min=5,max=SOME 10},
            Box.WBox (Shape.mkRigid (Button.widgetOf easyButton)),
            Box.Glue {nat=10,min=5,max=SOME 10},
            Box.WBox (Shape.mkRigid (Button.widgetOf mediumButton)),
            Box.Glue {nat=10,min=5,max=SOME 10},
            Box.WBox (Shape.mkRigid (Button.widgetOf hardButton)),
            Box.Glue {nat=10,min=5,max=SOME 10},
            Box.WBox (Shape.mkRigid (Frame.widgetOf(Frame.mkFrame
              {color = SOME (EXeneBase.blackOfScr scr),
               width = 1,
               widget = TextList.widgetOf opList}
            ))),
            Box.Glue {nat=10,min=5,max=SOME 10},
            Box.WBox (Shape.mkRigid (Label.widgetOf gamesWonLabel)),
            Box.WBox (Shape.mkRigid (Label.widgetOf gamesWonCnt)),
            Box.Glue {nat=10,min=5,max=NONE}
          ]),
          Box.Glue {nat=5,min=5,max=SOME 5}
        ]))

      val (calcWidget, calcKbd) = 
        filterKey(Shape.fixSize (Calc.widgetOf calcBox, SIZE{wid=300,ht=400}))

      val layout = Box.widgetOf(Box.mkLayout root (Box.VtCenter [
          Box.HzCenter [
            Box.WBox calcWidget,
            Box.WBox (Divider.mkVertDivider root {color=NONE,width=1}),
            Box.WBox (Scene.widgetOf scene)
          ],
          Box.WBox (Divider.mkHorzDivider root {color=NONE,width=1}),
          Box.WBox buttons
        ]))
      val (layout,kbd) = filterKey layout
      val shell = Shell.mkShell (layout, NONE,
        {win_name = SOME "Arith", icon_name = SOME "Arith" })

      fun main opfn = let
        fun loop (0,opfn,d) = (gameWon ();Calc.reset calcBox; Scene.wave; idle opfn)
          | loop (i,opfn,d) = let
              fun handleBox Calc.Right = (Scene.up scene; loop(i-1,opfn,d))
                | handleBox Calc.Wrong = 
                    (Scene.dive scene; Calc.reset calcBox; idle opfn)
              fun handleGame (NewGame d') = startGame(d',opfn)
                | handleGame (NewOp opfn') = startGame(d,opfn')
              in
                select [
                  wrap(boxEvt, handleBox),
                  wrap(newGame, handleGame)
                ]
              end

        and idle opfn = 
              case sync newGame of
                NewGame d => startGame(d,opfn)
              | NewOp opfn' => idle opfn'

        and startGame (d,opfn) = (
              Calc.startGame calcBox (d,opfn);
              Scene.start scene;
              loop (rounds,opfn,d)
            )
        in
          startGame (Calc.Easy,opfn)
        end

        fun waitEvts [] = []
          | waitEvts evtl = let
              fun mk (e,i) = (i,NONE,wrap(e, fn v => (v,i)))
              fun update ([],_) = []
                | update ((item as (j,_,e))::r,(v,i)) =
                    if i = j then (j,SOME v,e)::r
                    else item::(update(r,(v,i)))
              fun getVals ([],l) = l
                | getVals ((_,SOME v,_)::r,l) = getVals(r,v::l)
                | getVals (_::r,l) = getVals(r,l)
              fun getLeft ([],l) = l
                | getLeft ((_,NONE,e)::r,l) = getLeft(r,e::l)
                | getLeft (_::r,l) = getLeft(r,l)
              fun waitAll l =
                    case getLeft (l,[]) of
                      [] => rev(getVals (l,[]))
                    | l' => waitAll(update(l,sync(choose l')))
              in
                waitAll(rev(#1(revfold (fn (e,(l,i)) => (mk(e,i)::l,i+1)) evtl ([],0))))
              end
              
        fun keylisten (kbd,calcKbd) = let
              open Interact
              val evtl = waitEvts [kbd,calcKbd]
              val (keyevt,_) = hd evtl
              val (ckeyevt,keychan) = hd (tl evtl)
              fun sink () = (sync ckeyevt; sink())
              val lookup = lookupString defaultTranslation
              fun transKey(KEY_Press key) = ((SOME(lookup key)) handle _ => NONE)
                | transKey _  = NONE
              fun handled c =
                    case CType.toLower c of
                      "s" => (send(newChan, NewGame Calc.Single); true)
                    | "e" => (send(newChan, NewGame Calc.Easy); true)
                    | "m" => (send(newChan, NewGame Calc.Medium); true)
                    | "h" => (send(newChan, NewGame Calc.Hard); true)
                    | "q" => (quitGame();true)
                    | "+" => (TextList.setChosen opList [(0,true)]; true)
                    | "-" => (TextList.setChosen opList [(1,true)]; true)
                    | "*" => (TextList.setChosen opList [(2,true)]; true)
                    | "x" => (TextList.setChosen opList [(2,true)]; true)
                    | _ => false
              fun loop () = let
                    val keymsg = sync keyevt
                    in 
                      (case transKey(msgBodyOf keymsg) of
                        NONE => ()
                      | SOME c => if handled c then ()
                                  else send(keychan,keymsg)
                      );
                      loop ()
                    end
              in
                spawn sink;
                loop ()
              end
                    
      in
        spawn (fn() => counter(wonChan,Label.setLabel gamesWonCnt));
        spawn (fn () => keylisten(kbd,calcKbd));
        Shell.init shell;
        spawn oplisten;
        main Calc.Add 
      end

    fun add server = RunCML.doit(fn () => main server, SOME 20)
  end

fun doit s = Add.add s

fun main (prog::server::_,_) = doit server
  | main _ = doit ""
