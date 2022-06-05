(* manager.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * tactic tree manager. 
 *)

functor TTreeManager(structure S : TTREE_SUPPORT) : TTREE_MANAGER = 
struct 

    structure S = S 
    structure TT = TTree(S)

    open CML Geometry Widget Box Scrollbar Button TextWidget 

    exception TacticParseError 
    exception TacticApplicationError of exn 
    exception FailedValidation of exn 
    exception ExtractDoesNotAchieve
    exception TacticTreeIncomplete

    datatype ttree_widget = TTreeWidget of Widget.widget * exn CML.event
    type ttree_state = TT.ttree_state

    val pause = TIME{sec=0,usec=500000}
    val display_border_width = 3

    val h_glue = Glue {nat=5, min=5, max= NONE}
    val v_glue = Glue {nat=5, min=1, max=NONE}
    val stretch_glue = Glue {nat=5, min=1, max=NONE}
    val h_space = Glue {nat=5, min=5, max=SOME 40}

    val button_ht = 25
    val char_wid = 10
    val min_button_chars = 3
    val max_button_chars = 20
    val tactic_bar_ht = 26
    val text_ht = 500
    val text_wid = 500
    val view_label_wid = 190

    fun mkTTreeWidget (ttree,menu_extension,root) = 
     let 
	val msg_ch = channel ()
        val blackc = EXB.blackOfScr (Widget.screenOf root) 

        fun mkDisplayBox w  = 
	      Frame.widgetOf(Frame.mkFrame {
		  widget = Shape.mkRigid w,
		  color = SOME blackc,
		  width = display_border_width
		})

	fun make_button (l,a)   = let 
	    val wid  = (min(max (min_button_chars,(String.length l)),max_button_chars)) 
		        * char_wid
	    in 
		WBox(mkDisplayBox(Shape.fixSize (
		  Button.widgetOf(
                   mkTextCmd root 
		    {action = a,
		     rounded = false, 
		     backgrnd = NONE, 
		     foregrnd = NONE, 
		     label = l}), 
                  Geometry.SIZE{wid=wid,ht=button_ht})))
	    end 

(* text *) 
	val text = mkTextWidget root {rows = text_ht, cols = text_wid}


	fun clear_screen start = TextWidget.clearToEOS text (ChrCrd{col=1,row=start})

	fun clear_line line_num = TextWidget.clearToEOL text (ChrCrd{col=1,row=line_num})
	fun put_strings format_func (text_bloc,(start_line,last_line))   = 
	    let fun write_strings ([],i) = ()
	          | write_strings (text::text_list,i) = 
		       if i > last_line
			  then () 
			  else (clear_line i; 
				format_func {at = ChrCrd{col = 1, row = i},text = text}; 
   			        write_strings(text_list,i+1))
             in write_strings(text_bloc,start_line) end 

        fun myHighLight tw {at= a ,text=t} = 
             if t = "" 
             then TextWidget.writeText tw {at= a ,text=t}
             else TextWidget.highlightText tw {at= a ,text=t}

	fun display []  =  ()
	  | display (TT.DoNothing::disp_instruct) = display disp_instruct
	  | display ((TT.DisplayText (text_bloc, form, (start,stop)))::disp_instruct) = 
	    let
		val write_text = 
	           case form 
		       of TT.Plain => put_strings (TextWidget.writeText text)
		        | TT.Highlight => put_strings (myHighLight text)
	    in 
		write_text (text_bloc,(start,stop));
		display disp_instruct
	    end

	  | display ((TT.ClearFrom start)::disp_instruct) = 
	              (clear_screen start; display disp_instruct)
	  | display ((TT.InsertText (text_bloc, form, (start,stop)))::disp_instruct) = 
	    let
		val write_text = 
	           case form 
		       of TT.Plain => put_strings (TextWidget.insertText text)
		        | TT.Highlight => put_strings (myHighLight text)
	    in 
		write_text (text_bloc,(start,stop));
		display disp_instruct
	    end

          | display ((TT.Scroll (start,num_lines))::disp_instruct) =
	        (if num_lines > 0 
		 then TextWidget.scrollDown text {from=start,nlines = num_lines}
		 else if num_lines < 0 
			 then (TextWidget.deleteLns 
                                      text 
                                     {lnum = start + num_lines, nlines = (abs num_lines)};
			       TextWidget.scrollUp 
                                      text 
                                      {from = start, nlines =num_lines})
		          else (); 
(* debugging stuff
	        (CIO.print ("Scroll: starting line of text to be scrolled -> " 
                            ^ (Integer.makestring start) ^ 
			    "\n number of lines to be scrolled " ^ 
			    (Integer.makestring num_lines) ^ 
                            "* <0 means scroll up >0 means scroll down\n"));
*)
		display disp_instruct)


(* Views *) 

	fun view_mode_widget () = let 

	    fun current_view (_,ref (TT.Subtree)) = "Subtree Highlighted"
	      | current_view (_,ref (TT.Node)) = "Node Highlighted"
	      | current_view (_,ref (TT.Local)) = "Local"
	
	    val	view_mode_label = Label.mkLabel root {label = (current_view ttree),
						      font = NONE, 
						      foregrnd = NONE,
						      backgrnd = NONE,
						      align = W.HCenter}

	    val set_label = Label.setLabel view_mode_label
	    val (view_label_widget,view_mode_evt) = 
		Widget.filterMouse (Label.widgetOf view_mode_label)

	    fun view_label_server (view_mode_evt) = let
		val (view_evt,_) = sync view_mode_evt

		fun check_button_pressed ({but,pt,scr_pt,...}) = let 
		    val but1_pressed = Widget.Interact.mbut1IsSet (W.Interact.mkButState [but])

		in
		    if but1_pressed 
			then (display (TT.do_action (ttree, TT.ChangeMode));
			      (set_label (current_view ttree)))
		    else ()

		end

		and loop () = let 
		    open Widget Interact
		in
			case (msgBodyOf (sync view_evt))
			  of (MOUSE_FirstDown button) => ((check_button_pressed button); loop ())
			   | _ =>  loop ()
		end
	    in
		loop ()
	    end
	in
	    spawn (fn () => view_label_server (view_mode_evt));
	    view_label_widget
	end
(*******) 

        fun apply_tactic s tac = 
	     (TT.do_action(ttree,TT.ApplyTactic(tac,s))
             ) handle e => (CML.send (msg_ch, TacticApplicationError e);[])

(* Tactic Entry Line *) 
	(* apply_string is a real hack *) 
        fun apply_string s = 
	    (let val f = ((System.Compile.use_stream 
			       (open_string ("tactic_ref := " ^ s ^ ";"));
			 !(S.tactic_ref)) handle _ => raise TacticParseError)
            in 
		TT.do_action(ttree,TT.ApplyTactic(f,s))
	    end ) handle TacticParseError => (CML.send (msg_ch,TacticParseError);[])
                       | e  => (CML.send (msg_ch, TacticApplicationError e);[])



	val string_edit = FieldEdit.mkFieldEdit root {foregrnd = NONE, backgrnd = NONE, 
						      initval = "", minlen =50}
	val get_string = fn () => FieldEdit.getString string_edit
	val set_string = FieldEdit.setString string_edit 

        val string_widget = 
	     (* mkLayout root *)
	       HzCenter [
		   make_button("clear",fn () => set_string ""), 
		   WBox(mkDisplayBox (FieldEdit.widgetOf string_edit)),
		   make_button("apply",fn () => display(apply_string (get_string ()))),
		   h_glue]

(* top line of buttons *) 
	val button_bar = 
	      HzTop [
		 WBox(mkDisplayBox(Shape.fixSize 
		      ((view_mode_widget() ,Geometry.SIZE{wid=view_label_wid,ht=button_ht})))), 
		 make_button("elision",fn () => display (TT.do_action (ttree,TT.Elide))),
		 h_space,
		 make_button("delete",fn () => display (TT.do_action (ttree,TT.Delete))),
		 h_space,
		 make_button("root",fn () => display (TT.do_action (ttree,TT.MoveToTop))), 
		 make_button("parent",(fn () =>  
				       display (TT.do_action (ttree,TT.MoveToParent)))),
		 make_button("first child",(fn () =>  
					    display (TT.do_action (ttree,TT.MoveToChild)))), 
		 make_button("prev",fn () =>  display (TT.do_action (ttree,TT.MoveLeft))), 
		 make_button("next",fn () =>  display (TT.do_action (ttree,TT.MoveRight))),
		 h_glue,
		 make_button("quit",fn () => (sync (timeout pause);
					      delRoot root; 
					      RunCML.shutdown()))
		]

(* tactic button bar *) 
	fun mk_tactic_bar [] = []
          | mk_tactic_bar tactic_menu = 
	   let 
               val wid = 
	       (min (max_button_chars,
                     (fold max 
                           (map (fn (n,_) => String.length n) tactic_menu) 
			   min_button_chars))) * char_wid
	       fun make_tact_button tactic_label  tac = 
		   WBox(mkDisplayBox(Shape.fixSize (Button.widgetOf(
                   mkTextCmd root 
		    {action = fn () => display (apply_tactic tactic_label tac),
		     rounded = false, 
		     backgrnd = NONE, 
		     foregrnd = NONE, 
		     label = tactic_label}), Geometry.SIZE{wid=wid,ht=button_ht})))
	       fun mk_bar [] = [v_glue]
		|  mk_bar ((n,t)::l) =
		      [make_tact_button n t] @ (mk_bar l)
	       fun split_list (l,n) = 
		   (rev (nthtail (rev l,(length l) - n)),nthtail (l,n)) 
                    handle NthTail => (l,[])
		val (tm_hd,tm_tl) = split_list (tactic_menu,tactic_bar_ht) 
	    in (VtLeft (mk_bar tm_hd))::(mk_tactic_bar tm_tl) end
	    
	val tactic_bar = HzTop (mk_tactic_bar (S.tactic_menu @ menu_extension))
	
	fun mouse_loop evt = let
	    val (m_evt,m_chan) = sync evt
	    fun tw_mouse_server ({but, pt,scr_pt,...}) = let

	   	val but1_pressed = Widget.Interact.mbut1IsSet 
		                       (W.Interact.mkButState [but])

		val but2_pressed = Widget.Interact.mbut2IsSet 
                                       (Widget.Interact.mkButState [but])

	   	val TextWidget.ChrCrd {row = line_num,...} = 
		                                     TextWidget.ptToCoord text pt

	    in
	        if but1_pressed 
		   then display(TT.do_action (ttree, TT.MoveToNode line_num)) 
		   else if but2_pressed 
			    
  (* NOTE: TT.ApplyTacticToNode unhighlights the current node and moves to the node 
           whose display occupies the line line_num. 
           If line_num is occupied by the current node,
	   then TT.ApplyTacticToNode returns [], so the current node will not be redrawn.
	   If a tactic is applied incorrectly, an exception is raised. 
           apply_string handles the exception by returning [].  
           The exception is raised prior to manipulating any display
	   data.  Therefore, the node(subtree) which we moved to in the call to 
           TT.ApplyTacticToNode	is unhighlighted.  
           Hence we highlight that node(subtree) by calling TT.HighlightSubtree.
   *)
			    then 
				case (TT.do_action (ttree, TT.ApplyTacticToNode line_num),
                                                    apply_string (get_string()))
				     of ([],[]) => ()
			              | (moved_to_subgoal,[]) => 
					    (display moved_to_subgoal;
					     display (TT.do_action (ttree,TT.HighlightSubtree)))
				      | ([],tactic_applied) => 
					    display tactic_applied
				      | (moved_to_subgoal,tactic_applied) => 
					    (display moved_to_subgoal; display tactic_applied)
(* 
(* If we apply an incorrect tactic, then the current node will never be highlighted. 
   Therefore, we have to make sure it is redrawn. 
*)				 (let val current_node_disp = apply_string (get_string())
				 in
				     if current_node_disp <> []
					 then (display (current_node_disp);
					      display (TT.do_action (ttree, TT.MoveToNode line_num)))
				     else ()
				 end))*)

			else ()
	    end

	    and loop () = 
		let open Widget Interact 
		in 
		   case (msgBodyOf (sync m_evt)) 
		    of  (MOUSE_FirstDown b) => ((tw_mouse_server b); loop ())
		     | _ =>  loop () 
		end
	in
	    loop ()
	end

	fun mktext_widget tw = let
		val (fw,evt) = Widget.filterMouse tw

		val scroll_widget = ScrollPort.widgetOf(ScrollPort.mkScrollPort 
					{widget = fw,
				 continuous = true,
				 color = NONE,
				 hsb = SOME {top = true},
				 vsb = SOME {left = true}})
		val frame_widget = Frame.widgetOf(Frame.mkFrame { 
				widget = scroll_widget,
				color = SOME blackc,
				width = display_border_width})
		in
		     spawn (fn () => (mouse_loop evt));
		     Shape.freeSize (frame_widget,Geometry.SIZE{wid=500,ht=500})
				
		end

	val text_widget =  mktext_widget (TextWidget.widgetOf text)

	val main_widget = 
	      Box.widgetOf (Box.mkLayout root 
	        (VtCenter [button_bar, 
			   HzTop [WBox text_widget , tactic_bar ] ,
			   string_widget]))
  
	 in 
	    spawn (fn () => display(TT.do_action(ttree,TT.Display))); 
	    TTreeWidget(main_widget, CML.receive msg_ch)
	end (* end of mkTreeWidget *)

    fun widgetOf (TTreeWidget(widget,_)) = widget
	
    fun evtOf (TTreeWidget(_,evt)) = evt 

    val mkTTreeState = TT.mkTTree 

    val extract_event = 
         fn s => ((TT.synthesize_event s) 
                  handle TT.EventDoesNotAchieve => raise ExtractDoesNotAchieve
                       | e => raise e)
 
    val extract_tactic_text = TT.synthesize_tactic_text

    val extract_text = TT.synthesize_text

end 
