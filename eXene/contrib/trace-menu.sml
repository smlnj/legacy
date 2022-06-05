(* ######################################################################
   #                     CML_TRACE_MENU.SML                             #
   ###################################################################### *)

(* An implementation of an eXene interface to the TraceCML structure.

   AUTHOR:  Clifford Krumvieda
            Department of Computer Science
            Cornell University
            Ithaca, NY 14850
            cliff@cs.cornell.edu
 *)

(* ######################################################################

   Trace menus:  The mkTraceMenu function can be used to create a menu of 
    certain TraceCML modules.  Each line in the menu consists of a box
    and a module name; the box has a checkmark in it if its module is
    being traced.  Clicking in the box toggles the checkmark and trace
    status.  
   The second argument to mkTraceMenu is a list of module names that 
    determine the "frontier" of modules appearing in the menu.  A 
    typical value is ["/"].

   ###################################################################### *)

functor MakeCMLTraceMenu (structure BufferChan : BUFFER_CHAN
			  and TraceCML : TRACE_CML
			  and Box: BOX
			  and Label: LABEL
			  and Toggle: TOGGLE
			  sharing Box.W = Label.W = Toggle.W) : CML_TRACE_MENU =
  struct
    
    structure W = Box.W;

    local
      structure BC = BufferChan
      open Box Label Toggle W.CML
    in

      type trace_menu = box_layout;
      val widgetOf = Box.widgetOf;

      fun mkTraceMenu root only = let
	val toggleCh = BC.buffer ();
	fun transform (name, isTraced) = let
	  val toggle =
	    mkToggleCheck root {state = W.Active isTraced,
				action = fn x =>
				if (x andalso TraceCML.amTracing name) orelse
				  not (x orelse TraceCML.amTracing name) then ()
				else BC.bufferSend (toggleCh, (name, x)),
				color = NONE,
				sz = 30};
	  val label = 
	    mkLabel root {label = TraceCML.nameOf name,
			  font = NONE,
			  foregrnd = NONE,
			  backgrnd = NONE,
			  align = Label.W.HLeft};
	  val box = (HzCenter
		     [WBox (Toggle.widgetOf toggle),
		      Glue {nat = 5, min = 0, max = SOME 5},
		      WBox (Label.widgetOf label),
		      Glue {nat = 5, min = 0, max = NONE}]);
	in
	  ((name, toggle), box)
	end;
	val (toggles, boxes) = 
	  fold (fn (x, (ts, bs)) => 
		(fn (t, b) => (t :: ts, b :: bs)) (transform x))
	    (fold (fn (x, a) => TraceCML.status(TraceCML.moduleOf x) @ a) only [])
	      ([], []);
	fun server () = let
	  fun handleToggle (name, x) = 
	    ((if x then TraceCML.traceOn else TraceCML.traceOff) name;
	     map (fn (n, t) => setState (t, TraceCML.amTracing n)) toggles; ());
	in
	  handleToggle (BC.bufferAccept toggleCh);
	  server ()
	end;
      in
	spawn server;
	mkLayout root (VtCenter boxes)
      end

    end
  end;

structure CMLTraceMenu = 
  MakeCMLTraceMenu (structure BufferChan = BufferChan
		    and TraceCML = TraceCML
		    and Box = Box
		    and Label = Label
		    and Toggle = Toggle);
