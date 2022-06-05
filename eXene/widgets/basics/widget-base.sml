(* widget-base.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Definitions for basic widget types.
 *)

signature WIDGET_BASE =
  sig

    structure G : GEOMETRY
    structure CML : CML

    datatype valign = VCenter | VTop | VBottom
    datatype halign = HCenter | HRight | HLeft
    datatype gravity = Center | North | South | East | West |
                       NorthWest | NorthEast | SouthWest | SouthEast

  (* Widget states (e.g., on/off); the bool is the state, and the constructor
   * specifies whether the state can be affected by user action (e.g., mouse
   * click).
   *)
    datatype wstate
      = Active of bool		(* state may be affected by user actions *)
      | Inactive of bool	(* state cannot be affected by user actions *)

    datatype arrow_dir = AD_Up | AD_Down | AD_Left | AD_Right

    type shades = ShadeServer.shades

    exception BadIncrement

    datatype dim = DIM of {
        base    : int,
        incr    : int,
        min     : int,
        nat     : int,
        max     : int option
      }

    (* type bounds = { x_dim : dim, y_dim : dim } *)
    type bounds
    val mkBounds : { x_dim : dim, y_dim : dim } -> bounds

    val fixDim : int -> dim
    val flexDim : int -> dim
    val natDim : dim -> int
    val minDim : dim -> int
    val maxDim : dim -> int option
    val fixBounds : (int * int) -> bounds
    val compatibleDim : dim * int -> bool
    val compatibleSize : bounds * G.size -> bool

    type win_args

    val wrapCreate : (EXeneBase.window * G.rect * win_args) -> EXeneBase.window

    val wrapQueue : 'a CML.event -> 'a CML.event


  end (* WIDGET_BASE *)

structure WidgetBase : WIDGET_BASE =
  struct

    structure G = Geometry
    structure EXB = EXeneBase
    structure CML = CML

    open G

    datatype valign = VCenter | VTop | VBottom
    datatype halign = HCenter | HRight | HLeft
    datatype gravity = Center | North | South | East | West |
                       NorthWest | NorthEast | SouthWest | SouthEast

    datatype wstate = Active of bool | Inactive of bool

    datatype arrow_dir = AD_Up | AD_Down | AD_Left | AD_Right

    type shades = ShadeServer.shades

    exception BadIncrement

    datatype dim = DIM of {
      base    : int,
      incr    : int,
      min     : int,
      nat     : int,
      max     : int option
      }

    type bounds = { x_dim : dim, y_dim : dim }
    fun mkBounds x = x

    fun fixDim x = DIM {base = x, incr = 1, min = 0, nat = 0, max = SOME 0}
    fun flexDim x = DIM {base = x, incr = 1, min = 0, nat = 0, max = NONE}
    fun natDim (DIM{base,incr,nat,...}) = base + incr*nat
    fun minDim (DIM{base,incr,min,...}) = base + incr*min
    fun maxDim (DIM{base,incr,max=NONE,...}) = NONE
      | maxDim (DIM{base,incr,max=SOME max,...}) = SOME(base + incr*max)

    fun fixBounds (x,y) = {x_dim = fixDim x, y_dim = fixDim y}

    fun compatibleDim (dim,v) =
          (minDim dim <= v) andalso
            case maxDim dim of
              NONE => true
            | SOME max => v <= max

    fun compatibleSize ({x_dim,y_dim} : bounds, SIZE{wid,ht}) =
          compatibleDim(x_dim,wid) andalso compatibleDim(y_dim,ht)

    type win_args = {background : EXB.color option}

    fun wrapCreate (pwin, rect, args : win_args) = let
          open EXB
	  val SIZE{wid,ht} = sizeOfRect rect
	  in
	    if (wid <= 0) orelse (ht <= 0) 
              then LibBase.failure{
                     module="Widget",
                     func="wrapCreate",
                     msg="invalid size"
                   }
              else ();
	    EXeneWin.createSimpleSubwin pwin {
	        geom = WGEOM{pos=originOfRect rect, sz=sizeOfRect rect, border=0},
	        backgrnd = #background args,
	        border = NONE  (* not used *)
	      }
	  end

    fun wrapQueue ine = let
          val outchan = CML.channel()
          fun loop ([],[]) = loop([CML.sync ine],[])
            | loop ([],l) = loop(rev l,[])
            | loop (l as e::tl,rest) = 
                loop (CML.select [
                  CML.wrap(CML.sendEvt(outchan,e),fn () => (tl,rest)),
                  CML.wrap(ine,fn e => (l,e::rest))
                ])
          in
            CML.spawn(fn () => loop ([],[]));
            CML.recvEvt outchan
          end

  end (* WidgetBase *)
