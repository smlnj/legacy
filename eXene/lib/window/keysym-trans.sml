(* keysym-trans.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The implementation of keysym to string translation tables.
 *
 * NOTE: we could probably implement the default bindings using the red-black tree,
 * and thus avoid the ugly ad hoc Xlib code.
 *)

signature KEYSYM_TRANSLATION =
  sig

    type translation

    val defaultTranslation : translation

    val rebind : translation -> (Keysym.keysym * KeyBut.modkey list * string)
	  -> translation

    exception KeysymNotFound
    val lookupString : translation -> (Keysym.keysym * KeyBut.modkey_state)
	  -> string

  end (* KEYSYM_TRANSLATION *)

structure KeysymTranslation : KEYSYM_TRANSLATION =
  struct

    exception KeysymNotFound

    local

    open Keysym

    (* This string maps an ascii character "C" to "^C". *)
      val cntrlMap = "\
	    \\000\001\002\003\004\005\006\007\
	    \\008\009\010\011\012\013\014\015\
	    \\016\017\018\019\020\021\022\023\
	    \\024\025\026\027\028\029\030\031\
	    \\000\033\034\035\036\037\038\039\
	    \\040\041\042\043\044\045\046\031\
	    \\048\049\000\027\028\029\030\031\
	    \\127\057\058\059\060\061\062\063\
	    \\000\001\002\003\004\005\006\007\
	    \\008\009\010\011\012\013\014\015\
	    \\016\017\018\019\020\021\022\023\
	    \\024\025\026\027\028\029\030\031\
	    \\000\001\002\003\004\005\006\007\
	    \\008\009\010\011\012\013\014\015\
	    \\016\017\018\019\020\021\022\023\
	    \\024\025\026\027\028\029\030\127\
	    \"
      fun control x = (String.sub(cntrlMap, x)) handle _ => (Char.chr x)

    (* Translation tables are implemented as red-black trees *)
      datatype color = RED | BLACK
      datatype tree
	= NIL
	| ND of {
	      key : int,
	      color : color,
	      bindings : (KeyBut.modkey_state * string) list,
	      left : tree,
	      right : tree
	    }

      fun insertBinding (t, k, state, v) = let
	    fun upd (ND{key, color, bindings, left, right}, c, l, r) =
		  ND{key=key, color=c, bindings=bindings, left=l, right=r}
	  (* insert (state, v) into the binding list of t, removing any bindings
	   * subsumed by state. *)
	    fun ins (t as ND{key, color, bindings, left, right}) = let
		  fun f [] = NONE
		    | f ((b as (s, _))::r) = (case (KeyBut.matchMod(s, state), f r)
			 of (false, NONE) => NONE
			  | (true, NONE) => SOME r
			  | (false, SOME r') => SOME(b :: r')
			  | (true, x) => x)
		  val b = (case (f bindings)
		       of NONE => (state, v) :: bindings
			| (SOME b) => (state, v) :: b)
		  in
		    ND{key=key, color=color, bindings=b, left=left, right=right}
		  end
	    fun f NIL = ND{
		      key = k, color = RED, bindings = [(state, v)],
		      left = NIL, right = NIL
		    }
	      | f (t as ND{key, color=RED, left, right, ...}) =
		    if (key = k)
		      then ins t
		    else if (k < key)
		      then upd(t, RED, f left, right)
		      else upd(t, RED, left, f right)
	      | f (t as ND{key, color=BLACK, left, right, ...}) =
		    if (key = k)
		      then ins t
		    else if (k < key)
		      then (case (f left)
		       of (l as ND{color=RED, left=ll,
			    right=(lr as ND{color=RED, left=lrl, right=lrr, ...}), ...})
			  => (
			    case right
			     of (r as ND{color=RED, left=rl, right=rr, ...}) =>
				  upd(t, RED,
				    upd(l, BLACK, ll, lr), upd(r, BLACK, rl, rr))
			      | r => upd(lr, BLACK,
					upd(l, RED, ll, lrl), upd(r, RED, lrr, r)))
			| (l as ND{color=RED, right=lr,
			    left=(ll as ND{color=RED, left=lll, right=llr, ...}), ...})
			  => (
			    case right
			     of (r as ND{color=RED, left=rl, right=rr, ...}) =>
				  upd(t, RED,
				    upd(l, BLACK, ll, lr), upd(r, BLACK, rl, rr))
			      | r => upd(l, BLACK, ll, upd(t, RED, lr, r)))
			| l => upd(t, BLACK, l, right))
		      else (case (f right)
		       of (r as ND{color=RED, right=rr,
			    left=(rl as ND{color=RED, left=rll, right=rlr, ...}), ...})
			  => (
			    case left
			     of (l as ND{color=RED, left=ll, right=lr, ...}) =>
				  upd(t, RED,
				    upd(l, BLACK, ll, lr), upd(r, BLACK, rl, rr))
			      | l => upd(rl, BLACK,
					upd(t, RED, l, rll), upd(r, BLACK, rlr, rr)))
			| (r as ND{color=RED, left=rl,
			    right=(rr as ND{color=RED, left=rrl, right=rrr, ...}), ...})
			  => (
			    case left
			     of (l as ND{color=RED, left=ll, right=lr, ...}) =>
				  upd(t, RED,
				    upd(l, BLACK, ll, lr), upd(r, BLACK, rl, rr))
			      | l => upd(r, BLACK, upd(t, RED, l, rl), rr))
			| r => upd(t, BLACK, left, r)
		      (* end case *))
	    in
	      f t
	    end

      fun findBinding (t, k, state) = let
	    fun find NIL = raise KeysymNotFound
	      | find (ND{key, bindings, left, right, ...}) =
		  if (key = k)
		    then let
		      fun getBinding [] = raise KeysymNotFound
			| getBinding ((s, v) :: r) =
			    if (KeyBut.matchMod(state, s)) then v else (getBinding r)
		      in
			getBinding bindings
		      end
		  else if (key > k)
		    then find left
		    else find right
	    in
	      find t
	    end

      fun defaultBinding (k, state) = let
	    val k' = Word.fromInt k
	    val highBytes = Word.<<(k', 0w8)
	  (* Map Misc keysmys to ascii equivalents *)
	    fun standardize 0wxFF80 = 0x20	(* KP_Space => " " *)
	      | standardize 0wx00AD = 0x2D	(* hyphen => "-" *)
	      | standardize 0wxFF08 = 0x08	(* Backspace => BS *)
	      | standardize 0wxFF09 = 0x09	(* Tab => HT *)
	      | standardize 0wxFF0A = 0x0A	(* Linefeed => LF *)
	      | standardize 0wxFF0B = 0x0B	(* Clear => VT *)
	      | standardize 0wxFF0D = 0x0D	(* Return => CR *)
	      | standardize 0wxFF1B = 0x1B	(* Escape => ESC *)
	      | standardize 0wxFFFF = 0x7F	(* Delete => DEL *)
	      | standardize 0wxFF8D = 0x0D	(* KP_Enter => CR *)
	      | standardize 0wxFFBD = 0x3D	(* KP_Equal => "=" *)
	      | standardize c =			(* handle keypad "*+,-./0123456789" *)
		  if ((0wxFFAA <= c) andalso (c <= 0wxFFB9))
		    then Word.toIntX(Word.andb(c, 0wx7f))
		    else raise KeysymNotFound
	    val c = case (Word.>>(k', 0w8))
		   of 0w0 => if (k' = 0wx00AD) then 0x2D else k
		    | 0wxFF => standardize k'
		    | _ => raise KeysymNotFound
	    in
	      if (KeyBut.cntrlIsSet state)
		then String.str(control c)
		else String.str(Char.chr c)
	    end
    in

    datatype translation = TRANS of tree

    val defaultTranslation = TRANS NIL

    fun rebind (TRANS t) (KEYSYM ks, modkeys, v) = let
	  val state = KeyBut.mkModState modkeys
	  in
	    TRANS(insertBinding(t, ks, state, v))
	  end

    fun lookupString (TRANS t) (KEYSYM k, state) = 
	  (findBinding(t, k, state))
	    handle _ => defaultBinding (k, state)

    end (* local *)
  end (* KeysymTranslation *)
