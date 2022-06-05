(* transform2d.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * 2-dimensional transformations.
 *
 * Scaling, rotation and translation of a point (x, y) can be represented
 * by multiplying by the general matrix:
 *    
 *                | r11 r12  0 |
 *    [x y 1]  *  | r21 r22  0 |  =  [x*r11+y*r21+tx x*r12+y*r22+ty 1]
 *                | tx  ty   1 |
 *
 * where the sub matrix (r11, ...) is the rotation/scaling operations.
 *)

signature TRANSFORM_2D =
  sig

    type xform
	(* the abstract representation of transformations *)

    val id : xform
	(* the identity transformation *)

    val transform : real * real * xform -> real * real
	(* transform real coordinates *)

    val pixelize  : (real * real * xform) -> (int * int)
	(* transform real coordinates and convert to integers (presumably
	 * pixel coordinates).
	 *)

    val translate : real * real * xform -> xform
	(* add a translation to a transformation *)
    val scale     : real * real * xform -> xform
	(* add scaling to a transformation *)
    val uScale    : real * xform -> xform
	(* add uniform scaling to a transformation *)
    val rotate    : (real * xform) -> xform
	(* add rotation by the given number of radians to a transformation *)

    val inverse   : xform -> xform
	(* compute the inverse transformation *)

    val compose   : xform * xform -> xform
	(* compose two transformations *)

  end;

structure Transform2D : TRANSFORM_2D =
  struct

  (* we specialize the representation of transformations to improve
   * efficiency in the common cases.
   *)
    datatype xform
      = XForm_Id	(* identity *)
      | XForm_TSR of {	(* general transformation *)
	  r11 : real, r12 : real, r21 : real, r22 : real,
	  tx : real, ty : real
        }
      | XForm_S of {	(* scale only *)
	  sx : real, sy : real
        }
      | XForm_T of {	(* translate only *)
	  tx : real, ty : real
	}
      | XForm_TS of {	(* translate & scale *)
	  sx : real, sy : real,
	  tx : real, ty : real
	}

    val id = XForm_Id

    fun transform (x, y, XForm_Id) = (x, y)
      | transform (x, y, XForm_TSR{r11, r12, r21, r22, tx, ty}) =
	  (x*r11 + y*r21 + tx, x*r12 + y*r22 + ty)
      | transform (x, y, XForm_S{sx, sy}) = (x*sx, y*sy)
      | transform (x, y, XForm_T{tx, ty}) = (x+tx, y+ty)
      | transform (x, y, XForm_TS{sx, sy, tx, ty}) = (x*sx + tx, y*sy + ty)

    fun pixelize arg = let
	  val (x, y) = transform arg
	  in
	    (floor(x+0.5), floor(y+0.5))
	  end

  (* scaling *)
    fun scale (sx, sy, XForm_Id) = XForm_S{sx=sx, sy=sy}
      | scale (sx, sy, XForm_TSR{r11, r12, r21, r22, tx, ty}) =
	  XForm_TSR{
	      r11 = sx*r11, r12 = sy*r12,
	      r21 = sx*r21, r22 = sy*r22,
	      tx  = sx*tx,  ty  = sy*ty
	    }
      | scale (sx', sy', XForm_S{sx, sy}) = XForm_S{sx = sx'*sx, sy = sy'*sy}
      | scale (sx, sy, XForm_T{tx, ty}) =
	  XForm_TS{sx = sx, sy = sy, tx = sx*tx, ty = sy*ty}
      | scale (sx', sy', XForm_TS{sx, sy, tx, ty}) =
	  XForm_TS{sx = sx'*sx, sy = sy'*sy, tx = sx'*tx, ty = sy'*ty}

  (* uniform scaling *)
    fun uScale (s, xform) = scale(s, s, xform)

  (* add rotation by the given number of radians to a transformation *)
    fun rotate (theta, xform) = let
	  val sinTheta = Math.sin theta
	  val cosTheta = Math.cos theta
	  in
	    case xform
	     of XForm_Id => XForm_TSR{
		    r11 = cosTheta,  r12 = sinTheta,
		    r21 = ~sinTheta, r22 = cosTheta,
		    tx  = 0.0,       ty  = 0.0
		  }
	      | _ => raise Fail "rotations not implemented yet"
	    (* end case *)
	  end

  (* translation *)
    fun translate (tx, ty, XForm_Id) = XForm_T{tx=tx, ty=ty}
      | translate (tx', ty', XForm_TSR{r11, r12, r21, r22, tx, ty}) = XForm_TSR{
	    r11 = r11, r12 = r12, r21 = r21, r22 = r22, tx = tx'+tx, ty = ty'+ty
	  }
      | translate (tx, ty, XForm_S{sx, sy}) = XForm_TS{
	    sx = sx, sy = sy, tx = tx, ty = ty
	  }
      | translate (tx', ty', XForm_T{tx, ty}) = XForm_T{tx = tx'+tx, ty = ty'+ty}
      | translate (tx', ty', XForm_TS{sx, sy, tx, ty}) = XForm_TS{
	    sx = sx, sy = sy,tx = tx+tx', ty = ty+ty'
	  }

  (* map a transformation into the general representation *)
    fun mkTSR (XForm_Id) = {
	    r11 = 1.0, r12 = 0.0, r21 = 0.0, r22 = 1.0, tx = 0.0, ty = 0.0
	  }
      | mkTSR (XForm_TSR a) = a
      | mkTSR (XForm_S{sx, sy}) = {
	    r11 = sx, r12 = 0.0, r21 = 0.0, r22 = sy, tx = 0.0, ty = 0.0
	  }
      | mkTSR (XForm_T{tx, ty}) = {
	    r11 = 1.0, r12 = 0.0, r21 = 0.0, r22 = 1.0, tx = tx, ty = ty
	  }
      | mkTSR (XForm_TS{tx, ty, sx, sy}) = {
	    r11 = sx, r12 = 0.0, r21 = 0.0, r22 = sy, tx = tx, ty = ty
	  }

  (* compute the inverse of a transformation matrix *)
    fun inverse (XForm_Id) = XForm_Id
      | inverse (XForm_S{sx, sy}) = XForm_S{sx = 1.0/sx, sy = 1.0/sy}
      | inverse (XForm_T{tx, ty}) = XForm_T{tx = ~tx, ty = ~ty}
      | inverse (XForm_TS{sx, sy, tx, ty}) = XForm_TS{
	    sx = 1.0/sx, sy = 1.0/sy, tx = ~tx/sx, ty = ~ty/sy
	  }
      | inverse (XForm_TSR{r11, r12, r21, r22, tx, ty}) = let
	  val r11r22 = r11*r22
	  val r12r21 = r12*r21
	  val a = r11r22 - r12r21
	  val b = r12r21 - r11r22
	  val r11' = ~(r22 / b)
	  val r12' = ~(r22 / a)
	  val r21' = r21 / b
	  val r22' = r11 / a
	  in
	    XForm_TSR{
		r11 = r11', r12 = r12',
		r21 = r21', r22 = r22',
		tx = ~(r11'*tx + r21'*ty), ty = ~(r12'*tx + r22'*ty)
	      }
	  end

  (* compose two transformations *)
    fun compose (XForm_Id, xform) = xform
      | compose (xform, XForm_Id) = xform
      | compose (xform, XForm_S{sx, sy}) = scale(sx, sy, xform)
      | compose (xform, XForm_T{tx, ty}) = translate(tx, ty, xform)
      | compose (XForm_S{sx, sy}, XForm_TS b) = XForm_TS{
	    sx = sx*(#sx b), sy = sy*(#sy b),
	    tx = (#tx b), ty = (#ty b)
	  }
      | compose (XForm_T{tx, ty}, XForm_TS b) = XForm_TS{
	    sx = (#sx b), sy = (#sy b),
	    tx = tx*(#sx b)+(#tx b), ty = ty*(#sy b)+(#ty b)
	  }
      | compose (XForm_TS a, XForm_TS b) = XForm_TS{
	    sx = (#sx a)*(#sx b), sy = (#sy a)*(#sy b),
	    tx = (#tx a)*(#sx b)+(#tx b), ty = (#ty a)*(#sy b)+(#ty b)
	  }
      | compose (xform1, xform2) = let
	  val a = mkTSR xform1 and b = mkTSR xform2
	  in
	    XForm_TSR{
		r11 = (#r11 a)*(#r11 b) + (#r12 a)*(#r21 b),
		r12 = (#r11 a)*(#r12 b) + (#r12 a)*(#r22 b),
		r21 = (#r21 a)*(#r11 b) + (#r22 a)*(#r21 b),
		r22 = (#r21 a)*(#r12 b) + (#r22 a)*(#r22 b),
		tx  = (#tx a)*(#r11 b) + (#ty a)*(#r21 b) + (#tx b),
		ty  = (#ty a)*(#r12 b) + (#ty a)*(#r22 b) + (#ty b)
	      }
	  end

  end;

