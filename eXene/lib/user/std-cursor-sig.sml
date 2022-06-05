(* std-cursor.sml
 *
 * COPYRIGHT (c) 1990,1991 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * The "names" of the standard cursors supplied by the X server.
 *)

signature STD_CURSOR =
  sig

    structure EXB : EXENE_BASE

    val x_cursor		: EXB.std_cursor
    val arrow			: EXB.std_cursor
    val based_arrow_down	: EXB.std_cursor
    val based_arrow_up		: EXB.std_cursor
    val boat			: EXB.std_cursor
    val bogosity		: EXB.std_cursor
    val bottom_left_corner	: EXB.std_cursor
    val bottom_right_corner	: EXB.std_cursor
    val bottom_side		: EXB.std_cursor
    val bottom_tee		: EXB.std_cursor
    val box_spiral		: EXB.std_cursor
    val center_ptr		: EXB.std_cursor
    val circle			: EXB.std_cursor
    val clock			: EXB.std_cursor
    val coffee_mug		: EXB.std_cursor
    val cross			: EXB.std_cursor
    val cross_reverse		: EXB.std_cursor
    val crosshair		: EXB.std_cursor
    val diamond_cross		: EXB.std_cursor
    val dot			: EXB.std_cursor
    val dotbox			: EXB.std_cursor
    val double_arrow		: EXB.std_cursor
    val draft_large		: EXB.std_cursor
    val draft_small		: EXB.std_cursor
    val draped_box		: EXB.std_cursor
    val exchange		: EXB.std_cursor
    val fleur			: EXB.std_cursor
    val gobbler			: EXB.std_cursor
    val gumby			: EXB.std_cursor
    val hand1			: EXB.std_cursor
    val hand2			: EXB.std_cursor
    val heart			: EXB.std_cursor
    val icon			: EXB.std_cursor
    val iron_cross		: EXB.std_cursor
    val left_ptr		: EXB.std_cursor
    val left_side		: EXB.std_cursor
    val left_tee		: EXB.std_cursor
    val leftbutton		: EXB.std_cursor
    val ll_angle		: EXB.std_cursor
    val lr_angle		: EXB.std_cursor
    val man			: EXB.std_cursor
    val middlebutton		: EXB.std_cursor
    val mouse			: EXB.std_cursor
    val pencil			: EXB.std_cursor
    val pirate			: EXB.std_cursor
    val plus			: EXB.std_cursor
    val question_arrow		: EXB.std_cursor
    val right_ptr		: EXB.std_cursor
    val right_side		: EXB.std_cursor
    val right_tee		: EXB.std_cursor
    val rightbutton		: EXB.std_cursor
    val rtl_logo		: EXB.std_cursor
    val sailboat		: EXB.std_cursor
    val sb_down_arrow		: EXB.std_cursor
    val sb_h_double_arrow	: EXB.std_cursor
    val sb_left_arrow		: EXB.std_cursor
    val sb_right_arrow		: EXB.std_cursor
    val sb_up_arrow		: EXB.std_cursor
    val sb_v_double_arrow	: EXB.std_cursor
    val shuttle			: EXB.std_cursor
    val sizing			: EXB.std_cursor
    val spider			: EXB.std_cursor
    val spraycan		: EXB.std_cursor
    val star			: EXB.std_cursor
    val target			: EXB.std_cursor
    val tcross			: EXB.std_cursor
    val top_left_arrow		: EXB.std_cursor
    val top_left_corner		: EXB.std_cursor
    val top_right_corner	: EXB.std_cursor
    val top_side		: EXB.std_cursor
    val top_tee			: EXB.std_cursor
    val trek			: EXB.std_cursor
    val ul_angle		: EXB.std_cursor
    val umbrella		: EXB.std_cursor
    val ur_angle		: EXB.std_cursor
    val watch			: EXB.std_cursor
    val xterm			: EXB.std_cursor

  end (* STD_CURSOR *)
