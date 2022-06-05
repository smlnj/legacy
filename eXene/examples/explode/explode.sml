(* explode.sml
 *
 * NOTE: there are two implementation strategies for this:
 *  (1) a centeralized implementation that seperates the game engine
 *      from the interface, and
 *  (2) a distributed implementation that more tightly couples the
 *      engines for the nodes with the widgets that represent them.
 *)

signature GAME_GRAPH =
  sig
    type node

    val neighbors : node -> node list
	(* returns the list of neighbors of this node *)
    val degree : node ->int
	(* returns the number of edges *)
    val state : node -> {player:int, npebbles} option
	(* returns the current state of the node, which can be
	 * empty (NONE), or have some number of pebbles belonging
	 * to some player.
	 *)
    val setState : (node * {player:int, npebbles} option) -> unit
  end

functor Explode (G : GAME_GRAPH) =
  struct

    fun placePebble (nd, player) = let
	  val n = (case (G.state nd)
		 of NONE => (
		      G.setState (nd, SOME{player=player, npebbles=1});
		      1)
		  | (SOME{player=p, npebbles}) =>
		      if (p <> player) then raise Illegal
		        else (
			  G.setState (nd,
			    SOME{player=p, npebbles=npebbles+1});
			  npebbles+1)
		(* end case *))
	  in
	    if (G.degree nd = n)
	      then (
		G.setState (nd, NONE);
		explode (player, G.neighbors nd))
	      else ()
	  end

  end;
