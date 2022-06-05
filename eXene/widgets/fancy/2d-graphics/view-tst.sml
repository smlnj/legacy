structure V = WorldView
structure T2D = Transform2D

val v = V.makeView {
	left = ~1.0, right = 1.0,
	bottom = ~1.0, top = 1.0,
	scale = 72.0,
	wid = 72, ht = 72
      }

val t = V.transformOfView v;

