structure Main = struct
    structure V = Sim.V
    fun mkData (x, y, vx, vy, m, r, cs) = ((x, y), (vx, vy), m, r, cs)
    val bodyData = map mkData
	[(      0.0, 0.0, 0.0,      0.0, 1.99e33, 8, SOME "orange"),
	 (5.85e12, 0.0, 0.0, 4.76e6, 3.29e26, 2, SOME "yellow"),
	 (1.08e13, 0.0, 0.0, 3.51e6, 4.84e27, 4, SOME "green"),
	 (1.50e13, 0.0, 0.0, 2.97e6, 5.98e27, 4, SOME "blue"),
	 (2.25e13, 0.0, 0.0, 2.43e6, 6.57e26, 3, SOME "red"),
	 (7.80e13, 0.0, 0.0, 1.30e6, 1.90e30, 6, SOME "brown") 
(*
	 (7.80e13, 0.0, 0.0, 1.30e6, 1.90e32, 6, SOME "brown"),
	 (~7.80e13, 0.0, 0.0, ~1.30e6, 1.90e32, 6, SOME "goldenrod1")
*)
	 ]

    structure AnimateSim = AnimateSimFun
	(structure Sim = Sim
	 val bodyData = bodyData)

    val run = AnimateSim.doit
end
