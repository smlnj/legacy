local
  structure PRS = ParseResourceSpecs
  fun parse (db, str) = let
	val (PRS.RsrcSpec{loose, path, attr, value, ...}) = PRS.parseRsrcSpec str
	in
	  insertRsrcSpec (db, {loose=loose, path=path, attr=attr, value=value})
	end
val path_A_B_C = PRS.parseStyleName "A.B.C"
in
val db = newDB()
val _ = (
      parse (db, "*C.x: 1\n");
      parse (db, "*C.z: 2\n");
      parse (db, "*B.C.y: 3\n");
      parse (db, "*B.C.y: 4\n");
      parse (db, "A.B.C.z: 5\n");
      ())
val look = let val find = findAttrTbls (db, path_A_B_C)
      in
	fn attr => find (Quark.quark attr)
      end
end
