
local
  val dir = "/usr/addon/sml/smlnj-lib-0.1/";
in
  val libsg =
    SourceGroup.create [SourceGroup.Connections [dir ^ "library.dep"]];
end;

