signature S = sig type t end;

functor F () : S = struct type t = int end;

functor G () :> S = struct type t = bool end;

structure A = F ();

structure B = G ();
