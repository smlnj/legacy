(* Copyright 1989 by AT&T Bell Laboratories *)
(* environ.sig *)

signature ENVIRONMENT =
sig

  type staticEnv 
  type dynenv 
  type symenv
  type environment
       (* = { static: staticEnv, dynamic: dynenv, symbolic: symenv } *)
  type symbol (* = Symbol.symbol *)

  val emptyEnv : environment
  val staticPart : environment -> staticEnv
  val dynamicPart : environment -> dynenv
  val symbolicPart : environment -> symenv
  val mkenv : { static: staticEnv, dynamic: dynenv, symbolic: symenv }
              -> environment

  val layerEnv    : environment * environment -> environment
  val concatEnv   : environment * environment -> environment
  val layerStatic : staticEnv * staticEnv -> staticEnv
  val layerSymbolic: symenv * symenv -> symenv
  val filterEnv   : environment * Symbol.symbol list -> environment
(*
  val filterStaticEnv : staticEnv * Symbol.symbol list -> staticEnv
*)
  val consolidateEnv : environment -> environment
  val consolidateStatic : staticEnv -> staticEnv
  val consolidateSymbolic: symenv -> symenv

  (* reduce dynamic and symbolic part to what's actually needed *)
  val trimEnv: environment -> environment

  val describe : staticEnv -> Symbol.symbol -> unit

  val primEnv : staticEnv

end (* signature ENVIRONMENT *)


