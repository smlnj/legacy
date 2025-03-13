(* ElabData/statenv/statenv.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 * (C) 2025 The Fellowship of SML/NJ (www.smlnj.org)
 *)

signature STATICENV =
sig

    (* Static environments now optionally contain modtrees anchored at
     * bindings.  This allows for rapid on-demand construction of
     * modmaps (= pickling/unpickling contexts).
     *
     * [Matthias Blume, 2000.03] *)

  type staticEnv
  type real_binding = Bindings.binding * Modules.modtree option

  exception Unbound  

  val empty: staticEnv

  val look: staticEnv * Symbol.symbol -> Bindings.binding
      (* raises Unbound when symbol not bound in the staticEnv *)

  val bind: Symbol.symbol * Bindings.binding * staticEnv -> staticEnv

  val special: (Symbol.symbol -> Bindings.binding) * (unit -> Symbol.symbol list)
                  -> staticEnv

  val atop: staticEnv * staticEnv -> staticEnv

  val consolidate: staticEnv -> staticEnv

  val consolidateLazy: staticEnv -> staticEnv

  val app: (Symbol.symbol * Bindings.binding -> unit) -> staticEnv -> unit

  val map: (Bindings.binding -> Bindings.binding) -> staticEnv -> staticEnv

  val fold: ((Symbol.symbol * Bindings.binding) * 'a -> 'a) -> 'a -> staticEnv -> 'a

  val foldOverElems: ((Symbol.symbol * Bindings.binding) * 'a -> 'a)
                     * 'a * staticEnv * Symbol.symbol list
                     -> 'a 

  val sort: staticEnv -> (Symbol.symbol * Bindings.binding) list

  val bind0: Symbol.symbol * real_binding * staticEnv -> staticEnv

  val symbols : staticEnv -> Symbol.symbol list


  val filter : staticEnv * Symbol.symbol list -> staticEnv

end (* signature STATICENV *)
