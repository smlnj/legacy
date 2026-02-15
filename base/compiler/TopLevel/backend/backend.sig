(* backend.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature BACKEND = sig
    structure Profile : PROFILE
    structure Compile : COMPILE
    structure Interact : INTERACT
    structure Mutecompiler : MUTECOMPILER (* Added as part of 'Execute as a script' change done by Dayanandan Natarajan Heriot Watt University *)
    structure Machine : MACHINE
    val architecture: string
    val abi_variant: string option
end
