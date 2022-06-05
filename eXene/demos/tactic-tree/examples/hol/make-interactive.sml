
use "hol-support.sml";
(* dummy_tactic must be in top-level env for use_string in ttree_widget *) 
val tactic_ref = HOL_TTree_Support.tactic_ref; 
(* build interactive interface *) 
structure ITT = InteractiveTT (structure TTS = HOL_TTree_Support);
fun create (g,s,tl) = ITT.create(([],g),s,tl);
val view = ITT.view;
val extract_event = ITT.extract_event; 
val extract_tactic = ITT.extract_tactic_string; 
val extract_text = ITT.extract_text;
(* test formula *) 
val f0 = --`!A.!B.!C.((A \/ B) ==> C) ==> ((A ==> C) /\ (B ==> C))`-- ; 
