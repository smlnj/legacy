open DerivativeTactics;
structure Interactive = InteractiveTT(structure TTS  = Derivative_TTree_Support);
fun create(t,d,l) = Interactive.create(ExpressionParseString.read_expression t, d,l) ;	
val extract = Interactive.extract_event; 
val view = Interactive.view;
val tactic_ref = Derivative_TTree_Support.tactic_ref;	

