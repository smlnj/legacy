signature Graph_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val AT:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val RBRACKET:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val LBRACKET:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val SYMBOL: (string) *  'a * 'a -> (svalue,'a) token
val EDGEOP:  'a * 'a -> (svalue,'a) token
val EDGE:  'a * 'a -> (svalue,'a) token
val NODE:  'a * 'a -> (svalue,'a) token
val STRICT:  'a * 'a -> (svalue,'a) token
val SUBGRAPH:  'a * 'a -> (svalue,'a) token
val DIGRAPH:  'a * 'a -> (svalue,'a) token
val GRAPH:  'a * 'a -> (svalue,'a) token
end
signature Graph_LRVALS=
sig
structure Tokens : Graph_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
