structure Tokens =
struct
datatype ('b,'a) token =
    EOF of 'a * 'a
  | AT of 'a * 'a
  | DOT of 'a * 'a
  | EQUAL of 'a * 'a
  | RPAREN of 'a * 'a
  | RBRACKET of 'a * 'a
  | RBRACE of 'a * 'a
  | LPAREN of 'a * 'a
  | LBRACKET of 'a * 'a
  | LBRACE of 'a * 'a
  | COMMA of 'a * 'a
  | SEMICOLON of 'a * 'a
  | COLON of 'a * 'a
  | SYMBOL of string * 'a * 'a
  | EDGEOP of 'a * 'a
  | EDGE of 'a * 'a
  | NODE of 'a * 'a
  | STRICT of 'a * 'a
  | SUBGRAPH of 'a * 'a
  | DIGRAPH of 'a * 'a
  | GRAPH of 'a * 'a
type svalue = unit
end
