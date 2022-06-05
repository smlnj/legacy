
functor GraphLrValsFun(
           structure Token : TOKEN
           structure G : ATTR_GRAPH 
           val graph_info : unit -> G.graph_info
           val node_info : unit -> G.node_info
           val edge_info : unit -> G.edge_info)
         = 
struct
structure ParserData=
struct
structure Header = 
struct
exception Error
datatype vertex = 
    Node of (string * string option) 
  | Subgraph of (G.graph -> G.graph)
fun GA g = G.setAttr(G.GRAPH g)
fun NA n = G.setAttr(G.NODE n)
fun EA e = G.setAttr(G.EDGE e)
fun PNA g = G.setAttr(G.PROTONODE g)
fun PEA g = G.setAttr(G.PROTOEDGE g)
fun findSubgraph (g, name) =
      case G.findSubgraph (g, name) of
        SOME sg => sg
      | NONE => raise Error
local
  val cnt = ref 0
in
fun anonymous() = ("_anonymous_"^(makestring(!cnt))) before (inc cnt)
end
local
  fun mkPortFn (NONE, NONE) = (fn _ => ())
    | mkPortFn (SOME tp, NONE) = (fn e => EA e ("tailport",tp))
    | mkPortFn (NONE, SOME hp) = (fn e => EA e ("headport",hp))
    | mkPortFn (SOME tp, SOME hp) = 
        (fn e => (EA e ("headport",hp); EA e ("tailport",tp)))
in
fun mkEdges (vs, attrs) g = let
      fun doEdge portfn (t,h) = let
            val e = G.addEdge{graph=g,hd=h,tl=t,info=NONE}
            in
              portfn e;
              app (EA e) attrs
            end
      fun mkE (tl::(rest as hd::L)) =
            (case (tl,hd) of
              (Node(t,tport),Node(h,hport)) => (
                doEdge (mkPortFn(tport,hport)) (G.getNode(g,t,NONE),G.getNode(g,h,NONE));
                mkE rest
              )
            | (Node(name,port),Subgraph mkg) => let
                val edgefn = doEdge (mkPortFn(port,NONE))
                val t = G.getNode(g,name,NONE)
                val subg = mkg g
                in
                  G.appNodes (fn n => edgefn(t,n)) subg;
                  mkE((Subgraph(fn _ => subg))::L)
                end
            | (Subgraph mkg, Node(name,port)) => let
                val edgefn = doEdge (mkPortFn(NONE, port))
                val h = G.getNode(g,name,NONE)
                in
                  G.appNodes (fn n => edgefn(n,h)) (mkg g);
                  mkE rest
                end
            | (Subgraph mkg, Subgraph mkg') => let
                val edgefn = doEdge (mkPortFn(NONE, NONE))
                val tailg = mkg g
                val headg = mkg' g
                in
                  G.appNodes (fn h => G.appNodes (fn t => edgefn(t,h)) tailg) headg;
                  mkE((Subgraph(fn _ => headg))::L)
                end
            )
        | mkE _ = ()
      in
        mkE vs;
        g
      end
end


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\009\000\002\000\008\000\000\000\
\\001\000\003\000\028\000\008\000\053\000\012\000\024\000\000\000\
\\001\000\008\000\007\000\000\000\
\\001\000\008\000\048\000\000\000\
\\001\000\008\000\056\000\000\000\
\\001\000\008\000\058\000\014\000\057\000\000\000\
\\001\000\008\000\065\000\000\000\
\\001\000\008\000\069\000\000\000\
\\001\000\008\000\075\000\000\000\
\\001\000\011\000\073\000\000\000\
\\001\000\012\000\010\000\000\000\
\\001\000\013\000\045\000\000\000\
\\001\000\015\000\043\000\000\000\
\\001\000\015\000\064\000\000\000\
\\001\000\015\000\066\000\000\000\
\\001\000\016\000\072\000\000\000\
\\001\000\017\000\076\000\000\000\
\\001\000\018\000\047\000\000\000\
\\001\000\021\000\000\000\000\000\
\\078\000\000\000\
\\079\000\001\000\006\000\002\000\005\000\004\000\004\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\008\000\063\000\000\000\
\\089\000\011\000\071\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\093\000\007\000\032\000\000\000\
\\094\000\013\000\045\000\000\000\
\\095\000\000\000\
\\096\000\001\000\029\000\003\000\028\000\005\000\027\000\006\000\026\000\
\\008\000\025\000\012\000\024\000\000\000\
\\097\000\001\000\029\000\003\000\028\000\005\000\027\000\006\000\026\000\
\\008\000\025\000\012\000\024\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\010\000\041\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\007\000\032\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\109\000\018\000\047\000\000\000\
\\110\000\009\000\037\000\020\000\036\000\000\000\
\\111\000\020\000\036\000\000\000\
\\112\000\009\000\037\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\007\000\032\000\000\000\
\\122\000\000\000\
\\123\000\007\000\032\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\012\000\030\000\000\000\
\\128\000\000\000\
\"
val actionRowNumbers =
"\020\000\002\000\000\000\023\000\
\\021\000\010\000\024\000\022\000\
\\039\000\070\000\047\000\045\000\
\\044\000\053\000\035\000\046\000\
\\042\000\040\000\038\000\012\000\
\\049\000\011\000\039\000\052\000\
\\027\000\026\000\003\000\025\000\
\\039\000\034\000\001\000\055\000\
\\054\000\050\000\004\000\005\000\
\\034\000\061\000\036\000\043\000\
\\041\000\019\000\048\000\029\000\
\\013\000\006\000\071\000\014\000\
\\063\000\066\000\064\000\051\000\
\\056\000\057\000\060\000\007\000\
\\058\000\062\000\033\000\030\000\
\\015\000\017\000\069\000\037\000\
\\068\000\067\000\065\000\009\000\
\\029\000\031\000\032\000\008\000\
\\028\000\016\000\059\000\018\000"
val gotoT =
"\
\\001\000\075\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\021\000\009\000\020\000\010\000\019\000\011\000\018\000\
\\012\000\017\000\013\000\016\000\014\000\015\000\015\000\014\000\
\\016\000\013\000\020\000\012\000\021\000\011\000\023\000\010\000\
\\024\000\009\000\000\000\
\\000\000\
\\022\000\029\000\000\000\
\\000\000\
\\000\000\
\\017\000\033\000\018\000\032\000\019\000\031\000\000\000\
\\007\000\038\000\008\000\037\000\022\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\021\000\009\000\020\000\012\000\040\000\013\000\016\000\
\\014\000\015\000\015\000\014\000\016\000\013\000\020\000\012\000\
\\021\000\011\000\023\000\010\000\024\000\009\000\000\000\
\\000\000\
\\000\000\
\\006\000\042\000\000\000\
\\003\000\021\000\009\000\020\000\010\000\044\000\011\000\018\000\
\\012\000\017\000\013\000\016\000\014\000\015\000\015\000\014\000\
\\016\000\013\000\020\000\012\000\021\000\011\000\023\000\010\000\
\\024\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\021\000\009\000\020\000\010\000\047\000\011\000\018\000\
\\012\000\017\000\013\000\016\000\014\000\015\000\015\000\014\000\
\\016\000\013\000\020\000\012\000\021\000\011\000\023\000\010\000\
\\024\000\009\000\000\000\
\\007\000\038\000\008\000\048\000\000\000\
\\015\000\050\000\016\000\013\000\023\000\049\000\024\000\009\000\000\000\
\\018\000\052\000\000\000\
\\019\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\038\000\008\000\057\000\000\000\
\\000\000\
\\006\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\060\000\009\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\065\000\000\000\
\\022\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\072\000\009\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 76
val numrules = 51
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.arrayoflist(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.arrayoflist(actionRows) in fn i=>Array.sub(a,i) end
in Array.arrayoflist(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | SYMBOL of unit ->  (string) | SubgHdr of unit ->  (string)
 | SubgStmt of unit ->  ( ( G.graph -> G.graph ) )
 | EdgeRHS of unit ->  (vertex list)
 | EdgeStmt of unit ->  ( ( G.graph -> G.graph ) )
 | NodeStmt of unit ->  ( ( G.graph -> G.graph ) )
 | PortAngle of unit ->  (string) | PortLocation of unit ->  (string)
 | NodePort of unit ->  (string option)
 | NodeName of unit ->  (string)
 | NodeId of unit ->  ( ( string * string option ) )
 | AttrStmt of unit ->  ( ( G.graph -> G.graph ) )
 | Stmt1 of unit ->  ( ( G.graph -> G.graph ) )
 | Stmt of unit ->  ( ( G.graph -> G.graph ) )
 | StmtList1 of unit ->  ( ( G.graph -> G.graph ) )
 | StmtList of unit ->  ( ( G.graph -> G.graph ) )
 | AttrSet of unit ->  ( ( string * string ) )
 | OptAttrList of unit ->  ( ( string * string )  list)
 | RecAttrList of unit ->  ( ( string * string )  list)
 | AttrList of unit ->  ( ( string * string )  list)
 | InsideAttrList of unit ->  ( ( string * string )  list)
 | AttrClass of unit ->  ( ( ((string * string) list) -> G.graph -> G.graph ) )
 | GraphType of unit ->  (string) | File of unit ->  (G.graph option)
end
type svalue = MlyValue.svalue
type result = G.graph option
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_change = 
nil
val noShift = 
fn (T 20) => true | _ => false
val showTerminal =
fn (T 0) => "GRAPH"
  | (T 1) => "DIGRAPH"
  | (T 2) => "SUBGRAPH"
  | (T 3) => "STRICT"
  | (T 4) => "NODE"
  | (T 5) => "EDGE"
  | (T 6) => "EDGEOP"
  | (T 7) => "SYMBOL"
  | (T 8) => "COLON"
  | (T 9) => "SEMICOLON"
  | (T 10) => "COMMA"
  | (T 11) => "LBRACE"
  | (T 12) => "LBRACKET"
  | (T 13) => "LPAREN"
  | (T 14) => "RBRACE"
  | (T 15) => "RBRACKET"
  | (T 16) => "RPAREN"
  | (T 17) => "EQUAL"
  | (T 18) => "DOT"
  | (T 19) => "AT"
  | (T 20) => "EOF"
  | _ => "bogus-term"
val errtermvalue=
let open Header in
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 8) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13) :: (T 14)
 :: (T 15) :: (T 16) :: (T 17) :: (T 18) :: (T 19) :: (T 20) :: nil
end
structure Actions =
struct 
exception mlyAction of int
val actions = 
let open Header
in
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(_,_,RBRACE1right))::(_,(MlyValue.StmtList StmtList1,_,_))::_
::(_,(MlyValue.SYMBOL SYMBOL1,_,_))::(_,(MlyValue.GraphType GraphType1
,GraphType1left,_))::rest671) => let val result=MlyValue.File(fn _ => 
let val GraphType as GraphType1=GraphType1 ()
val SYMBOL as SYMBOL1=SYMBOL1 ()
val StmtList as StmtList1=StmtList1 ()
 in (
let val g = G.mkGraph{name=SYMBOL,graph_info=graph_info,
                        edge_info=edge_info, node_info=node_info, info=NONE}
                       in 
                         G.setAttr (G.GRAPH g) ("graph_type",GraphType);
                         SOME(StmtList g) 
                       end
) end
)
 in (LrTable.NT 0,(result,GraphType1left,RBRACE1right),rest671) end
| (1,rest671) => let val result=MlyValue.File(fn _ => (NONE))
 in (LrTable.NT 0,(result,defaultPos,defaultPos),rest671) end
| (2,(_,(_,GRAPH1left,GRAPH1right))::rest671) => let val result=
MlyValue.GraphType(fn _ => ("g"))
 in (LrTable.NT 1,(result,GRAPH1left,GRAPH1right),rest671) end
| (3,(_,(_,_,GRAPH1right))::(_,(_,STRICT1left,_))::rest671) => let 
val result=MlyValue.GraphType(fn _ => ("sg"))
 in (LrTable.NT 1,(result,STRICT1left,GRAPH1right),rest671) end
| (4,(_,(_,DIGRAPH1left,DIGRAPH1right))::rest671) => let val result=
MlyValue.GraphType(fn _ => ("dg"))
 in (LrTable.NT 1,(result,DIGRAPH1left,DIGRAPH1right),rest671) end
| (5,(_,(_,_,DIGRAPH1right))::(_,(_,STRICT1left,_))::rest671) => let 
val result=MlyValue.GraphType(fn _ => ("sdg"))
 in (LrTable.NT 1,(result,STRICT1left,DIGRAPH1right),rest671) end
| (6,(_,(_,GRAPH1left,GRAPH1right))::rest671) => let val result=
MlyValue.AttrClass(fn _ => (fn al => fn g => (app (GA g) al; g)))
 in (LrTable.NT 2,(result,GRAPH1left,GRAPH1right),rest671) end
| (7,(_,(_,NODE1left,NODE1right))::rest671) => let val result=
MlyValue.AttrClass(fn _ => (fn al => fn g => (app (PNA g) al; g)))
 in (LrTable.NT 2,(result,NODE1left,NODE1right),rest671) end
| (8,(_,(_,EDGE1left,EDGE1right))::rest671) => let val result=
MlyValue.AttrClass(fn _ => (fn al => fn g => (app (PEA g) al; g)))
 in (LrTable.NT 2,(result,EDGE1left,EDGE1right),rest671) end
| (9,(_,(MlyValue.InsideAttrList InsideAttrList1,_,
InsideAttrList1right))::(_,(MlyValue.ntVOID Optcomma1,_,_))::(_,(
MlyValue.AttrSet AttrSet1,AttrSet1left,_))::rest671) => let val result
=MlyValue.InsideAttrList(fn _ => let val AttrSet as AttrSet1=AttrSet1 
()
val Optcomma1=Optcomma1 ()
val InsideAttrList as InsideAttrList1=InsideAttrList1 ()
 in (AttrSet::InsideAttrList) end
)
 in (LrTable.NT 3,(result,AttrSet1left,InsideAttrList1right),rest671)
 end
| (10,rest671) => let val result=MlyValue.InsideAttrList(fn _ => ([]))
 in (LrTable.NT 3,(result,defaultPos,defaultPos),rest671) end
| (11,rest671) => let val result=MlyValue.ntVOID(fn _ => ())
 in (LrTable.NT 4,(result,defaultPos,defaultPos),rest671) end
| (12,(_,(_,COMMA1left,COMMA1right))::rest671) => let val result=
MlyValue.ntVOID(fn _ => ())
 in (LrTable.NT 4,(result,COMMA1left,COMMA1right),rest671) end
| (13,(_,(_,_,RBRACKET1right))::(_,(MlyValue.InsideAttrList 
InsideAttrList1,_,_))::(_,(_,LBRACKET1left,_))::rest671) => let val 
result=MlyValue.AttrList(fn _ => let val InsideAttrList as 
InsideAttrList1=InsideAttrList1 ()
 in (InsideAttrList) end
)
 in (LrTable.NT 5,(result,LBRACKET1left,RBRACKET1right),rest671) end
| (14,(_,(MlyValue.AttrList AttrList1,_,AttrList1right))::(_,(
MlyValue.RecAttrList RecAttrList1,RecAttrList1left,_))::rest671) => 
let val result=MlyValue.RecAttrList(fn _ => let val RecAttrList as 
RecAttrList1=RecAttrList1 ()
val AttrList as AttrList1=AttrList1 ()
 in (RecAttrList@AttrList) end
)
 in (LrTable.NT 6,(result,RecAttrList1left,AttrList1right),rest671)
 end
| (15,rest671) => let val result=MlyValue.RecAttrList(fn _ => ([]))
 in (LrTable.NT 6,(result,defaultPos,defaultPos),rest671) end
| (16,(_,(MlyValue.RecAttrList RecAttrList1,RecAttrList1left,
RecAttrList1right))::rest671) => let val result=MlyValue.OptAttrList(
fn _ => let val RecAttrList as RecAttrList1=RecAttrList1 ()
 in (RecAttrList) end
)
 in (LrTable.NT 7,(result,RecAttrList1left,RecAttrList1right),rest671)
 end
| (17,(_,(MlyValue.SYMBOL SYMBOL2,_,SYMBOL2right))::_::(_,(
MlyValue.SYMBOL SYMBOL1,SYMBOL1left,_))::rest671) => let val result=
MlyValue.AttrSet(fn _ => let val SYMBOL1=SYMBOL1 ()
val SYMBOL2=SYMBOL2 ()
 in ((SYMBOL1,SYMBOL2)) end
)
 in (LrTable.NT 8,(result,SYMBOL1left,SYMBOL2right),rest671) end
| (18,(_,(MlyValue.StmtList1 StmtList11,StmtList11left,StmtList11right
))::rest671) => let val result=MlyValue.StmtList(fn _ => let val 
StmtList1 as StmtList11=StmtList11 ()
 in (StmtList1) end
)
 in (LrTable.NT 9,(result,StmtList11left,StmtList11right),rest671) end
| (19,rest671) => let val result=MlyValue.StmtList(fn _ => (fn g => g)
)
 in (LrTable.NT 9,(result,defaultPos,defaultPos),rest671) end
| (20,(_,(MlyValue.Stmt Stmt1,Stmt1left,Stmt1right))::rest671) => let 
val result=MlyValue.StmtList1(fn _ => let val Stmt as Stmt1=Stmt1 ()
 in (Stmt) end
)
 in (LrTable.NT 10,(result,Stmt1left,Stmt1right),rest671) end
| (21,(_,(MlyValue.Stmt Stmt1,_,Stmt1right))::(_,(MlyValue.StmtList1 
StmtList11,StmtList11left,_))::rest671) => let val result=
MlyValue.StmtList1(fn _ => let val StmtList1 as StmtList11=StmtList11 
()
val Stmt as Stmt1=Stmt1 ()
 in (Stmt o StmtList1) end
)
 in (LrTable.NT 10,(result,StmtList11left,Stmt1right),rest671) end
| (22,(_,(MlyValue.Stmt1 Stmt11,Stmt11left,Stmt11right))::rest671) => 
let val result=MlyValue.Stmt(fn _ => let val Stmt1 as Stmt11=Stmt11 ()
 in (Stmt1) end
)
 in (LrTable.NT 11,(result,Stmt11left,Stmt11right),rest671) end
| (23,(_,(_,_,SEMICOLON1right))::(_,(MlyValue.Stmt1 Stmt11,Stmt11left,
_))::rest671) => let val result=MlyValue.Stmt(fn _ => let val Stmt1
 as Stmt11=Stmt11 ()
 in (Stmt1) end
)
 in (LrTable.NT 11,(result,Stmt11left,SEMICOLON1right),rest671) end
| (24,(_,(MlyValue.NodeStmt NodeStmt1,NodeStmt1left,NodeStmt1right))::
rest671) => let val result=MlyValue.Stmt1(fn _ => let val NodeStmt as 
NodeStmt1=NodeStmt1 ()
 in (NodeStmt) end
)
 in (LrTable.NT 12,(result,NodeStmt1left,NodeStmt1right),rest671) end
| (25,(_,(MlyValue.EdgeStmt EdgeStmt1,EdgeStmt1left,EdgeStmt1right))::
rest671) => let val result=MlyValue.Stmt1(fn _ => let val EdgeStmt as 
EdgeStmt1=EdgeStmt1 ()
 in (EdgeStmt) end
)
 in (LrTable.NT 12,(result,EdgeStmt1left,EdgeStmt1right),rest671) end
| (26,(_,(MlyValue.AttrStmt AttrStmt1,AttrStmt1left,AttrStmt1right))::
rest671) => let val result=MlyValue.Stmt1(fn _ => let val AttrStmt as 
AttrStmt1=AttrStmt1 ()
 in (AttrStmt) end
)
 in (LrTable.NT 12,(result,AttrStmt1left,AttrStmt1right),rest671) end
| (27,(_,(MlyValue.SubgStmt SubgStmt1,SubgStmt1left,SubgStmt1right))::
rest671) => let val result=MlyValue.Stmt1(fn _ => let val SubgStmt as 
SubgStmt1=SubgStmt1 ()
 in (fn g => (SubgStmt g; g)) end
)
 in (LrTable.NT 12,(result,SubgStmt1left,SubgStmt1right),rest671) end
| (28,(_,(MlyValue.AttrList AttrList1,_,AttrList1right))::(_,(
MlyValue.AttrClass AttrClass1,AttrClass1left,_))::rest671) => let val 
result=MlyValue.AttrStmt(fn _ => let val AttrClass as AttrClass1=
AttrClass1 ()
val AttrList as AttrList1=AttrList1 ()
 in (AttrClass AttrList) end
)
 in (LrTable.NT 13,(result,AttrClass1left,AttrList1right),rest671) end
| (29,(_,(MlyValue.AttrSet AttrSet1,AttrSet1left,AttrSet1right))::
rest671) => let val result=MlyValue.AttrStmt(fn _ => let val AttrSet
 as AttrSet1=AttrSet1 ()
 in (fn g => (GA g (#1 AttrSet,#2 AttrSet);g)) end
)
 in (LrTable.NT 13,(result,AttrSet1left,AttrSet1right),rest671) end
| (30,(_,(MlyValue.NodePort NodePort1,_,NodePort1right))::(_,(
MlyValue.NodeName NodeName1,NodeName1left,_))::rest671) => let val 
result=MlyValue.NodeId(fn _ => let val NodeName as NodeName1=NodeName1
 ()
val NodePort as NodePort1=NodePort1 ()
 in (NodeName,NodePort) end
)
 in (LrTable.NT 14,(result,NodeName1left,NodePort1right),rest671) end
| (31,(_,(MlyValue.SYMBOL SYMBOL1,SYMBOL1left,SYMBOL1right))::rest671)
 => let val result=MlyValue.NodeName(fn _ => let val SYMBOL as SYMBOL1
=SYMBOL1 ()
 in (SYMBOL) end
)
 in (LrTable.NT 15,(result,SYMBOL1left,SYMBOL1right),rest671) end
| (32,rest671) => let val result=MlyValue.NodePort(fn _ => (NONE))
 in (LrTable.NT 16,(result,defaultPos,defaultPos),rest671) end
| (33,(_,(MlyValue.PortLocation PortLocation1,PortLocation1left,
PortLocation1right))::rest671) => let val result=MlyValue.NodePort(fn 
_ => let val PortLocation as PortLocation1=PortLocation1 ()
 in (SOME PortLocation) end
)
 in (LrTable.NT 16,(result,PortLocation1left,PortLocation1right),
rest671) end
| (34,(_,(MlyValue.PortAngle PortAngle1,PortAngle1left,PortAngle1right
))::rest671) => let val result=MlyValue.NodePort(fn _ => let val 
PortAngle as PortAngle1=PortAngle1 ()
 in (SOME PortAngle) end
)
 in (LrTable.NT 16,(result,PortAngle1left,PortAngle1right),rest671)
 end
| (35,(_,(MlyValue.PortLocation PortLocation1,_,PortLocation1right))::
(_,(MlyValue.PortAngle PortAngle1,PortAngle1left,_))::rest671) => let 
val result=MlyValue.NodePort(fn _ => let val PortAngle as PortAngle1=
PortAngle1 ()
val PortLocation as PortLocation1=PortLocation1 ()
 in (SOME(PortAngle^PortLocation)) end
)
 in (LrTable.NT 16,(result,PortAngle1left,PortLocation1right),rest671)
 end
| (36,(_,(MlyValue.PortAngle PortAngle1,_,PortAngle1right))::(_,(
MlyValue.PortLocation PortLocation1,PortLocation1left,_))::rest671)
 => let val result=MlyValue.NodePort(fn _ => let val PortLocation as 
PortLocation1=PortLocation1 ()
val PortAngle as PortAngle1=PortAngle1 ()
 in (SOME(PortLocation^PortAngle)) end
)
 in (LrTable.NT 16,(result,PortLocation1left,PortAngle1right),rest671)
 end
| (37,(_,(MlyValue.SYMBOL SYMBOL1,_,SYMBOL1right))::(_,(_,COLON1left,_
))::rest671) => let val result=MlyValue.PortLocation(fn _ => let val 
SYMBOL as SYMBOL1=SYMBOL1 ()
 in (":"^SYMBOL) end
)
 in (LrTable.NT 17,(result,COLON1left,SYMBOL1right),rest671) end
| (38,(_,(_,_,RPAREN1right))::(_,(MlyValue.SYMBOL SYMBOL2,_,_))::_::(_
,(MlyValue.SYMBOL SYMBOL1,_,_))::_::(_,(_,COLON1left,_))::rest671) => 
let val result=MlyValue.PortLocation(fn _ => let val SYMBOL1=SYMBOL1 
()
val SYMBOL2=SYMBOL2 ()
 in (concat[":(",SYMBOL1,",",SYMBOL2,")"]) end
)
 in (LrTable.NT 17,(result,COLON1left,RPAREN1right),rest671) end
| (39,(_,(MlyValue.SYMBOL SYMBOL1,_,SYMBOL1right))::(_,(_,AT1left,_))
::rest671) => let val result=MlyValue.PortAngle(fn _ => let val SYMBOL
 as SYMBOL1=SYMBOL1 ()
 in ("@"^SYMBOL) end
)
 in (LrTable.NT 18,(result,AT1left,SYMBOL1right),rest671) end
| (40,(_,(MlyValue.OptAttrList OptAttrList1,_,OptAttrList1right))::(_,
(MlyValue.NodeId NodeId1,NodeId1left,_))::rest671) => let val result=
MlyValue.NodeStmt(fn _ => let val NodeId as NodeId1=NodeId1 ()
val OptAttrList as OptAttrList1=OptAttrList1 ()
 in (fn g => (app (NA (G.getNode(g, #1 NodeId,NONE))) OptAttrList; g))
 end
)
 in (LrTable.NT 19,(result,NodeId1left,OptAttrList1right),rest671) end
| (41,(_,(MlyValue.OptAttrList OptAttrList1,_,OptAttrList1right))::(_,
(MlyValue.EdgeRHS EdgeRHS1,_,_))::(_,(MlyValue.NodeId NodeId1,
NodeId1left,_))::rest671) => let val result=MlyValue.EdgeStmt(fn _ => 
let val NodeId as NodeId1=NodeId1 ()
val EdgeRHS as EdgeRHS1=EdgeRHS1 ()
val OptAttrList as OptAttrList1=OptAttrList1 ()
 in (mkEdges((Node NodeId)::EdgeRHS,OptAttrList)) end
)
 in (LrTable.NT 20,(result,NodeId1left,OptAttrList1right),rest671) end
| (42,(_,(MlyValue.OptAttrList OptAttrList1,_,OptAttrList1right))::(_,
(MlyValue.EdgeRHS EdgeRHS1,_,_))::(_,(MlyValue.SubgStmt SubgStmt1,
SubgStmt1left,_))::rest671) => let val result=MlyValue.EdgeStmt(fn _
 => let val SubgStmt as SubgStmt1=SubgStmt1 ()
val EdgeRHS as EdgeRHS1=EdgeRHS1 ()
val OptAttrList as OptAttrList1=OptAttrList1 ()
 in (mkEdges((Subgraph SubgStmt)::EdgeRHS,OptAttrList)) end
)
 in (LrTable.NT 20,(result,SubgStmt1left,OptAttrList1right),rest671)
 end
| (43,(_,(MlyValue.NodeId NodeId1,_,NodeId1right))::(_,(_,EDGEOP1left,
_))::rest671) => let val result=MlyValue.EdgeRHS(fn _ => let val 
NodeId as NodeId1=NodeId1 ()
 in ([Node NodeId]) end
)
 in (LrTable.NT 21,(result,EDGEOP1left,NodeId1right),rest671) end
| (44,(_,(MlyValue.EdgeRHS EdgeRHS1,_,EdgeRHS1right))::(_,(
MlyValue.NodeId NodeId1,_,_))::(_,(_,EDGEOP1left,_))::rest671) => let 
val result=MlyValue.EdgeRHS(fn _ => let val NodeId as NodeId1=NodeId1 
()
val EdgeRHS as EdgeRHS1=EdgeRHS1 ()
 in ((Node NodeId)::EdgeRHS) end
)
 in (LrTable.NT 21,(result,EDGEOP1left,EdgeRHS1right),rest671) end
| (45,(_,(MlyValue.SubgStmt SubgStmt1,_,SubgStmt1right))::(_,(_,
EDGEOP1left,_))::rest671) => let val result=MlyValue.EdgeRHS(fn _ => 
let val SubgStmt as SubgStmt1=SubgStmt1 ()
 in ([Subgraph SubgStmt]) end
)
 in (LrTable.NT 21,(result,EDGEOP1left,SubgStmt1right),rest671) end
| (46,(_,(MlyValue.EdgeRHS EdgeRHS1,_,EdgeRHS1right))::(_,(
MlyValue.SubgStmt SubgStmt1,_,_))::(_,(_,EDGEOP1left,_))::rest671) => 
let val result=MlyValue.EdgeRHS(fn _ => let val SubgStmt as SubgStmt1=
SubgStmt1 ()
val EdgeRHS as EdgeRHS1=EdgeRHS1 ()
 in ((Subgraph SubgStmt)::EdgeRHS) end
)
 in (LrTable.NT 21,(result,EDGEOP1left,EdgeRHS1right),rest671) end
| (47,(_,(_,_,RBRACE1right))::(_,(MlyValue.StmtList StmtList1,_,_))::_
::(_,(MlyValue.SubgHdr SubgHdr1,SubgHdr1left,_))::rest671) => let val 
result=MlyValue.SubgStmt(fn _ => let val SubgHdr as SubgHdr1=SubgHdr1 
()
val StmtList as StmtList1=StmtList1 ()
 in (fn g => (StmtList (G.addSubgraph(g,SubgHdr,NONE)))) end
)
 in (LrTable.NT 22,(result,SubgHdr1left,RBRACE1right),rest671) end
| (48,(_,(_,_,RBRACE1right))::(_,(MlyValue.StmtList StmtList1,_,_))::(
_,(_,LBRACE1left,_))::rest671) => let val result=MlyValue.SubgStmt(fn 
_ => let val StmtList as StmtList1=StmtList1 ()
 in (fn g => (StmtList (G.addSubgraph(g,anonymous(),NONE)))) end
)
 in (LrTable.NT 22,(result,LBRACE1left,RBRACE1right),rest671) end
| (49,(_,(MlyValue.SubgHdr SubgHdr1,SubgHdr1left,SubgHdr1right))::
rest671) => let val result=MlyValue.SubgStmt(fn _ => let val SubgHdr
 as SubgHdr1=SubgHdr1 ()
 in (fn g => findSubgraph(g, SubgHdr)) end
)
 in (LrTable.NT 22,(result,SubgHdr1left,SubgHdr1right),rest671) end
| (50,(_,(MlyValue.SYMBOL SYMBOL1,_,SYMBOL1right))::(_,(_,
SUBGRAPH1left,_))::rest671) => let val result=MlyValue.SubgHdr(fn _
 => let val SYMBOL as SYMBOL1=SYMBOL1 ()
 in (SYMBOL) end
)
 in (LrTable.NT 23,(result,SUBGRAPH1left,SYMBOL1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.File x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Graph_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun GRAPH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DIGRAPH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SUBGRAPH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun STRICT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun NODE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun EDGE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EDGEOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun SYMBOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.SYMBOL (fn () => i),p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun AT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
end
end
