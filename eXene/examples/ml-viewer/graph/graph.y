(* graph-gram
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Parser specification for "dot" style syntax.
 *)

exception Error

datatype vertex
  = Node of (string * string option) 
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

%%
%header (functor GraphLrValsFun(
           structure Token : TOKEN
           structure G : ATTR_GRAPH 
           val graph_info : unit -> G.graph_info
           val node_info : unit -> G.node_info
           val edge_info : unit -> G.edge_info)
        )
%pos int
%name Graph
%verbose
(* %pure *)
%noshift EOF
%eop EOF
%term
    GRAPH | DIGRAPH | SUBGRAPH | STRICT
  | NODE | EDGE | EDGEOP | SYMBOL of string
  | COLON | SEMICOLON | COMMA | LBRACE | LBRACKET | LPAREN 
  | RBRACE | RBRACKET | RPAREN | EQUAL | DOT | AT | EOF
%nonterm
    File of G.graph option
  | GraphType of string
  | AttrClass of (((string * string) list) -> G.graph -> G.graph)
  | InsideAttrList of (string * string) list
  | Optcomma
  | AttrList of (string * string) list
  | RecAttrList of (string * string) list
  | OptAttrList of (string * string) list
  | AttrSet of (string * string)
  | StmtList of (G.graph -> G.graph)
  | StmtList1 of (G.graph -> G.graph)
  | Stmt of (G.graph -> G.graph)
  | Stmt1 of (G.graph -> G.graph)
  | AttrStmt of (G.graph -> G.graph)
  | NodeId of (string * string option)
  | NodeName of string
  | NodePort of string option
  | PortLocation of string
  | PortAngle of string
  | NodeStmt of (G.graph -> G.graph)
  | EdgeStmt of (G.graph -> G.graph)
  | EdgeRHS of vertex list
  | SubgStmt of (G.graph -> G.graph)
  | SubgHdr of string
%left SUBGRAPH (* to avoid subgraph hdr shift/reduce conflict *)
%left LBRACE

%%

File            :    GraphType SYMBOL LBRACE StmtList RBRACE 
                      (let val g = G.mkGraph{name=SYMBOL,graph_info=graph_info,
                        edge_info=edge_info, node_info=node_info, info=NONE}
                       in 
                         G.setAttr (G.GRAPH g) ("graph_type",GraphType);
                         SOME(StmtList g) 
                       end)
                |    (* empty *) (NONE)
GraphType       :    GRAPH ("g")
                |    STRICT GRAPH ("sg")
                |    DIGRAPH ("dg")
                |    STRICT DIGRAPH ("sdg")
AttrClass       :    GRAPH (fn al => fn g => (app (GA g) al; g))
                |    NODE  (fn al => fn g => (app (PNA g) al; g))
                |    EDGE  (fn al => fn g => (app (PEA g) al; g))
InsideAttrList  :    AttrSet Optcomma InsideAttrList 
                      (AttrSet::InsideAttrList)
                |    (* empty *) ([])
Optcomma        :    (* empty *) ()
                |    COMMA ()
AttrList        :    LBRACKET InsideAttrList RBRACKET (InsideAttrList)
RecAttrList     :    RecAttrList AttrList (RecAttrList@AttrList)
                |    (* empty *) ([])
OptAttrList     :    RecAttrList (RecAttrList)
AttrSet         :    SYMBOL EQUAL SYMBOL  ((SYMBOL1,SYMBOL2))
StmtList        :    StmtList1 (StmtList1)
                |    (* empty *) (fn g => g)
StmtList1       :    Stmt (Stmt)
                |    StmtList1 Stmt (Stmt o StmtList1)
Stmt            :    Stmt1 (Stmt1)
                |    Stmt1 SEMICOLON (Stmt1)
Stmt1           :    NodeStmt (NodeStmt)
                |    EdgeStmt (EdgeStmt)
                |    AttrStmt (AttrStmt)
                |    SubgStmt (fn g => (SubgStmt g; g))
AttrStmt        :    AttrClass AttrList (AttrClass AttrList)
                |    AttrSet 
                      (fn g => (GA g (#1 AttrSet,#2 AttrSet);g))
NodeId          :    NodeName NodePort (NodeName,NodePort)
NodeName        :    SYMBOL (SYMBOL)
NodePort        :    (* empty *) (NONE)
                |    PortLocation (SOME PortLocation)
                |    PortAngle                         (* undocumented *)
                      (SOME PortAngle)
                |    PortAngle PortLocation            (* undocumented *)
                      (SOME(PortAngle^PortLocation))
                |    PortLocation PortAngle            (* undocumented *)
                      (SOME(PortLocation^PortAngle))
PortLocation    :    COLON SYMBOL (":"^SYMBOL)
                |    COLON LPAREN SYMBOL COMMA SYMBOL RPAREN 
                      (concat[":(",SYMBOL1,",",SYMBOL2,")"])
PortAngle       :    AT SYMBOL ("@"^SYMBOL)
NodeStmt        :    NodeId OptAttrList 
                      (fn g => (app (NA (G.getNode(g, #1 NodeId,NONE))) OptAttrList; g))
EdgeStmt        :    NodeId EdgeRHS OptAttrList 
                      (mkEdges((Node NodeId)::EdgeRHS,OptAttrList))
                |    SubgStmt EdgeRHS OptAttrList
                      (mkEdges((Subgraph SubgStmt)::EdgeRHS,OptAttrList))
EdgeRHS         :    EDGEOP NodeId ([Node NodeId])
                |    EDGEOP NodeId EdgeRHS ((Node NodeId)::EdgeRHS)
                |    EDGEOP SubgStmt ([Subgraph SubgStmt])
                |    EDGEOP SubgStmt EdgeRHS ((Subgraph SubgStmt)::EdgeRHS)
SubgStmt        :    SubgHdr LBRACE StmtList RBRACE %prec LBRACE 
                      (fn g => (StmtList (G.addSubgraph(g,SubgHdr,NONE))))
                |    LBRACE StmtList RBRACE
                      (fn g => (StmtList (G.addSubgraph(g,anonymous(),NONE))))
                |    SubgHdr %prec SUBGRAPH 
                      (fn g => findSubgraph(g, SubgHdr))
SubgHdr         :    SUBGRAPH SYMBOL (SYMBOL)

