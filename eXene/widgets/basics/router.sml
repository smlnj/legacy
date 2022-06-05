(* router.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories  See COPYRIGHT file for details.
 *
 * Generic event routers. 
 *)

signature ROUTER = 
  sig

    structure EXB : EXENE_BASE
    structure Interact : INTERACT

    exception NotFound

    type router

    val mkRouter : Interact.in_env * Interact.out_env * 
      (EXB.window * Interact.out_env) list -> router

    val addChild : router -> EXB.window * Interact.out_env -> unit
    val delChild : router -> EXB.window -> unit
    val getChildEnv : router -> EXB.window -> Interact.out_env
      (* Return environment associated in router with given window.
       * Raise NotFound if not found.
       *)

    val routePair : Interact.in_env * Interact.out_env * Interact.out_env -> unit
    (* added by ddeboer: *)
    val bufferEvt : ('a Interact.addr_msg -> unit CML.event) -> ('a Interact.addr_msg -> unit CML.event)
    (* end added *)
  end (* ROUTER *)

structure Router : ROUTER = struct

  structure EXB = EXeneBase
  structure Interact = Interact

  exception NotFound

  open CML EXeneBase Interact

  datatype route_req = 
    AddChild of (window * out_env)
  | DelChild of window
  | GetChild of window

  datatype router = Router of {
    reqch : route_req chan,
    replych : out_env option chan
  }

  (* make a buffer-handler; ddeboer, fall 2004. 
   * Try to synchronize on inev, queueing value v; or
   * Try to synchronize on outev v if queue is nonempty, where v is head of queue.
   * bufferEvt : ('a addr_msg -> unit event) -> ('a addr_msg -> unit event)
   *)
  (* note: should use wrapQueue where possible. *)
  fun bufferEvt outStm : ('a addr_msg -> unit event) =
      let val inCh = channel()
          fun loop ([], [])   = loop([(recv inCh)],[])
            | loop ([], rear) = loop(rev rear,[])
            | loop (front as (msgOut::r), rear) =
                (select[
                    (wrap((outStm msgOut), fn () => loop(r,rear))),
                    (wrap(recvEvt inCh, fn msg => loop(front,msg::rear)))
                ])
      fun inEvt msg = (sendEvt (inCh,msg))
          in spawn(fn () => loop ([],[])); inEvt end
  (* end addition *)
  
  (* The router is constructed with an in_env, out_env for a
   * composite widget and an initial distribution
   * list. The router listens for an event on the input environment, 
   * resolves the event to an output environment, and passes the event
   * along.
   *)
  fun mkRouter (InEnv{m, k, ci,...}, myOut, outList) = let
    val routeReqCh = channel() and routeReplyCh = channel()
          
    val winMap = newMap()
    val find = lookup winMap
    (* val findMsg = addrLookup winMap *)
    fun findMsg m = addrLookup winMap m
    val insert = insert winMap
    val remove = remove winMap
             
    fun mEvt (OutEnv {m,...}) = m
    fun kEvt (OutEnv {k,...}) = k
    fun ciEvt (OutEnv {ci,...}) = ci
          
    (* modified by ddeboer; original:
     fun handleReq (AddChild item) = insert item*)
    val myOut = case myOut of OutEnv{m,k,ci,co} => 
            OutEnv{m=(bufferEvt m),k=(bufferEvt k),ci=(bufferEvt ci),co=co}
    fun handleReq (AddChild (w,OutEnv{m,k,ci,co})) = 
        insert (w,OutEnv{m=(bufferEvt m),k=(bufferEvt k),ci=(bufferEvt ci),co=co}) 
    (* end modification *)
      | handleReq (DelChild w) = ((remove w; ()) handle _ => ())
      | handleReq (GetChild w) = send(routeReplyCh, (SOME(find w)) handle _ => NONE)
          
    fun handleEvt proj msg = (
      case stripMsg msg of 
        Here _ => select [
            proj myOut msg,
            wrap (recvEvt routeReqCh, fn req => (handleReq req; handleEvt proj msg))
          ]
      | ToChild msg' => sync (proj (findMsg msg') msg'))
          
    val evt = choose [
      wrap (recvEvt routeReqCh, handleReq),
      wrap (m, handleEvt mEvt),
      wrap (k, handleEvt kEvt),
      wrap (ci, handleEvt ciEvt)
    ]
          
    fun loop () = (sync evt; loop ())
    fun init [] = ()
      | init (item::rest) = (insert item; init rest)
  in
    init outList;
    XDebug.xspawn ("Router", loop);
    Router {reqch = routeReqCh, replych = routeReplyCh}
  end
          
  fun addChild (Router{reqch,...}) arg = send (reqch, AddChild arg)
  fun delChild (Router{reqch,...}) arg = send (reqch, DelChild arg)
  fun getChildEnv (Router{reqch, replych}) arg = (
    send (reqch, GetChild arg);
    case recv replych of
      NONE => raise NotFound
    | SOME e => e
  )
        
  (* Simple router for a composite widget with a single child.
   *)
  fun routePair (InEnv{m, k, ci,...}, parentOut, childOut) = let
          
    fun mEvt  (OutEnv {m,...})  = m (* mouse_msg addr_msg -> unit event *)
    fun kEvt  (OutEnv {k,...})  = k
    fun ciEvt (OutEnv {ci,...}) = ci
          
    (* added by ddeboer: *)
    val childOut = 
        case childOut of OutEnv{m,k,ci,co} => 
            OutEnv{m= (bufferEvt m),
                   k= (bufferEvt k),
                   ci=(bufferEvt ci),
                   co=co}
    (* end added. *)
    
    fun handleEvt proj msg =
      case stripMsg msg of
        Here _ => sync (proj parentOut msg)
      | ToChild msg' => sync (proj childOut msg')
          
    fun loop () =
      loop (sync(choose[
        wrap (m, handleEvt mEvt),
        wrap (k, handleEvt kEvt),
        wrap (ci, handleEvt ciEvt)
      ]))
          
  in
    XDebug.xspawn ("Router2", loop);
    ()
  end
          
end (* Router *)

