(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* parser.sml:  This is a parser driver for LR tables with an error-recovery
   routine added to it.  The routine used is described in detail in this
   article:

	'A Practical Method for LR and LL Syntactic Error Diagnosis and
	 Recovery', by M. Burke and G. Fisher, ACM Transactions on
	 Programming Langauges and Systems, Vol. 9, No. 2, April 1987,
	 pp. 164-197.

    This program is an implementation is the partial, deferred method discussed
    in the article.  The algorithm and data structures used in the program
    are described below.  

    This program assumes that all semantic actions are delayed.  A semantic
    action should produce a function from unit -> value instead of producing the
    normal value.  The parser returns the semantic value on the top of the
    stack when accept is encountered.  The user can deconstruct this value
    and apply the unit -> value function in it to get the answer.

    It also assumes that the lexer is a lazy stream.

    Data Structures:
    ----------------
	
	* The parser:

	   The state stack has the type

		 (state * (semantic value * line # * line #)) list

	   The parser keeps a queue of (state stack * lexer pair).  A lexer pair
	 consists of a terminal * value pair and a lexer.  This allows the 
	 parser to reconstruct the states for terminals to the left of a
	 syntax error, and attempt to make error corrections there.

	   The queue consists of a pair of lists (x,y).  New additions to
	 the queue are cons'ed onto y.  The first element of x is the top
	 of the queue.  If x is nil, then y is reversed and used
	 in place of x.

    Algorithm:
    ----------

	* The steady-state parser:  

	    This parser keeps the length of the queue of state stacks at
	a steady state by always removing an element from the front when
	another element is placed on the end.

	    It has these arguments:

	   stack: current stack
	   queue: value of the queue
	   lexPair ((terminal,value),lex stream)

	When SHIFT is encountered, the state to shift to and the value are
	are pushed onto the state stack.  The state stack and lexPair are
	placed on the queue.  The front element of the queue is removed.

	When REDUCTION is encountered, the rule is applied to the current
	stack to yield a triple (nonterm,value,new stack).  A new
	stack is formed by adding (goto(top state of stack,nonterm),value)
	to the stack.

	When ACCEPT is encountered, the top value from the stack and the
	lexer are returned.

	When an ERROR is encountered, fixError is called.  FixError
	takes the arguments to the parser, fixes the error if possible and
        returns a new set of arguments.

	* The distance-parser:

	This parser includes an additional argument distance.  It pushes
	elements on the queue until it has parsed distance tokens, or an
	ACCEPT or ERROR occurs.  It returns a stack, lexer, the number of
	tokens left unparsed, a queue, and an action option.
*)

signature FIFO = 
  sig type 'a queue
      val empty : 'a queue
      exception Empty
      val get : 'a queue -> 'a * 'a queue
      val put : 'a * 'a queue -> 'a queue
  end

(* drt (12/15/89) -- the functor should be used in development work, but
   it wastes space in the release version.

functor ParserGen(structure LrTable : LR_TABLE
		  structure Stream : STREAM) : LR_PARSER =
*)

abstraction LrParser : LR_PARSER =
   struct
      structure LrTable = LrTable
      structure Stream = Stream

      structure Token : TOKEN =
	struct
	    structure LrTable = LrTable
	    datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
	    val sameToken = fn (TOKEN(t,_),TOKEN(t',_)) => t=t'
        end

      open LrTable
      open Token

      val DEBUG1 = false
      val DEBUG2 = false
      exception ParseError
      exception ParseImpossible of int

      abstraction Fifo : FIFO =
        struct
	  type 'a queue = ('a list * 'a list)
	  val empty = (nil,nil)
	  exception Empty
	  fun get(a::x, y) = (a, (x,y))
	    | get(nil, nil) = raise Empty
	    | get(nil, y) = get(rev y, nil)
 	  fun put(a,(x,y)) = (x,a::y)
        end

      type ('a,'b) elem = (state * ('a * 'b * 'b))
      type ('a,'b) stack = ('a,'b) elem list
      type ('a,'b) lexv = ('a,'b) token
      type ('a,'b) lexpair = ('a,'b) lexv * (('a,'b) lexv Stream.stream)
      type ('a,'b) distanceParse =
		 ('a,'b) lexpair *
		 ('a,'b) stack * 
		 (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
		 int ->
		   ('a,'b) lexpair *
		   ('a,'b) stack * 
		   (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
		   int *
		   action option

      type ('a,'b) ecRecord =
	 {is_keyword : term -> bool,
          preferred_subst : term -> term list,
	  preferred_insert : term -> bool,
	  error : string * 'b * 'b -> unit,
	  errtermvalue : term -> 'a,
	  terms : term list,
	  showTerminal : term -> string,
	  noShift : term -> bool}

      local 
	 val print = fn s => output(std_out,s)
	 val println = fn s => (print s; print "\n")
	 val showState = fn (STATE s) => "STATE " ^ (makestring s)
      in
        fun printStack(stack: ('a,'b) stack, n: int) =
         case stack
           of (state,_) :: rest =>
                 (print("\t" ^ makestring n ^ ": ");
                  println(showState state);
                  printStack(rest, n+1))
            | nil => ()
                
        fun prAction showTerminal
		 (stack as (state,_) :: _, next as (TOKEN (term,_),_), action) =
             (println "Parse: state stack:";
              printStack(stack, 0);
              print("       state="
                         ^ showState state	
                         ^ " next="
                         ^ showTerminal term
                         ^ " action="
                        );
              case action
                of SHIFT state => println ("SHIFT " ^ (showState state))
                 | REDUCE i => println ("REDUCE " ^ (makestring i))
                 | ERROR => println "ERROR"
		 | ACCEPT => println "ACCEPT")
        | prAction _ (_,_,action) = ()
     end

    (* ssParse: parser which maintains the queue of (state * lexvalues) in a
	steady-state.  It takes a table, showTerminal function, saction
	function, and fixError function.  It parses until an ACCEPT is
	encountered, or an exception is raised.  When an error is encountered,
	fixError is called with the arguments of parseStep (lexv,stack,and
	queue).  It returns the lexv, and a new stack and queue adjusted so
	that the lexv can be parsed *)
	
    val ssParse =
      fn (table,showTerminal,saction,fixError,arg) =>
	let val prAction = prAction showTerminal
	    val action = LrTable.action table
	    val goto = LrTable.goto table
	    fun parseStep(args as
			 (lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
				      lexer
				      ),
			  stack as (state,_) :: _,
			  queue)) =
	      let val nextAction = action (state,terminal)
	          val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
			  else ()
	      in case nextAction
		 of SHIFT s =>
		  let val newStack = (s,value) :: stack
		      val newLexPair = Stream.get lexer
		      val (_,newQueue) =Fifo.get(Fifo.put((newStack,newLexPair),
							    queue))
		  in parseStep(newLexPair,(s,value)::stack,newQueue)
		  end
		 | REDUCE i =>
		     (case saction(i,leftPos,stack,arg)
		      of (nonterm,value,stack as (state,_) :: _) =>
		          parseStep(lexPair,(goto(state,nonterm),value)::stack,
				    queue)
		       | _ => raise (ParseImpossible 197))
		 | ERROR => parseStep(fixError args)
		 | ACCEPT => 
			(case stack
			 of (_,(topvalue,_,_)) :: _ =>
				let val (token,restLexer) = lexPair
				in (topvalue,Stream.cons(token,restLexer))
				end
			  | _ => raise (ParseImpossible 202))
	      end
	    | parseStep _ = raise (ParseImpossible 204)
	in parseStep
	end

    (*  distanceParse: parse until n tokens are shifted, or accept or
	error are encountered.  Takes a table, showTerminal function, and
	semantic action function.  Returns a parser which takes a lexPair
	(lex result * lexer), a state stack, a queue, and a distance
	(must be > 0) to parse.  The parser returns a new lex-value, a stack
	with the nth token shifted on top, a queue, a distance, and action
	option. *)

    val distanceParse =
      fn (table,showTerminal,saction,arg) =>
	let val prAction = prAction showTerminal
	    val action = LrTable.action table
	    val goto = LrTable.goto table
	    fun parseStep(lexPair,stack,queue,0) = (lexPair,stack,queue,0,NONE)
	      | parseStep(lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
				      lexer
				     ),
			  stack as (state,_) :: _,
			  queue,distance) =
	      let val nextAction = action(state,terminal)
	          val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
			  else ()
	      in case nextAction
		 of SHIFT s =>
		  let val newStack = (s,value) :: stack
		      val newLexPair = Stream.get lexer
		  in parseStep(newLexPair,(s,value)::stack,
			       Fifo.put((newStack,newLexPair),queue),distance-1)
		  end
		 | REDUCE i =>
		    (case saction(i,leftPos,stack,arg)
		      of (nonterm,value,stack as (state,_) :: _) =>
		         parseStep(lexPair,(goto(state,nonterm),value)::stack,
				 queue,distance)
		      | _ => raise (ParseImpossible 240))
		 | ERROR => (lexPair,stack,queue,distance,SOME nextAction)
		 | ACCEPT => (lexPair,stack,queue,distance,SOME nextAction)
	      end
	   | parseStep _ = raise (ParseImpossible 242)
	in parseStep : ('_a,'_b) distanceParse 
	end

(* mkFixError: function to create fixError function which adjusts parser state
   so that parse may continue in the presence of an error *)

val mkFixError = fn ({is_keyword,preferred_subst,terms,errtermvalue,
		      preferred_insert,noShift,
		      showTerminal,error,...} : ('_a,'_b) ecRecord,
		      distanceParse : ('_a,'_b) distanceParse,
		      minAdvance,maxAdvance) =>
  let fun FixError(lexv as (TOKEN (term,value as (_,leftPos,_)),_),
		   stack,queue) =
    let val lexVList = map (fn t => TOKEN (t,(errtermvalue t,leftPos,leftPos)))
		       terms
	val _ = if DEBUG2 then
			error("syntax error found at " ^ (showTerminal term),
			      leftPos,leftPos)
		else ()

	val minDelta = 3

	(* pull all the state * lexv elements from the queue *)

	val stateList = 
	   let fun f q = let val (elem,newQueue) = Fifo.get q
			 in elem :: (f newQueue)
			 end handle Fifo.Empty => nil
	   in f queue
	   end

	(* now number elements of stateList, giving distance from
	   error token *)

	val (_,numStateList) = List.fold (fn (a,(num,r)) => (num+1,(a,num)::r))
				stateList (0,nil)

	(* Represent the set of potential changes as a linked list.

	   Values of datatype Change hold information about a potential change.

	   oper = oper to be applied
	   pos = the # of the element in stateList that would be altered.
	   distance = the number of tokens beyond the error token which the
	     change allows us to parse.
	   new = new terminal * value pair at that point
	   orig = original terminal * value pair at the point being changed.
	 *)

	datatype oper = INSERT | DELETE  | SUBST
	datatype ('a,'b) change = CHANGE of
	   {oper : oper, pos : int, distance : int,
	    new : ('a,'b) lexv, orig : ('a,'b) lexv}

	val operToString = 
	       fn INSERT => "INSERT "
		| SUBST  => "SUBST "
		| DELETE => "DELETE "

	 val printChange = fn c =>
	  let val CHANGE {oper,distance,new=TOKEN (t,_),
			  orig=TOKEN (t',_),pos,...} = c
	  in (print ("{distance= " ^ (makestring distance));
	      print (",orig = " ^ (showTerminal t'));
	      print (",new = " ^ (showTerminal t));
	      print (",oper= " ^ (operToString oper));
	      print (",pos= " ^ (makestring pos));
	      print "}\n")
	  end

	val printChangeList = app printChange

(* parse: given a lexPair, a stack, and the distance from the error
   token, return the distance past the error token that we are able to parse.*)

	fun parse (lexPair,stack,queuePos : int) =
	    let val (_,_,_,distance,action) =
		  distanceParse(lexPair,stack,Fifo.empty,queuePos+maxAdvance+1)
	    in maxAdvance - distance - 1
	    end

(* foldStateList: accumulates results while scanning numStateList *)


	fun foldStateList f start = List.fold f numStateList start

(* foldLexVList: accumulates results while scanning lexVList *)

	fun foldLexVList f start = List.fold f lexVList start

(* deleteFold: function which accumulates results of deleting the
   current terminal.  Does not delete the current terminal if that terminal
   cannot be shifted *)

	val deleteFold =
		fn (((stack,lexPair as (orig as TOKEN (term,_),lexer)),
			queuePos),r) =>
		 if noShift term then r
		 else
		   let val newLexPair as (new,_) = Stream.get lexer
		       val distance = parse(newLexPair,stack,queuePos-1)
		   in if distance >= minAdvance then
			CHANGE {pos=queuePos,distance=distance,orig=orig,
				new=new,oper=DELETE} :: r
		      else r
		   end


(* insertFold: accumulate results of trying to insert tokens before
   the current terminal *)

	val insertFold =
	   fn (((stack,lexPair as (orig,lexer)),queuePos),r) =>
	    let val lexer = Stream.cons lexPair
	    in foldLexVList (fn (newLexV,r) =>
		let val distance = parse((newLexV,lexer),stack,queuePos+1)
		in if distance >= minAdvance
			 then CHANGE{pos=queuePos,distance=distance,orig=orig,
					new=newLexV,oper=INSERT} :: r
			 else r
		end) r
	    end

(* substFold: accumulate results of deleting the current terminal
   and then trying to insert tokens *)

	val substFold =
	    fn (((stack,lexPair as (orig as TOKEN (term,_),lexer)),queuePos),
		r) =>
	      if noShift term then r
	      else
		  foldLexVList (fn (newLexV,r) =>
		   let val distance = parse((newLexV,lexer),stack,queuePos)
		   in if distance >= minAdvance then
			   CHANGE{pos=queuePos,distance=distance,orig=orig,
				  new=newLexV,oper=SUBST} :: r
		     else r
		   end) r

	val changes = (foldStateList insertFold nil) @
			  (foldStateList substFold nil) @
				(foldStateList deleteFold nil)

	val findMaxDist = fn l => 
	  fold (fn (CHANGE {distance,...},high) => max(distance,high)) l 0

(* maxDist: max distance past error taken that we could parse *)

	val maxDist = findMaxDist changes

(* sieve: keep only the elements of a list for which pred is true *)

	val sieve = fn pred => fn l => 
	  fold (fn (elem,rest) => if pred elem then elem::rest else rest) l nil

(* remove changes which did not parse maxDist tokens past the error token *)

	val changes = sieve (fn CHANGE{distance=a,...} => a = maxDist) changes

(* Find preferred elements *)

        val preferredInsertChanges =
		sieve (fn CHANGE {new=TOKEN (term,_),oper=INSERT,...} => 
				 preferred_insert term
		        | _ => false) changes

        val preferredSubstChanges =
		sieve
		    (fn CHANGE {new=TOKEN(t,_),orig=TOKEN (t',_),
				oper=SUBST,...} =>
			  List.exists (fn a => a =t) (preferred_subst t')
		      | _ => false) changes

        val _ = if DEBUG2 then
	    (print "preferred insert:\n";
	     printChangeList preferredInsertChanges;
	     print "preferred subst:\n";
	     printChangeList preferredSubstChanges
	    ) else ()

(* Remove keywords which don't meet the long parse check
   (minAdvance+minDelta) *)

         val changes =
	    sieve (fn CHANGE {new=TOKEN (term,_),distance,...} =>
		(not (is_keyword term) orelse distance >= minAdvance+minDelta))
			changes


         val changes =
	       preferredInsertChanges @ (preferredSubstChanges @ changes)

         in case changes 
	     of (l as _ :: _) =>
	        let fun print_msg (CHANGE {new=TOKEN (term,_),oper,
					   orig=TOKEN (t',(_,leftPos,rightPos)),
					   ...}) =
		     let val s = 
		       case oper
			 of DELETE => "deleting " ^ (showTerminal t')
			  | INSERT => "inserting " ^ (showTerminal term)
		          | SUBST => "replacing " ^ (showTerminal t') ^
				   " with " ^ (showTerminal term)
		     in error ("syntax error: " ^ s,leftPos,rightPos)
		     end
		   
		   val a = 
		     (if length l > 1 andalso DEBUG2 then
			(print "multiple fixes possible; could fix it by:\n";
		 	 map print_msg l;
		 	 print "chosen correction:\n")
		      else ();
		      print_msg (hd l); (hd l))

		    (* findNth: find nth queue entry from the error
		       entry.  Returns the Nth queue entry and the  portion of
		       the queue from the beginning to the nth-1 entry.  The
		       error entry is at the end of the queue.

			Examples:

			queue = a b c d e
		        findNth 0 = (e,a b c d)
			findNth 1 =  (d,a b c)
		    *)

		    val findNth = fn n =>
		     let fun f (h::t,0) = (h,rev t)
			   | f (h::t,n) = f(t,n-1)
			   | f (nil,_) = let exception FindNth
					   in raise FindNth
					   end
		     in f (rev stateList,n)
		     end
		
		    val CHANGE {pos,oper,new=TOKEN (term,(value,_,_)),...} = a
		    val (last,queueFront) = findNth pos
		    val (stack,lexPair as (orig,lexer)) = last
		    val TOKEN (_,(_,leftPos,rightPos)) = orig
 		    val newLexV = TOKEN (term,(value,leftPos,rightPos))

		    val newLexPair =
			case oper
			of DELETE => Stream.get lexer
			 | SUBST => (newLexV,lexer)
			 | INSERT => (newLexV,Stream.cons lexPair)

		    val restQueue = 
		     Fifo.put((stack,newLexPair),
			      revfold Fifo.put queueFront Fifo.empty)

	 	   val (lexPair,stack,queue,_,_) =
			distanceParse(newLexPair,stack,restQueue,pos)

	      in (lexPair,stack,queue)
	      end
	  | nil => (error("syntax error found at " ^ (showTerminal term),
			      leftPos,leftPos); raise ParseError)
	end
     in FixError
     end

   val parse = fn {arg,table,lexer,saction,void,lookahead,
		   ec=ec as {showTerminal,...} : ('_a,'_b) ecRecord} =>
	let val distance = 15   (* defer distance tokens *)
	    val minAdvance = 1  (* must parse at least 1 token past error *)
	    val maxAdvance = max(lookahead,0)(* max distance for parse check *)
	    val lexPair = Stream.get lexer
	    val (TOKEN (_,(_,leftPos,_)),_) = lexPair
	    val startStack = [(initialState table,(void,leftPos,leftPos))]
	    val startQueue = Fifo.put((startStack,lexPair),Fifo.empty)
	    val distanceParse = distanceParse(table,showTerminal,saction,arg)
	    val fixError = mkFixError(ec,distanceParse,minAdvance,maxAdvance)
	    val ssParse = ssParse(table,showTerminal,saction,fixError,arg)
	    fun loop (lexPair,stack,queue,_,SOME ACCEPT) =
		   ssParse(lexPair,stack,queue)
	      | loop (lexPair,stack,queue,0,_) = ssParse(lexPair,stack,queue)
	      | loop (lexPair,stack,queue,distance,SOME ERROR) =
		 let val (lexPair,stack,queue) = fixError(lexPair,stack,queue)
		 in loop (distanceParse(lexPair,stack,queue,distance))
		 end
	      | loop _ = let exception ParseInternal
			 in raise ParseInternal
			 end
	in loop (distanceParse(lexPair,startStack,startQueue,distance))
	end
 end;

(* drt (12/15/89) -- needed only when the code above is functorized

structure LrParser = ParserGen(structure LrTable=LrTable
			     structure Stream=Stream);
*)
