(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor HeaderFun () : HEADER =
  struct
	val DEBUG = true

	type pos = int
        val lineno = ref 0
        val text = ref (nil: string list)
        type inputSource = {name : string,
			    errStream : outstream,
			    inStream : instream,
			    errorOccurred : bool ref}

	val newSource = fn (s : string,i : instream ,errs : outstream) =>
			{name=s,errStream=errs,inStream=i,
			 errorOccurred = ref false}
			
	val errorOccurred = fn (s : inputSource) =>fn () => !(#errorOccurred s)

	val pr = fn out : outstream => fn s : string => output(out,s)

	val error = fn {name,errStream, errorOccurred,...} : inputSource =>
	      let val pr = pr errStream
	      in fn l : pos => fn msg : string =>
	          (pr name; pr ", line "; pr (makestring l); pr ": Error: ";
	           pr msg; pr "\n"; errorOccurred := true)
	      end

	val warn = fn {name,errStream, errorOccurred,...} : inputSource =>
	      let val pr = pr errStream
	      in fn l : pos => fn msg : string =>
	          (pr name; pr ", line "; pr (makestring l); pr ": Warning: ";
	           pr msg; pr "\n")
	      end

        datatype prec = LEFT | RIGHT | NONASSOC

	type symbol = string * pos
        val symbolName = fn s : symbol => #1 s
        val symbolPos = fn s : symbol => #2 s
        val symbolMake = fn s : symbol => s
    
	type ty = string
        val tyName = fn i => i
        val tyMake = fn i => i
 
	datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
			   FUNCTOR of string  | START_SYM of symbol |
			   NSHIFT of symbol list | POS of string | PURE |
			   PARSE_ARG of string * string
			   
	datatype declData = DECL of
			{eop : symbol list,
			 keyword : symbol list,
			 nonterm : (symbol*ty option) list option,
			 prec : (prec * (symbol list)) list,
			 prefer : symbol list,
			 subst: (symbol*symbol) list,
			 term : (symbol* ty option) list option,
			 control : control list,
			 value : (symbol * string) list}

	type rhsData = {rhs:symbol list,code:string, prec:symbol option} list
	datatype rule = RULE of {lhs : symbol, rhs : symbol list,
		                 code : string, prec : symbol option}

 	type parseResult = string * declData * rule list
        val getResult = fn p => p

	fun join_decls
	      (DECL {eop=e,control=c,keyword=k,nonterm=n,prec, prefer=p,
		subst=su,term=t,value=v}:declData,
	       DECL {eop=e',control=c',keyword=k',nonterm=n',prec=prec',
		     prefer=p',	subst=su',term=t',value=v'} : declData,
               inputSource,pos) =
	  let val ignore = fn s => 
	                (warn inputSource pos ("ignoring duplicate " ^ s ^
					    " declaration"))
	      val join = fn (e,NONE,NONE) => NONE
			  | (e,NONE,a) => a
			  | (e,a,NONE) => a
			  | (e,a,b) => (ignore e; a)
	      fun mergeControl (nil,a) = [a]
		| mergeControl (l as h::t,a) =
		     case (h,a)
	  	     of (PARSER_NAME _,PARSER_NAME n1) => (ignore "%name"; l)
		      | (FUNCTOR _,FUNCTOR _) => (ignore "%header"; l)
		      | (PARSE_ARG _,PARSE_ARG _) => (ignore "%arg"; l)
		      | (START_SYM _,START_SYM s) => (ignore "%start"; l)
		      | (POS _,POS _) => (ignore "%pos"; l)
		      | (NSHIFT a,NSHIFT b) => (NSHIFT (a@b)::t)
		      | _ => h :: mergeControl(t,a)
	      fun loop (nil,r) = r
		| loop (h::t,r) = loop(t,mergeControl(r,h))
	 in DECL {eop=e@e',control=loop(c',c),keyword=k'@k,
	    nonterm=join("%nonterm",n,n'), prec=prec@prec',
	    prefer = p@p', subst=su@su', term=join("%term",t,t'),value=v@v'} :
	           declData
	end
end;

structure Header = HeaderFun();
      
