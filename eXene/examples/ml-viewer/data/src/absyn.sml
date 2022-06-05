(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi *)

structure Absyn : ABSYN =
   struct
       datatype exp = CODE of string
	            | EAPP of exp * exp
                    | EINT of int
                    | ETUPLE of exp list
	            | EVAR of string
                    | FN of pat * exp
                    | LET of decl list * exp
                    | SEQ of exp * exp
                    | UNIT
       and      pat = PVAR of string
                    | PAPP of string * pat
                    | PINT of int
                    | PLIST of pat list
                    | PTUPLE of pat list
                    | WILD
                    | AS of pat * pat
       and     decl = VB of pat * exp
       and     rule = RULE of pat * exp

         val isalpha = fn c => (c>="A" andalso c<="Z") orelse
	                       (c>="a" andalso c<="z")
         val isnum = fn c => (c>="0" andalso c<="9")
         val idchar = fn "'" => true
                       | "_" => true
                       | i => isalpha i orelse isnum i
         val code_to_ids = fn s =>
	        let fun g(nil,r) = r
                      | g(a as (h::t),r) =
	                   if isalpha h then f(t,[h],r)
                           else g(t,r)
                    and f(nil,accum,r)= implode(rev accum)::r
                      | f(a as (h::t),accum,r) =
	                       if idchar h then f(t,h::accum,r)
                               else g(a,implode (rev accum) :: r)
                in g(explode s,nil)
                end
         val simplifyRule : rule -> rule = fn (RULE(p,e)) =>
            let val used : (string -> bool) =
               let fun f(CODE s) = code_to_ids s
                     | f(EAPP(a,b)) = f a @ f b
                     | f(ETUPLE l) = fold (op @) (map f l) nil
                     | f(EVAR s) = [s]
                     | f(FN(_,e)) = f e
                     | f(LET(dl,e)) =
                          (fold (op @) (map (fn VB(_,e) => f e) dl) nil) @
                           f e
                     | f(SEQ(a,b)) = f a @ f b
                     | f _ = nil
                   val identifiers = f e
               in fn s => exists (fn a=>a=s) identifiers
               end
              val simplifyPat : pat -> pat =
                let fun f a =
                    case a
                    of (PVAR s) => if used s then a else WILD
                     | (PAPP(s,pat)) =>
                         (case f pat
                          of WILD => WILD
                           | pat' => PAPP(s,pat'))
                     | (PLIST l) =>
	                  let val l' = map f l
                          in if exists(fn WILD=>false | _ => true) l'
                                then PLIST l'
                             else WILD
                          end
                     | (PTUPLE l) =>
                          let val l' = map f l
                          in if exists(fn WILD=>false | _ => true) l'
                             then PTUPLE l' 
                             else WILD
                          end
                     | (AS(a,b)) =>
                         let val a'=f a
                             val b'=f b
                         in case(a',b')
                            of (WILD,_) => b'
                             | (_,WILD) => a'
                             | _ => AS(a',b')
                         end
                     | _ => a
               in f
               end
           val simplifyExp : exp -> exp =
               let fun f(EAPP(a,b)) = EAPP(f a,f b)
                     | f(ETUPLE l) = ETUPLE(map f l)
                     | f(FN(p,e)) = FN(simplifyPat p,f e) 
                     | f(LET(dl,e)) = 
                          LET(map (fn VB(p,e) =>
	                          VB(simplifyPat p,f e)) dl,
                              f e)
                     | f(SEQ(a,b)) = SEQ(f a,f b)
                     | f a = a
               in f
               end
       in RULE(simplifyPat p,simplifyExp e)
       end

       fun printRule (say : string -> 'a,sayln:string -> 'a) =    
     let val lp = ["("]
         val rp = [")"]
         val sp = [" "]
         val sm = [";"]
         val cm = [","]
         val cr = ["\n"]
         val unit = ["()"]
         val compress : string list list -> string list =
	    fn l => fold (op @) l nil
          fun printExp c =
	   let fun f (CODE c) = ["(",c,")"]
                 | f (EAPP(EVAR a,UNIT)) = [a," ","()"]
                 | f (EAPP(EVAR a,EINT i)) =  [a," ",makestring i]
                 | f (EAPP(EVAR a,EVAR b)) = [a," ",b]
                 | f (EAPP(EVAR a,b)) = compress[[a],lp,f b,rp]
                 | f (EAPP(a,b)) = compress [lp,f a,rp,lp,f b,rp]
	         | f (EINT i) = [makestring i]
                 | f (ETUPLE (a::r)) = 
	              let fun scan nil = [rp]
                            | scan (h :: t) = cm :: f h :: scan t
                      in compress (lp :: f a :: scan r)
                      end
                 | f (ETUPLE _) = ["<bogus-tuple>"]
                 | f (EVAR s) = [s]
                 | f (FN (p,b)) = compress[["fn "],printPat p,[" => "],f b]
                 | f (LET (nil,body)) = f body
                 | f (LET (dl,body)) =
	              let fun scan nil = [[" in "],f body,[" end"],cr]
                            | scan (h :: t) = printDecl h :: scan t
	              in compress(["let "] :: scan dl)
	              end
                 | f (SEQ (a,b)) = compress [lp,f a,sm,f b,rp]
                 | f (UNIT) = unit
          in f c
          end
         and printDecl (VB (pat,exp)) =
                  compress[["val "],printPat pat,["="],printExp exp,cr]
         and printPat c =
	   let fun f (AS(PVAR a,PVAR b)) = [a," as ",b]
                 | f (AS(a,b)) = compress [lp,f a,[") as ("],f b,rp]
                 | f (PAPP(a,WILD)) = [a," ","_"]
                 | f (PAPP(a,PINT i)) =  [a," ",makestring i]
                 | f (PAPP(a,PVAR b)) = [a," ",b]
                 | f (PAPP(a,b)) = compress [lp,[a],sp,f b,rp]
	         | f (PINT i) = [makestring i]
                 | f (PLIST nil) = ["<bogus-list>"]
                 | f (PLIST l) =
	              let fun scan (h :: nil) = [f h]
                            | scan (h :: t) = f h :: ["::"] :: scan t
                      in compress (scan l)
                      end
                 | f (PTUPLE (a::r)) = 
	              let fun scan nil = [rp]
                            | scan (h :: t) = cm :: f h :: scan t
                      in compress (lp :: f a :: scan r)
                      end
                 | f (PTUPLE nil) = ["<bogus-pattern-tuple>"]
                 | f (PVAR a) = [a]
                 | f WILD = ["_"]
           in f c
           end
           val oursay = fn "\n" => sayln ""
                         | a => say a
         in fn a => 
	      let val RULE(p,e) = simplifyRule a
              in app oursay (printPat p);
	         say " => ";
                 app oursay (printExp e)
              end
         end
end;
