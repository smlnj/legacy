(* fol-pretty.sml  
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * 
 *)

signature DISPFOL =
  sig
  type form
  type term 
  type otype
  val format_otype : (otype * int) -> string list 
  val format_term : (term * int) -> string list 
  val format_form : (form * int) -> string list 
  val pr_form : (form * int) -> unit
  end


functor DispFolFUN (structure Pretty: PRETTY
		    and       Fol: FOL) : DISPFOL =
  struct
  local open Pretty Fol  
  
    fun maxl[m] : int = m
      | maxl(m::n::ns) = if m>n  then  maxl(m::ns)  else  maxl(n::ns);

    fun enclose sexp = blo(1, [str"(", sexp, str")"])

    fun commas [] = []
      | commas (sexp::sexps) = str"," :: brk 1 :: sexp :: commas sexps

    fun list (sexp::sexps) = blo(0, sexp :: commas sexps)

    fun term k (Var a) = str a
      | term k (IntTerm i) = str (makestring i)
      | term k (Fun(Var name,[a,b])) = 
	    let val prec = prec_of name
            in if prec < 0 
               then blo(0, [str name, args [a,b]])
               else let val pt = term (maxl[prec, k])
		        val sexp = blo(0, [pt a, str(" "^name), brk 1, pt b])
	            in  if (prec <= k) then (enclose sexp) else sexp end
	    end
      | term k (Fun (t,ts)) =  blo(0, [term 0 t, args ts])
    and args [] = str""
      | args ts = enclose (list (map (term 0) ts))

    fun ptype k (BaseType s) = str s
      | ptype k (TypeCon(s,l)) = blo(0,[(str s),enclose(list(map (ptype k) l))])

    fun bindinglist [] = [str "."]
      | bindinglist ((x,t)::[]) = [str x,str " : ", ptype 0 t,str "."]
      | bindinglist ((x,t)::bl) =  [str x,str " : ", ptype 0 t,str ","] @ (bindinglist bl)

    (*display formula in context of operator with precedence k *)
    fun form k (Pred ("=",[a,b])) = blo(0, [term 0 a,str " = ",term 0 b])
      | form k (Pred (a,ts)) = blo(0, [str a, args ts])
      | form k (Conn("~", [p])) = blo(0, [str "~", form (prec_of "~") p])
      | form k (Conn(C, [p,q])) =
	    let val pf = form (maxl[prec_of C, k])
		val sexp = blo(0, [pf p, str(" "^C), brk 1, pf q])
	    in  if (prec_of C <= k) then (enclose sexp) else sexp
	    end
      | form k (Quant(qnt,bl,p)) =
	    let val sexp = blo(2, 
		     [str (qnt ^ " ")] @ 
                     (bindinglist bl) @ [brk 1,  form 0 p])
	    in  if  k>0  then  (enclose sexp)  else sexp  end
      | form k _ = str"??UNKNOWN FORMULA??"

    fun formlist [] = str"empty"
      | formlist ps = list (map (form 0) ps)

    fun print_lines [] = print "\n"
      | print_lines ((s : string)::l) = (print s; print "\n"; print_lines l)
  in
    type form = Fol.form
    type term = Fol.term
    type otype = Fol.otype

    fun format_otype (t,margin) = format (ptype 0 t, margin)

    fun format_term (t,margin) = format (term 0 t, margin)

    fun format_form (f,margin) = format (form 0 f, margin)

    fun pr_form (f,margin) = print_lines (format_form(f,margin))

  end
  end





