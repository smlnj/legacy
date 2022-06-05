(* splay-dict.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Functor implementing dictionaries using splay trees.
 *
 *)

functor SplayDict (K : ORD_KEY) : DICT =
  struct
    structure Key = K
    open LibBase Key SplayTree

    datatype 'a dict = 
      DICT of {
        root : (ord_key * 'a) splay ref,
        nobj : int
      }

    exception NotFound
    fun cmpf k = fn (k',_) => cmpKey(k',k)

    fun mkDict () = DICT{root = ref SplayNil, nobj = 0}
    
	(* Insert an item.  
	 *)
    fun insert (DICT{root,nobj},key,v) =
          case splay (cmpf key, !root) of
            (_,SplayNil) => 
              DICT{nobj=1,root=ref(SplayObj{value=(key,v),left=SplayNil,right=SplayNil})}
          | (Equal,SplayObj{value,left,right}) => 
              DICT{nobj=nobj,root=ref(SplayObj{value=(key,v),left=left,right=right})}
          | (Less,SplayObj{value,left,right}) => 
              DICT{
                nobj=nobj+1,
                root=ref(SplayObj{value=(key,v),left=SplayObj{value=value,left=left,right=SplayNil},right=right})
              }
          | (Greater,SplayObj{value,left,right}) => 
              DICT{
                nobj=nobj+1,
                root=ref(SplayObj{
                  value=(key,v),
                  left=left,
                  right=SplayObj{value=value,left=SplayNil,right=right}
                })
              }

	(* Find an item, raising NotFound if not found
         *)
    fun find (d as DICT{root,nobj},key) =
          case splay (cmpf key, !root) of
            (_,SplayNil) => raise NotFound
          | (Equal,r as SplayObj{value,...}) => (root := r; #2 value)
          | (_,r) => (root := r; raise NotFound)

	(* Look for an item, return NONE if the item doesn't exist *)
    fun peek arg = (SOME(find arg)) handle NotFound => NONE

	(* Remove an item.
         * Raise NotFound if not found
	 *)
    fun remove (DICT{root,nobj}, key) = (case (splay (cmpf key, !root))
	   of (_,SplayNil) => raise NotFound
	    | (Equal,SplayObj{value,left,right}) => 
		(DICT{root=ref(join(left,right)),nobj=nobj-1}, #2 value)
	    | (_,r) => (root := r; raise NotFound)
	  (* end case *))

	(* Return the number of items in the table *)
    fun numItems (DICT{nobj,...}) = nobj

	(* Return a list of the items (and their keys) in the dictionary *)
    fun listItems (DICT{root,...}) =
        let fun apply (SplayNil,l) = l
              | apply (SplayObj{value,left,right},l) =
                  apply(left, value::(apply (right,l)))
        in
          apply (!root,[])
        end

	(* Apply a function to the entries of the dictionary *)
    fun app af (DICT{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) = 
                    (apply left; af value; apply right)
        in
          apply (!root)
        end

    fun revapp af (DICT{root,...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value,left,right}) = 
                    (apply right; af value; apply left)
        in
          apply (!root)
        end

	(* Fold function *)
    fun fold (abf : ord_key * 'a * 'b -> 'b)  (DICT{root,...}) b =
          let fun apply (SplayNil : (ord_key * 'a) splay, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(left,abf(#1 value,#2 value,apply(right,b)))
        in
          apply (!root,b)
        end

    fun revfold (abf : ord_key * 'a * 'b -> 'b)  (DICT{root,...}) b =
          let fun apply (SplayNil : (ord_key * 'a) splay, b) = b
                | apply (SplayObj{value,left,right},b) =
                    apply(right,abf(#1 value,#2 value,apply(left,b)))
        in
          apply (!root,b)
        end

	(* Map a table to a new table that has the same keys*)
    fun map (af : ord_key * 'a -> '2b) (DICT{root,nobj}) =
          let fun ap (SplayNil : (ord_key * 'a) splay) = SplayNil
                | ap (SplayObj{value,left,right}) = let
                    val left' = ap left
                    val value' = (#1 value, af value)
                    in
                      SplayObj{value = value', left = left', right = ap right}
                    end
        in
          DICT{root = ref(ap (!root)), nobj = nobj}
        end

    fun transform (af : 'a -> '2b) (DICT{root,nobj}) =
          let fun ap (SplayNil : (ord_key * 'a) splay) = SplayNil
                | ap (SplayObj{value,left,right}) = let
                    val left' = ap left
                    val value' = (#1 value, af (#2 value))
                    in
                      SplayObj{value = value', left = left', right = ap right}
                    end
        in
          DICT{root = ref(ap (!root)), nobj = nobj}
        end

  end (* SplayDict *)
