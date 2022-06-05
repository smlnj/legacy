(* cps-aliasing.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor CPSAliasingF (structure CPSRegions : CPS_REGION
                      val exnptr   : int
                      val allocptr : int
                     ) =
struct

   structure Region = CPSRegions
   structure R = Region

   type reg = int
   type m_def_use =
       {m_def:reg list, m_use:reg list, r_def:reg list, r_use:reg list}

   val rw        = [65]
   val ro        = [66]
   val stack     = [67]
   val real      = [68]
   val storelist = [69]

   fun read_region R.RW_MEM   = {m_def=[],m_use=[],r_def=[],r_use=rw}
     | read_region R.RO_MEM   = {m_def=[],m_use=[],r_def=[],r_use=ro}
     | read_region R.STACK    = {m_def=[],m_use=[],r_def=[],r_use=stack}
     | read_region R.REAL     = {m_def=[],m_use=[],r_def=[],r_use=real}
     | read_region (R.RVAR r) = {m_def=[],m_use=[r],r_def=[],r_use=[]}
     | read_region R.STORELIST = {m_def=[],m_use=[],r_def=[],r_use=storelist}
     | read_region (R.MUTABLE(def,use)) = read_region use
     | read_region (R.RECORD[(d,u,_)]) =
          let val {m_def=a,m_use=b,r_def=c,r_use=d}=read_region d
              val {m_def=e,m_use=f,r_def=g,r_use=h}=write_region u
          in
             {m_def=a@e,m_use=b@f,r_def=c@g,r_use=d@h}
          end
     | read_region (R.REGIONS(x,y)) =
          let val {m_def=a,m_use=b,r_def=c,r_use=d}=read_region x
              val {m_def=e,m_use=f,r_def=g,r_use=h}=read_region y
          in
             {m_def=a@e,m_use=b@f,r_def=c@g,r_use=d@h}
          end

   and write_region R.RW_MEM    = {m_def=[],m_use=[],r_def=rw,r_use=[]}
     | write_region R.RO_MEM    = {m_def=[],m_use=[],r_def=ro,r_use=[]}
     | write_region R.STACK     = {m_def=[],m_use=[],r_def=stack,r_use=[]}
     | write_region R.REAL      = {m_def=[],m_use=[],r_def=real,r_use=[]}
     | write_region (R.RVAR r)  = {m_def=[r],m_use=[],r_def=[],r_use=[]}
     | write_region R.STORELIST = {m_def=[],m_use=[],r_def=storelist,r_use=[]}
     | write_region (R.MUTABLE(def,use)) = write_region def
     | write_region (R.RECORD[(d,u,_)]) =
          let val {m_def=a,m_use=b,r_def=c,r_use=d}=read_region d
              val {m_def=e,m_use=f,r_def=g,r_use=h}=write_region u
          in
             {m_def=a@e,m_use=b@f,r_def=c@g,r_use=d@h}
          end
     | write_region (R.REGIONS(x,y)) =
          let val {m_def=a,m_use=b,r_def=c,r_use=d}=write_region x
              val {m_def=e,m_use=f,r_def=g,r_use=h}=write_region y
          in
             {m_def=a@e,m_use=b@f,r_def=c@g,r_use=d@h}
          end

   fun is_safe_read  R.RW_MEM           = false
     | is_safe_read  R.RO_MEM           = false
     | is_safe_read  R.STACK            = true
     | is_safe_read  R.REAL             = true
     | is_safe_read  (R.RVAR r)         = false
     | is_safe_read  R.STORELIST        = false
     | is_safe_read  (R.MUTABLE _)      = false
     | is_safe_read  (R.RECORD [(d,u,_)]) = is_safe_read d
     | is_safe_read  (R.REGIONS(a,b))   = is_safe_read a andalso is_safe_read b
     | is_safe_read  _                  = false

   fun isTrapSafeRead R.RW_MEM           = false
     | isTrapSafeRead R.RO_MEM           = true
     | isTrapSafeRead R.STACK            = false
     | isTrapSafeRead R.REAL             = true
     | isTrapSafeRead (R.RVAR r)         = true
     | isTrapSafeRead R.STORELIST        = false
     | isTrapSafeRead (R.MUTABLE _)      = false
     | isTrapSafeRead (R.RECORD [(d,u,_)]) = isTrapSafeRead d
     | isTrapSafeRead (R.REGIONS(a,b))   = isTrapSafeRead a
                                   andalso isTrapSafeRead b
     | isTrapSafeRead _                  = false

   fun is_safe_write R.RW_MEM           = false
     | is_safe_write R.RO_MEM           = true
     | is_safe_write R.STACK            = false
     | is_safe_write R.REAL             = true
     | is_safe_write (R.RVAR r)         = true
     | is_safe_write R.STORELIST        = false
     | is_safe_write (R.MUTABLE _)      = false
     | is_safe_write (R.RECORD [(d,u,_)]) = is_safe_write d
     | is_safe_write (R.REGIONS(a,b))   = is_safe_write a andalso is_safe_write b
     | is_safe_write _                  = false

   fun isTrapSafeWrite mem = is_safe_write mem

   val trapBarrier = { def   = [allocptr],
                       use   = [allocptr],
                       m_def = [],
                       m_use = [],
                       r_use = [],
                       r_def = []
                     }

end

