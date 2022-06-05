(* image-server.sml
 *
 * COPYRIGHT (c) 1994 by AT&T Bell Laboratories.  See COPYRIGHT file for details
 *
 * This provides a name to eXene image server.  
 *)

signature IMAGE_SERVER =
  sig
    structure EXB : EXENE_BASE

    exception BadName

    type image_server

    val mkImageServer : (Quark.quark * EXB.image) list -> image_server
    val getImage : image_server -> Quark.quark -> EXB.image
    val addImage : image_server -> Quark.quark * EXB.image -> unit
    
  end

structure ImageServer : IMAGE_SERVER =
  struct

    structure EXB = EXeneBase

    exception BadName

    datatype req = 
        GetImage of Quark.quark
      | AddImage of (Quark.quark * EXB.image)

    datatype reply = 
        Image of EXB.image
      | Okay
      | Error

    datatype image_server = IS of {req : req CML.chan, rep : reply CML.chan}

    structure StringTbl = HashTableFn (struct
          type hash_key = Quark.quark
          val sameKey = Quark.same
          val hashVal = Quark.hash
        end)

    type image_tbl = EXB.image StringTbl.hash_table

    fun mkImageServer inits = let
          exception NotFound
          val imageTbl : image_tbl = StringTbl.mkTable(32, NotFound)
          val imageIns = StringTbl.insert imageTbl
          val imageFind = StringTbl.find imageTbl
          val reqCh = CML.channel () and replyCh = CML.channel ()
 
          fun handleReq (GetImage n) =
                  (case imageFind n
                    of NONE => Error 
                     | SOME i => Image i)
            | handleReq (AddImage (q,i)) =
                case imageFind q 
                 of NONE => (imageIns (q,i); Okay)
                  | SOME _ => Error

          fun loop () = (CML.send(replyCh,handleReq(CML.recv reqCh)); loop ())
          in
            app imageIns inits;
            XDebug.xspawn("ImageServer", loop);
            IS{req=reqCh,rep=replyCh}
          end

    fun getImage (IS{req,rep}) name = (
          CML.send(req, GetImage name);
          case CML.recv rep of Image i => i | _ => raise BadName
        )
    fun addImage (IS{req,rep}) arg = (
          CML.send(req, AddImage arg);
          case CML.recv rep of Okay => () | _ => raise BadName
        )

  end

