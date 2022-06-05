
signature TILE_SERVER =
  sig
    structure EXB : EXENE_BASE

    type tile_server

    exception BadName

    val mkTileServer : (EXB.screen * (Quark.quark -> EXB.image)) -> tile_server

    val getTile : tile_server -> string -> EXB.tile
    
  end

structure TileServer : TILE_SERVER =
  struct

    structure EXB = EXeneBase

    exception BadName

    datatype req = GetTile of string
    type reply = EXB.tile option

    datatype tile_server = TS of {
        req : req CML.chan,
        reply : reply CML.chan
      }

    structure StringTbl = HashTableFn (struct
          type hash_key = Quark.quark
          val sameKey = Quark.same
          val hashVal = Quark.hash
        end)

    type tile_tbl = EXB.tile StringTbl.hash_table

    fun mkTileServer (scr,imageOf) = let
          exception NotFound
          val tileTbl : tile_tbl = StringTbl.mkTable(32, NotFound)
          val tileIns = StringTbl.insert tileTbl
          val tileFind = StringTbl.find tileTbl
 
          fun wrapIn (ins,f) = 
               (f ins before TextIO.closeIn ins) 
                 handle e => (TextIO.closeIn ins; raise e)

          fun mkFileTile (n,q) = let
                val fileName = substring(n,1,size n - 1)
                val ins = TextIO.openIn fileName
                val {image, ...} = wrapIn (ins, BitmapIO.readBitmap)
                val t = EXB.createTileFromImage scr image
                in
		  tileIns(q, t); SOME t
		end

          fun mkImageTile q = let
                val t = EXB.createTileFromImage scr (imageOf q)
                in tileIns(q,t); SOME t end

          fun mkTile (arg as (n, q)) = 
                (if String.sub(n, 0) = #"@" then mkFileTile arg else mkImageTile q)
                  handle _ => NONE
          
          fun handleReq (GetTile n) = let
                val q = Quark.quark n
                in case tileFind q of NONE => mkTile (n,q) | s => s end

          val req = CML.channel () and reply = CML.channel ()
          fun loop () = (CML.send(reply,handleReq(CML.recv req)); loop ())
          in
            XDebug.xspawn("TileServer", loop);
            TS{req = req, reply = reply}
          end

    fun getTile (TS{req,reply}) name = (
          CML.send(req, GetTile name);
          case CML.recv reply of SOME s => s | _ => raise BadName
        )

  end

