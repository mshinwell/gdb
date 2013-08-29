let _ = prerr_endline "Hello from ml world!"

let rec val_print ~depth v out =
  if depth > 2 then Gdb.print out ".." else 
  if Gdb.Obj.is_int v
  then Gdb.printf out "%d" (Gdb.Obj.int v)
  else if Gdb.Obj.is_block v then
  begin match Gdb.Obj.tag v with
    | tag when tag < Gdb.Obj.closure_tag  ->
      begin
        if tag > 0 then Gdb.printf out "tag %d:" tag;
        Gdb.print out "(";
        for field = 0 to Gdb.Obj.size v - 1 do
          if field > 0 then Gdb.print out ", ";
          try 
            let v' = Gdb.Obj.field v field in
            val_print ~depth:(succ depth) v' out
          with Gdb.Read_error _ ->
            Gdb.printf out "<field %d read failed>" field
        done;
        Gdb.print out ")"
      end
    | tag when tag = Gdb.Obj.string_tag ->
      Gdb.printf out "%S" (Gdb.Obj.string v)
    | tag -> Gdb.printf out "<tag %d, TODO>" tag
  end
  else Gdb.printf out "<unaligned object>"

let val_print addr stream = val_print ~depth:0 addr stream
let () = Callback.register "gdb_ocaml_support_val_print" val_print
