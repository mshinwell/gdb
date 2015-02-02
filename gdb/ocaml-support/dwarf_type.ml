open Std

(* CR mshinwell: bad function name *)
let decode_dwarf_type dwarf_type =
  (* CR mshinwell: use [Core_kernel] and rewrite with sensible string
     functions *)
  let magic = "__ocaml" in
  if String.length dwarf_type <= String.length magic then
    None, None
  else
    (* CR mshinwell: we should be able to work out what the source file path was
       by reading the compilation unit DIE.  Then we can remove the propagation of it
       via the DWARF types. *)
    let delimiter =
      try Some (String.rindex dwarf_type ' ') with Not_found -> None
    in
    match delimiter with
    | None -> None, None
    | Some delimiter when delimiter <= String.length magic -> None, None
    | Some delimiter -> begin
      let output_path =
        String.sub dwarf_type (String.length magic)
          (delimiter - (String.length magic))
      in
      let symbol_linkage_name =
        String.sub dwarf_type (delimiter + 1)
          ((String.length dwarf_type) - (delimiter + 1))
      in
      if Debug.debug then
        Printf.eprintf "output path '%s' symbol linkage name '%s'\n%!"
          output_path symbol_linkage_name;
      Some output_path, Some symbol_linkage_name
    end
