include Hashtbl.Make (struct
  type t = Path.t
  open Path

  (* Don't worry about the stamps. *)
  let rec equal x y =
    match x, y with
    | Pident x, Pident y -> Ident.name x = Ident.name y
    | Pdot (p1, s1, _), Pdot (p2, s2, _) ->
      equal p1 p2 && s1 = s2
    | _, _ -> false

  let hash x =
    let rec aux = function
      | Pident i -> Ident.name i
      | Pdot (p, s, _) -> s ^ aux p
      | Papply (p1, p2) -> aux p1 ^ aux p2
    in
    Hashtbl.hash (aux x)
end)

let table : Types.type_declaration t = create 20
