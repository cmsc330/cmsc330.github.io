type t = Item.t list

let empty = []
let is_empty s = (s = [])
let size s = List.length s

let find name s = List.find_opt (fun it -> Item.name it = name) s

let drop name s = List.filter (fun it -> Item.name it <> name) s

let add item s =
  match find (Item.name item) s with
  | None -> item :: s
  | Some existing ->
      let merged =
        Item.make (Item.name item) (Item.price item)
          (Item.qty item + Item.qty existing)
      in
      merged :: drop (Item.name item) s

let remove name n s =
  match find name s with
  | None -> raise Not_found
  | Some it ->
      if n < 0 || n > Item.qty it then invalid_arg "Stock.remove"
      else if n = Item.qty it then drop name s
      else Item.make name (Item.price it) (Item.qty it - n) :: drop name s

let total_value s =
  List.fold_left (fun acc it -> acc +. Item.value it) 0.0 s
