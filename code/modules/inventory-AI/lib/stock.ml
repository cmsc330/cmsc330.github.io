(* A store's stock: a collection of items, indexed by item name.

   Representation: an association list mapping each name to its item,
   containing at most one entry per name. This is an invariant your
   functions must maintain! You may change the representation -- the
   test suite only uses the functions below.

   Stocks are persistent (immutable): every operation returns a new
   stock and leaves the old one unchanged. *)

type t = (string * Item.t) list

(* The empty stock. *)
let empty : t = []

(* [is_empty s] is whether [s] contains no items. *)
let is_empty (s : t) : bool = s = []

(* [size s] is the number of distinct items in [s]. *)
let size (s : t) : int = List.length s

(* [find name s] is [Some item] if [s] contains an item called
   [name], and [None] otherwise. *)
let find (name : string) (s : t) : Item.t option = List.assoc_opt name s

(* [add item s] adds [item] to [s]. If [s] already contains an item
   with the same name, the quantities are merged and the price of
   [item] (the most recent price) wins. *)
let add (item : Item.t) (s : t) : t =
  let name = Item.name item in
  match find name s with
  | None -> (name, item) :: s
  | Some existing ->
    let merged =
      Item.make name (Item.price item) (Item.qty existing + Item.qty item)
    in
    (name, merged) :: List.remove_assoc name s

(* [remove name n s] removes [n] units of the item called [name].
   If the quantity on hand drops to 0, the item disappears from the
   stock entirely.
   Raises [Not_found] if there is no item called [name].
   Raises [Invalid_argument "Stock.remove"] if [n < 0] or [n] is
   more than the quantity on hand. *)
let remove (name : string) (n : int) (s : t) : t =
  match find name s with
  | None -> raise Not_found
  | Some item ->
    if n < 0 || n > Item.qty item then invalid_arg "Stock.remove"
    else
      let remaining = List.remove_assoc name s in
      if Item.qty item - n = 0 then remaining
      else (name, Item.make name (Item.price item) (Item.qty item - n)) :: remaining

(* [total_value s] is the sum of the values of all items in [s]. *)
let total_value (s : t) : float =
  List.fold_left (fun acc (_, it) -> acc +. Item.value it) 0.0 s
