(* A store's stock: a collection of items, indexed by item name.

   Representation: a list of items containing at most one item per
   name. This is an invariant your functions must maintain! You may
   change the representation -- the test suite only uses the
   functions below.

   Stocks are persistent (immutable): every operation returns a new
   stock and leaves the old one unchanged. *)

type t = Item.t list

(* The empty stock. *)
let empty : t = []

(* [is_empty s] is whether [s] contains no items. *)
let is_empty (_s : t) : bool = match _s with [] -> true | _ -> false

(* [size s] is the number of distinct items in [s]. *)
let size (_s : t) : int = List.length _s

(* [find name s] is [Some item] if [s] contains an item called
   [name], and [None] otherwise. *)
let rec find (_name : string) (_s : t) : Item.t option =
  match _s with
  | [] -> None
  | h :: t -> if _name = Item.name h then Some h else find _name t

(* [add item s] adds [item] to [s]. If [s] already contains an item
   with the same name, the quantities are merged and the price of
   [item] (the most recent price) wins. *)
let rec add (_item : Item.t) (_s : t) : t =
  match _s with
  | [] -> [ _item ]
  | hd :: tl ->
      if Item.name hd = Item.name _item then
        Item.make (Item.name hd) (Item.price _item)
          (Item.qty hd + Item.qty _item)
        :: tl
      else hd :: add _item tl

(* [remove name n s] removes [n] units of the item called [name].
   If the quantity on hand drops to 0, the item disappears from the
   stock entirely.
   Raises [Not_found] if there is no item called [name].
   Raises [Invalid_argument "Stock.remove"] if [n < 0] or [n] is
   more than the quantity on hand. *)
let rec remove (_name : string) (_n : int) (_s : t) : t =
  if _n < 0 then raise (Invalid_argument "Stock.remove")
  else
    match _s with
    | [] -> raise Not_found
    | h :: t ->
        if Item.name h = _name then
          if Item.qty h > _n then
            Item.make (Item.name h) (Item.price h) (Item.qty h - _n) :: t
          else if Item.qty h = _n then t
          else raise (Invalid_argument "Stock.remove")
        else h :: remove _name _n t

(* [total_value s] is the sum of the values of all items in [s]. *)
let total_value (_s : t) : float =
  List.fold_left
    (fun (a : float) (c : Item.t) ->
      (float_of_int (Item.qty c) *. Item.price c) +. a)
    0.0 _s
