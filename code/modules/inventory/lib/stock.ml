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
let is_empty (_s : t) : bool =
  failwith "TODO"

(* [size s] is the number of distinct items in [s]. *)
let size (_s : t) : int =
  failwith "TODO"

(* [find name s] is [Some item] if [s] contains an item called
   [name], and [None] otherwise. *)
let find (_name : string) (_s : t) : Item.t option =
  failwith "TODO"

(* [add item s] adds [item] to [s]. If [s] already contains an item
   with the same name, the quantities are merged and the price of
   [item] (the most recent price) wins. *)
let add (_item : Item.t) (_s : t) : t =
  failwith "TODO"

(* [remove name n s] removes [n] units of the item called [name].
   If the quantity on hand drops to 0, the item disappears from the
   stock entirely.
   Raises [Not_found] if there is no item called [name].
   Raises [Invalid_argument "Stock.remove"] if [n < 0] or [n] is
   more than the quantity on hand. *)
let remove (_name : string) (_n : int) (_s : t) : t =
  failwith "TODO"

(* [total_value s] is the sum of the values of all items in [s]. *)
let total_value (_s : t) : float =
  failwith "TODO"
