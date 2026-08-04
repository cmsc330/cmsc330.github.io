(* A store's stock: a collection of items, indexed by item name.

   Stocks are persistent (immutable): every operation returns a new
   stock and leaves the old one unchanged. *)

(* The abstract type of a stock. *)
type t

(* The empty stock. *)
val empty : t

(* [is_empty s] is whether [s] contains no items. *)
val is_empty : t -> bool

(* [size s] is the number of distinct items in [s]. *)
val size : t -> int

(* [find name s] is [Some item] if [s] contains an item called
   [name], and [None] otherwise. *)
val find : string -> t -> Item.t option

(* [add item s] adds [item] to [s]. If [s] already contains an item
   with the same name, the quantities are merged and the price of
   [item] (the most recent price) wins. *)
val add : Item.t -> t -> t

(* [remove name n s] removes [n] units of the item called [name].
   If the quantity on hand drops to 0, the item disappears from the
   stock entirely.
   Raises [Not_found] if there is no item called [name].
   Raises [Invalid_argument "Stock.remove"] if [n < 0] or [n] is
   more than the quantity on hand. *)
val remove : string -> int -> t -> t

(* [total_value s] is the sum of the values of all items in [s]. *)
val total_value : t -> float
