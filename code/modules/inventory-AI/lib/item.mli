(* An item held in a store's inventory. *)

(* The abstract type of an item. *)
type t

(* [make name price qty] creates an item.
   Raises [Invalid_argument "Item.make"] if [price < 0.0] or [qty < 0]. *)
val make : string -> float -> int -> t

(* Accessors. *)
val name : t -> string
val price : t -> float
val qty : t -> int

(* [restock n item] is [item] with [n] more units on hand.
   Raises [Invalid_argument "Item.restock"] if [n < 0]. *)
val restock : int -> t -> t

(* [value item] is the total value of the units on hand:
   the unit price times the quantity. *)
val value : t -> float
