(* An item held in a store's inventory.

   Representation: a record with the item's name, unit price, and
   quantity on hand. You may change this representation -- the test
   suite only uses the functions below. *)

type t = { name : string; price : float; qty : int }

(* [make name price qty] creates an item.
   Raises [Invalid_argument "Item.make"] if [price < 0.0] or [qty < 0]. *)
let make (_name : string) (_price : float) (_qty : int) : t =
  if _price < 0.0 || _qty < 0  then raise (Invalid_argument "Item.make")
  else { name = _name; price = _price; qty = _qty }

(* Accessors. *)
let name (item : t) : string = item.name
let price (item : t) : float = item.price
let qty (item : t) : int = item.qty

(* [restock n item] is [item] with [n] more units on hand.
   Raises [Invalid_argument "Item.restock"] if [n < 0]. *)
let restock (_n : int) (_item : t) : t =
  if _n < 0 then raise (Invalid_argument "Item.restock")
  else { name = _item.name; price = _item.price; qty = _item.qty + _n }

(* [value item] is the total value of the units on hand:
   the unit price times the quantity. *)
let value (_item : t) : float = float_of_int _item.qty *. _item.price
