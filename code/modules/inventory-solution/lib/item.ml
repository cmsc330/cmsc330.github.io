type t = { name : string; price : float; qty : int }

let make name price qty =
  if price < 0.0 || qty < 0 then invalid_arg "Item.make"
  else { name; price; qty }

let name item = item.name
let price item = item.price
let qty item = item.qty

let restock n item =
  if n < 0 then invalid_arg "Item.restock"
  else { item with qty = item.qty + n }

let value item = item.price *. float_of_int item.qty
