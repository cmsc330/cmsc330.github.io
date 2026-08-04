type t
val empty : t
val is_empty : t -> bool
val find : string -> t -> Item.t option
val size : t -> int
val add : Item.t -> t -> t
val remove : string -> int -> t -> t
val total_value : t -> float
