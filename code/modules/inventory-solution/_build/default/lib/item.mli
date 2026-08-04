type t
val make : string -> float -> int -> t
val name : t -> string
val price : t -> float
val qty : t -> int
val restock : int -> t -> t
val value : t -> float
