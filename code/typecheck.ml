type op =
  | Add
  | Sub
  | Mult
  | Div
  | Concat
  | Greater
  | Less
  | GreaterEqual
  | LessEqual
  | Equal
  | NotEqual
  | Or
  | And

type var = string
type label = Lab of var

type exptype =
  | TInt
  | TBool
  | TArrow of exptype * exptype
  | TRec of (label * exptype) list
  | TBottom

type expr =
  | Int of int
  | Bool of bool
  | ID of var
  | Fun of var * exptype * expr
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of var * expr * expr
  | Record of (label * expr) list
  | Select of label * expr

exception TypeError

let gamma = Hashtbl.create 1367
let lookup = Hashtbl.find

let rec typecheck e =
  match e with
  (* lookup returns the first mapping of x in gamma *)
  | ID x -> lookup gamma x
  | Int _ -> TInt
  | Bool _ -> TBool
  | Binop (Add, e1, e2) | Binop (Sub, e1, e2) | Binop (Mult, e1, e2) ->
      if typecheck e1 = TInt && typecheck e2 = TInt then TInt
      else raise TypeError
  | Binop (Equal, e1, e2) ->
      if typecheck e1 = typecheck e2 then TBool else raise TypeError
  | Binop (And, e1, e2) | Binop (Or, e1, e2) ->
      if typecheck e1 = TBool && typecheck e2 = TBool then TBool
      else raise TypeError
  | Not e1 -> if typecheck e1 = TBool then TBool else raise TypeError
  | If (cond, e1, e2) ->
      if typecheck cond = TBool then
        if typecheck e1 = typecheck e2 then typecheck e1 else raise TypeError
      else raise TypeError
  | Fun (x, t, e1) ->
      let _ = Hashtbl.add gamma x t in
      let t' = typecheck e1 in
      TArrow (t, t')
  | App (f, arg) -> (
      let f_type = typecheck f in
      let arg_type = typecheck arg in
      match f_type with
      | TArrow (t0, t1) -> if arg_type = t0 then t1 else raise TypeError
      | _ -> raise TypeError)
  | Let (x, e1, e2) ->
      let t = typecheck e1 in
      let _ = Hashtbl.add gamma x t in
      typecheck e2
  | Record lst -> TRec (List.map (fun (l, e) -> (l, typecheck e)) lst)
  | Select (lbl, e) -> (
      match lbl with
      | Lab _x -> (
          match typecheck e with
          | TRec lst -> List.assoc lbl lst
          | _ -> raise TypeError))
  | _ -> raise TypeError
