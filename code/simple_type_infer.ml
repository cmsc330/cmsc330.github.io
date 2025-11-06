type exptype =
  | TInt
  | TBool
  | TArrow of exptype * exptype
  | Guess_t of int * exptype option ref

type op = Add | Sub | Mult | Equal | NotEqual
type var = string

type expr =
  | Int of int
  | Binop of op * expr * expr
  | Bool of bool
  | If of expr * expr * expr
  | ID of var
  | Fun of var * expr
  | App of expr * expr
  | Let of var * expr * expr

let rec string_of_exptype t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) ->
      "(" ^ string_of_exptype t1 ^ "->" ^ string_of_exptype t2 ^ ")"
  | Guess_t (i, { contents = None }) -> Printf.sprintf "?%d" i
  | Guess_t (_, { contents = Some t }) -> string_of_exptype t

type 'a env = (var * 'a) list

exception TypeError of string

let error (s : string) = raise (TypeError s)
let extend (env : 'a env) (x : var) (v : 'a) : 'a env = (x, v) :: env

let rec lookup (env : 'a env) (x : var) : 'a =
  match env with
  | [] -> error ("unbound variable" ^ x)
  | (y, v) :: rest -> if x = y then v else lookup rest x

let fresh_guess =
  let guess_counter = ref 0 in
  fun () : exptype ->
    let c = !guess_counter in
    guess_counter := c + 1;
    Guess_t (c, ref None)

(* path compression -- compress a path of constrained guesses, so that 
     the guess refers to the most constrained type. *)
let rec compress (t : exptype) : exptype =
  match t with
  | Guess_t (_, ({ contents = Some t' } as r)) ->
      let t'' = compress t' in
      r := Some t'';
      t''
  | Guess_t (_, { contents = None }) | TInt | TBool | TArrow (_, _) -> t

(* occurs check -- determine if a guess is in a type *)
let rec occurs (r : exptype option ref) (t : exptype) : bool =
  match t with
  | Guess_t (_, r') -> r == r'
  | TArrow (t1, t2) -> occurs r t1 || occurs r t2
  | TInt | TBool -> false

let rec unify (t1 : exptype) (t2 : exptype) : unit =
  if t1 == t2 then ()
  else
    match (compress t1, compress t2) with
    | Guess_t (_, ({ contents = None } as r)), t
    | t, Guess_t (_, ({ contents = None } as r)) ->
        if occurs r t then
          error
            (Printf.sprintf "occurs check failed:  %s <> %s"
               (string_of_exptype t1) (string_of_exptype t2))
        else r := Some t;
        print_string "\nType:";
        print_string
          (match !r with Some x -> string_of_exptype x | None -> "None")
    | TArrow (t1a, t1b), TArrow (t2a, t2b) ->
        unify t1a t2a;
        unify t1b t2b
    | TBool, TBool -> ()
    | TInt, TInt -> ()
    | t1, t2 ->
        error
          (Printf.sprintf "unify failure: %s <> %s" (string_of_exptype t1)
             (string_of_exptype t2))

let rec infer (e : expr) (env : exptype env) : exptype =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | ID x -> lookup env x
  | Binop (Add, e1, e2) | Binop (Sub, e1, e2) | Binop (Mult, e1, e2) ->
      unify (infer e1 env) TInt;
      unify (infer e2 env) TInt;
      TInt
  | Binop (Equal, e1, e2) | Binop (NotEqual, e1, e2) ->
      unify (infer e1 env) (infer e2 env);
      TBool
  | If (e1, e2, e3) ->
      unify (infer e1 env) TBool;
      let t2 = infer e2 env in
      let t3 = infer e3 env in
      unify t2 t3;
      t2
  | Let (x, e1, e2) -> infer e2 (extend env x (infer e1 env))
  | Fun (x1, e1) ->
      let g = fresh_guess () in
      TArrow (g, infer e1 (extend env x1 g))
  | App (e1, e2) ->
      let t1 = infer e1 env in
      let t2 = infer e2 env in
      let g = fresh_guess () in
      unify t1 (TArrow (t2, g));
      g

(* 3 + 4*2 *)
let e1 = Binop (Add, Int 3, Binop (Mult, Int 4, Int 2))
let e2 = Binop (Equal, Int 3, Binop (Mult, Int 4, Int 2))
let e3 = If (Bool true, Int 3, Int 4)
let e4 = Let ("x", Int 1, Binop (Add, Int 10, ID "x"))
let e5 = Let ("f", Fun ("x", Binop (Add, Int 3, ID "x")), App (ID "f", Int 4))
let e6 = Fun ("x", Fun ("y", If (ID "x", ID "y", Bool false)))
let e7 = Fun ("x", Fun ("y", If (ID "x", Bool true, ID "y")))
let e8 = Fun ("x", ID "x")

(* let polymorphism 

let f = fun x->x in f ( (f 1) = 5)

*)
let e9 =
  Let
    ( "f",
      Fun ("x", ID "x"),
      App (ID "f", Binop (Equal, Int 5, App (ID "f", Int 1))) )

let e10 = Fun ("x", Binop (Add, Int 2, ID "x"))
let e11 = Fun ("f", App (ID "f", Int 2))
let e12 = Fun ("x", Binop (Add, ID "x", ID "x"))
let e13 = App (e11, e12);;

string_of_exptype (compress (infer e13 []))
