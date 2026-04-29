(* open Lambda *)
open Format

let ident = Printf.printf "%s" (*print_string*)
let kwd = Printf.printf "%s" (*print_string*)

let rec print_exp0 = function
  | Var s -> ident s
  | lam ->
      open_hovbox 1;
      kwd "(";
      print_lambda lam;
      kwd ")";
      close_box ()

and print_app = function
  | e ->
      open_hovbox 2;
      print_other_applications e;
      close_box ()

and print_other_applications f =
  match f with
  | App (f, arg) ->
      print_app f;
      Printf.printf " ";
      print_exp0 arg
  | f -> print_exp0 f

and 
 print_lambda = function
  | Lam (s, lam) ->
      open_hovbox 1;
      kwd "λ";
      ident s;
      kwd ".";
      Printf.printf " ";
      print_lambda lam;
      close_box ()
  | e -> print_app e

let rec lambda_exp_2_str e =
  match e with
  | Var x -> "Var " ^ x
  | App (e1, e2) ->
      "App(" ^ lambda_exp_2_str e1 ^ "," ^ lambda_exp_2_str e2 ^ ")"
  | Lam (x, e) -> "Lam(" ^ x ^ "," ^ lambda_exp_2_str e ^ ")"

  let p = print_lambda ;;

  let rec to_int e =
  match e with
  | App (x, y) -> 1 + to_int x + to_int y
  | Lam (_a, b) -> to_int b
  | _ -> 0