(* This expression parser uses the OCaml Str module.
   To run, use:

     utop -require str parser_add_mul.ml

*)

(* expr : user-defined variant datatype for arithmetic 
   expressions  
*)

type expr =
  | Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr

(*
  function expr_to_str : expr -> string
  converts arithmetic expression into a string
*)

let rec expr_to_str a =
  match a with
  | Num n -> string_of_int n (* from Pervasives *)
  | Add (a1, a2) -> "(" ^ expr_to_str a1 ^ " + " ^ expr_to_str a2 ^ ")"
  | Sub (a1, a2) -> "(" ^ expr_to_str a1 ^ " - " ^ expr_to_str a2 ^ ")"
  | Mult (a1, a2) -> "(" ^ expr_to_str a1 ^ " * " ^ expr_to_str a2 ^ ")"

(*
  finds value of arithmetic expression.  always returns int
*)
let rec eval e =
  match e with
  | Num n -> n
  | Add (a1, a2) -> eval a1 + eval a2
  | Sub (a1, a2) -> ( match (eval a1, eval a2) with n1, n2 -> n1 - n2)
  | Mult (a1, a2) -> ( match (eval a1, eval a2) with n1, n2 -> n1 * n2)

exception IllegalExpression of string
exception ParseError of string

(* Scanner *)

type token =
  | Tok_Num of string
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_LParen
  | Tok_RParen
  | Tok_END

(*----------------------------------------------------------
  function tokenize : string -> token list
  converts string into a list of tokens
*)
let tokenize str =
  let re_num = Str.regexp "[0-9]+" in
  let re_add = Str.regexp "+" in
  let re_sub = Str.regexp "-" in
  let re_mult = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let re_space = Str.regexp "[ \t\n]+" in

  let rec tok pos s =
    if pos >= String.length s then [ Tok_END ]
    else if Str.string_match re_num s pos then
      let token = Str.matched_string s in
      let len = String.length token in
      Tok_Num token :: tok (pos + len) s
    else if Str.string_match re_space s pos then
      let matched = Str.matched_string s in
      tok (pos + String.length matched) s
    else if Str.string_match re_add s pos then Tok_Add :: tok (pos + 1) s
    else if Str.string_match re_sub s pos then Tok_Sub :: tok (pos + 1) s
    else if Str.string_match re_mult s pos then Tok_Mult :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression "tokenize")
  in
  tok 0 str

(*
  function token_to_name: token -> string
  converts token into its constructor name
*)

let token_to_name t =
  match t with
  | Tok_Num v -> "Tok_Num " ^ v
  | Tok_Add -> "Tok_Add"
  | Tok_Sub -> "Tok_Sub"
  | Tok_Mult -> "Tok_Mult"
  | Tok_LParen -> "Tok_LParen"
  | Tok_RParen -> "Tok_RParen"
  | Tok_END -> "END"

(*
  function token_to_str: token -> string
  converts token into its symbol representation
*)
let token_to_str t =
  match t with
  | Tok_Num v -> v
  | Tok_Add -> "+"
  | Tok_Sub -> "-"
  | Tok_Mult -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(* Parser *)
(*
   function lookahead : token list -> token
   Returns the first token in the token list
*)
let lookahead tokens =
  match tokens with [] -> raise (ParseError "no tokens") | h :: _ -> h

let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (ParseError (token_to_name tok))
  | h :: t when h = tok -> t
  | h :: _ -> raise (ParseError (token_to_name tok))

(*
recursive descent parser

Returns tuple of ast & token list for remainder of string

Arithmetic expression grammar:

   S -> A + S | A - S| A 
   A -> B * A | B
   B -> n | (S)
  
  [Basic grammar with tokens]

    S -> A Tok_Add S | A Tok_Sub S | A
    A -> B Tok_Mult A | B
    B -> Tok_Num | Tok_LParen S Tok_RParen
*)

let rec parse_S tokens =
  let e1, t1 = parse_A tokens in
  match lookahead t1 with
  | Tok_Add ->
      (* S -> A Tok_Add S *)
      let t2 = match_token t1 Tok_Add in
      let e2, t3 = parse_S t2 in
      (Add (e1, e2), t3)
  | Tok_Sub ->
      (* S -> A Tok_Sub S *)
      let t2 = match_token t1 Tok_Sub in
      let e2, t3 = parse_S t2 in
      (Sub (e1, e2), t3)
  | _ -> (e1, t1)
(* S -> A *)

and parse_A tokens =
  let e1, tokens = parse_B tokens in
  match lookahead tokens with
  | Tok_Mult ->
      (* A -> B Tok_Mult A *)
      let t2 = match_token tokens Tok_Mult in
      let e2, t3 = parse_A t2 in
      (Mult (e1, e2), t3)
  | _ -> (e1, tokens)

and parse_B tokens =
  match lookahead tokens with
  (* B -> Tok_Num *)
  | Tok_Num c ->
      let t = match_token tokens (Tok_Num c) in
      (Num (int_of_string c), t)
  (* B -> Tok_LParen E Tok_RParen *)
  | Tok_LParen ->
      let t = match_token tokens Tok_LParen in
      let e2, t2 = parse_S t in
      (e2, match_token t2 Tok_RParen)
  | _ -> raise (ParseError "parse_B 2")

let pp = Printf.printf

(*
  function eval_str : given string, parse string, build AST,
  	evaluate value of AST
*)

let eval_str str =
  let tokens = tokenize str in

  pp "Input token list = [";
  List.iter (fun x -> pp "%s;" (token_to_name x)) tokens;
  pp "]\n";

  let a, t = parse_S tokens in
  if t <> [ Tok_END ] then raise (IllegalExpression "parse_S");
  pp "AST produced = %s\n" (expr_to_str a);
  let v = eval a in
  pp "Value of AST = %d\n" v
;;

Printf.printf "\nExample 1:\n";;
eval_str "1*2*3-4*5*6";;
Printf.printf "\nExample 2:\n";;
eval_str "1+2+3*4*5+6";;
Printf.printf "\nExample 3:\n";;
eval_str "1+(2+3)*4*5+6";;
Printf.printf "\nExample 4:\n";;
eval_str "100 *      (10 + 20)"
(* eval_str "(2^5)*2";;  error *)
(* eval_str "1++12" error *)
