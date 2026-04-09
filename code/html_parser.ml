(* Simple HTML parser using recursive descent.
   To run:
     utop -require str html_parser.ml

     <div><p>Hello</p>World</div>

   Grammar:
     document ::= node document | ε
     node     ::= text | element
     element  ::= '<' ident '>' document '</' ident '>'
*)

(* ---------------------------------------------------------
   AST
*)
type node =
  | Text    of string
  | Element of string * node list

(*
  function node_to_str : node -> string
  converts HTML AST node into an indented string
*)
let rec node_to_str indent nd =
  match nd with
  | Text s ->
    indent ^ "Text(" ^ s ^ ")"
  | Element (tag, children) ->
    let child_strs = List.map (node_to_str (indent ^ "  ")) children in
    indent ^ "Element(" ^ tag ^ ")\n" ^ String.concat "\n" child_strs

(* ---------------------------------------------------------
   Scanner
*)
type token =
  | Tok_LT
  | Tok_GT
  | Tok_LT_Slash
  | Tok_Ident of string
  | Tok_Text  of string
  | Tok_END

exception IllegalExpression of string
exception ParseError of string

(*
  function is_letter : char -> bool
  returns true if character is an ASCII letter
*)
let is_letter c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

(*
  function tokenize : string -> token list
  converts input string into a list of tokens
*)
let tokenize input =
  let len = String.length input in

  let rec read_ident i =
    if i < len && is_letter input.[i] then
      let (id, j) = read_ident (i + 1) in
      (String.make 1 input.[i] ^ id, j)
    else ("", i)
  in

  let rec read_text i =
    if i < len && input.[i] <> '<' then
      let (txt, j) = read_text (i + 1) in
      (String.make 1 input.[i] ^ txt, j)
    else ("", i)
  in

  let rec tok i in_tag =
    if i >= len then [ Tok_END ]
    else
      match input.[i] with
      | '<' ->
        if i + 1 < len && input.[i + 1] = '/' then
          Tok_LT_Slash :: tok (i + 2) true
        else
          Tok_LT :: tok (i + 1) true
      | '>' ->
        Tok_GT :: tok (i + 1) false
      | c when is_letter c && in_tag ->
        let (id, j) = read_ident i in
        Tok_Ident id :: tok j in_tag
      | _ ->
        if in_tag then tok (i + 1) in_tag
        else
          let (txt, j) = read_text i in
          let txt = String.trim txt in
          if txt = "" then tok j false
          else Tok_Text txt :: tok j false
  in
  tok 0 false

(*
  function token_to_name : token -> string
  converts token into its constructor name
*)
let token_to_name t =
  match t with
  | Tok_LT       -> "Tok_LT"
  | Tok_GT       -> "Tok_GT"
  | Tok_LT_Slash -> "Tok_LT_Slash"
  | Tok_Ident s  -> "Tok_Ident \"" ^ s ^ "\""
  | Tok_Text  s  -> "Tok_Text \""  ^ s ^ "\""
  | Tok_END      -> "Tok_END"

(* ---------------------------------------------------------
   Parser helpers
*)

(*
  function lookahead : token list -> token
  returns the first token without consuming it
*)
let lookahead tokens =
  match tokens with
  | []     -> Tok_END
  | h :: _ -> h

(*
  function match_token : token list -> token -> token list
  consumes the expected token and returns the rest
*)
let match_token toks tok =
  match toks with
  | [] -> raise (ParseError ("expected " ^ token_to_name tok))
  | h :: t when h = tok -> t
  | h :: _ ->
    raise (ParseError ("expected " ^ token_to_name tok ^ ", got " ^ token_to_name h))

(* ---------------------------------------------------------
   Recursive descent parser

   document ::= node document | ε
   node     ::= text | element
   element  ::= '<' ident '>' document '</' ident '>'
*)

let rec parse_document tokens =
  (* document ::= node document | ε *)
  match lookahead tokens with
  | Tok_END | Tok_LT_Slash ->
    (* document ::= ε *)
    ([], tokens)
  | _ ->
    (* document ::= node document *)
    let node, t1  = parse_node tokens in
    let rest, t2  = parse_document t1 in
    (node :: rest, t2)

and parse_node tokens =
  (* node ::= text | element *)
  match lookahead tokens with
  | Tok_Text s ->
    (* node ::= text *)
    let t1 = match_token tokens (Tok_Text s) in
    (Text s, t1)
  | Tok_LT ->
    (* node ::= element *)
    parse_element tokens
  | t ->
    raise (ParseError ("parse_node: expected text or element, got " ^ token_to_name t))

and parse_element tokens =
  (* element ::= '<' ident '>' document '</' ident '>' *)
  let t1 = match_token tokens Tok_LT in
  let tag =
    match lookahead t1 with
    | Tok_Ident name -> name
    | t -> raise (ParseError ("parse_element: expected tag name, got " ^ token_to_name t))
  in
  let t2 = match_token t1 (Tok_Ident tag) in
  let t3 = match_token t2 Tok_GT in
  let children, t4 = parse_document t3 in
  let t5 = match_token t4 Tok_LT_Slash in
  let closing =
    match lookahead t5 with
    | Tok_Ident name -> name
    | t -> raise (ParseError ("parse_element: expected closing tag name, got " ^ token_to_name t))
  in
  if tag <> closing then
    raise (ParseError ("parse_element: mismatched tags <" ^ tag ^ "> vs </" ^ closing ^ ">"));
  let t6 = match_token t5 (Tok_Ident closing) in
  let t7 = match_token t6 Tok_GT in
  (Element (tag, children), t7)

(* ---------------------------------------------------------
   Driver
*)
let pp = Printf.printf

(*
  function parse_str : string -> unit
  tokenizes, parses, and pretty-prints the HTML input
*)
let parse_str str =
  pp "Input = %s\n" str;
  let tokens = tokenize str in
  pp "Input token list = [";
  List.iter (fun t -> pp "%s;" (token_to_name t)) tokens;
  pp "]\n";
  let nodes, remaining = parse_document tokens in
  if remaining <> [ Tok_END ] then
    raise (IllegalExpression "parse_document: leftover tokens");
  pp "AST produced =\n";
  List.iter (fun nd -> pp "%s\n" (node_to_str "" nd)) nodes

;;
Printf.printf "\nExample 1 (nested elements):\n";;
parse_str "<div><p>Hello</p>World</div>"

;;
Printf.printf "\nExample 2 (multiple children):\n";;
parse_str "<html><head><title>Test</title></head><body><p>Hi</p></body></html>"

;;
Printf.printf "\nExample 3 (text only element):\n";;
parse_str "<span>just text</span>"
