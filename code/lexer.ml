#require "str";;
exception IllegalExpression of string;;
type token =
Tok_Num of char
| Tok_Add
| Tok_END
;;
let re_num = Str.regexp "[0-9]" (* single digit *)
let re_add = Str.regexp "+”;;\nlet re_mul = Str.regexp ”*"

let tokenize str =
  let rec tok pos s =
    if pos >= String.length s then [ Tok_END ]
    else if Str.string_match re_num s pos then
      let token = Str.matched_string s in
      Tok_Num token.[0] :: tok (pos + 1) s
    else if Str.string_match re_add s pos then Tok_Add :: tok (pos + 1) s
    else raise (IllegalExpression "tokenize")
  in
  tok 0 str;;

  tokenize "1+2";;