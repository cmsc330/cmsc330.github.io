(*
open Lambda
open Pp
*)
#use "lambda.ml"
#use "pp.ml"
let r = Lam ("x", App (Var "x", Var "y"));;

Printf.printf "Lambda Expression: λx.x y\n";;
Printf.printf "AST: %s\n" (lambda_exp_2_str r);;
Printf.printf "unparsed AST: ";;
print_lambda r;;

Printf.printf "\nExample: if expression\n"

(* and true true  *)
let e = App (App (myand, mytrue), mytrue);;

(*  if e then 1 else 2 *)
let e2 = App (App (App (myif, e), one), two)
let e2 = reduce_multi e2
let () = Printf.printf "\nif true then 1 else 2 = %d\n" (to_int e2)

(*Example 2 *)

(* and true false  *)
let e = App (App (myand, mytrue), myfalse)

(*   e2 = if e then 1  else 2 *)
let e2 = App (App (App (myif, e), one), two)
let e2 = reduce_multi e2;;
let () = Printf.printf "\nif false then 1 else 2 = %d\n" (to_int e2)
;;
(* Example 3: Addition
   3 + 2
*)
Printf.printf "\nExample: Addition\n"
let e1 = App (App (plus, three), two)
let e2 = reduce_multi e1;;
let () = Printf.printf "3 + 2 = %d\n" (to_int e2);;

(* Example 4  Multiplication
   let e1 = 3 + 2
   let e2 = 3 + 1
   let e3 = e1 * e2
*)
Printf.printf "\nExample: Multiplication\n"
let e1 = App (App (plus, three), two)
let e2 = App (succ, three)
let e3 = App (App (mult, e1), e2)
let e4 = reduce_multi e3
let () = Printf.printf "5 * 4 = %d\n" (to_int e4)

(* let e5 = (e3+e2) * e4
            (5 + 4 ) * 20
*)
let e5 = App (App (mult, App (App (plus, e1), e2)), e4)
let e6 = reduce_multi e5
let () = Printf.printf "(5 + 4) * 20 = %d\n" (to_int e6);;

Printf.printf "\nExample: Integer Deivision\n"

(*   4 / 2  *)
let e = App(App(div,App(succ,three)),two);;
Printf.printf "succ(3)/2 =  %d\n" (to_int (reduce_multi e));;

Printf.printf "\nExample: Recursion: Factorial\n";;
(*
   Y = \f.(\x. f (x x)) (\x. f (x x))
  fact = \f.\n. if n = 0 then 1 else n * (f (n -1))
 *)

(* Example 4
   fact(3)
*)

let if2 (a, b, c) = App (App (App (myif, a), b), c)

let fact1 =
  Lam
    ( "f",
      Lam
        ( "n",
          if2
            ( App (iszero, Var "n"),
              (* condition *)
              one,
              (* if branch *)
              App (App (mult, Var "n"), App (Var "f", App (mypred, Var "n")))
              (* else *) ) ) )

(* calculate factorial of 3  *)
let e = App (App (yfix, fact1), three)

(* print the factorial 3 as int *)
let x = to_int (reduce_multi e) (*  6 *);;

Printf.printf "fact(3) = %d\n" x;;


(* calculate factorial of 4  *)

(*
let four = reduce_multi (App(succ,three));;
let e  = App(App(yfix, fact1), four);;

let x = to_int (reduce_multi e)   (*  6 *)
;;
Printf.printf "fact(4) = %d\n" x;;
*)