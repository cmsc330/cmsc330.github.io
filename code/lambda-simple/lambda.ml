type var = string
type exp = Var of var | Lam of var * exp | App of exp * exp

(* generates a fresh variable name *)
let newvar =
  let x = ref 0 in
  fun () ->
    let c = !x in
    incr x;
    "v" ^ string_of_int c

(* computes the free (non-bound) variables in e *)
let rec fvs e =
  match e with
  | Var x -> [ x ]
  | Lam (x, e) -> List.filter (fun y -> x <> y) (fvs e)
  | App (e1, e2) -> fvs e1 @ fvs e2

(* substitution: subst e y m means
   "substitute occurrences of variable y with m in the expression e" *)
let rec subst e y m =
  match e with
  | Var x ->
      if y = x then m (* replace x with m *)
      else e (* variables don't match: leave x alone *)
  | App (e1, e2) -> App (subst e1 y m, subst e2 y m)
  | Lam (x, e) ->
      if y = x then (* don't substitute under the variable binder *)
        Lam (x, e)
      else if not (List.mem x (fvs m)) then
        (* no need to alpha convert *)
        Lam (x, subst e y m)
      else
        (* need to alpha convert *)
        let z = newvar () in
        (* assumed to be "fresh" *)
        let e' = subst e x (Var z) in
        (* replace x with z in e *)
        Lam (z, subst e' y m)

let rec reduce e =
  (*let _= Printf.printf "reducing ...\n" in *)
  match e with
  | App (Lam (x, e), e2) -> subst e x e2 (* direct beta rule *)
  | App (e1, e2) ->
      let e1' = reduce e1 in
      (* try to reduce a term in the lhs *)
      if e1' != e1 then App (e1', e2) else App (e1, reduce e2)
      (* didn't work; try rhs *)
  | Lam (x, e) -> Lam (x, reduce e) (* reduce under the lambda (!) *)
  | _ -> e (* no opportunity to reduce *)

let rec reduce_multi e1 =
  let e2 = reduce e1 in
  if e2 = e1 then e1 else reduce_multi e2

(* Church Encodings*)

let zero = Lam ("f", Lam ("x", Var "x"))
let one = Lam ("f", Lam ("x", App (Var "f", Var "x")))
let two = Lam ("f", Lam ("x", App (Var "f", App (Var "f", Var "x"))))

let three =
  Lam ("f", Lam ("x", App (Var "f", App (Var "f", App (Var "f", Var "x")))))
(* "λf. λx. f (f (f x))" *)

let succ =
  Lam
    ( "n",
      Lam ("f", Lam ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x"))))
    )
(*"λn. λf. λx. f (n f x)" *)

let plus =
  Lam
    ( "m",
      Lam
        ( "n",
          Lam
            ( "f",
              Lam
                ( "x",
                  App
                    ( App (Var "m", Var "f"),
                      App (App (Var "n", Var "f"), Var "x") ) ) ) ) )
(* λm. λn. λf. λx. m f (n f x) *)

let mult = Lam ("m", Lam ("n", Lam ("f", App (Var "m", App (Var "n", Var "f")))))

(* "λm. λn. λf. m (n f)"*)
let mytrue = Lam ("x", Lam ("y", Var "x")) (* "λx. λy. x" *)
let myfalse = Lam ("x", Lam ("y", Var "y")) (* λx. λy. y *)
let myif = Lam ("a", Lam ("b", Lam ("c", App (App (Var "a", Var "b"), Var "c"))))
(* λa. λb. λc. a b c *)

let mynot =
  Lam ("b", Lam ("x", Lam ("y", App (App (Var "b", Var "y"), Var "x"))))
(* "λb. λx. λy.  b y x" *)

let myand =
  Lam
    ( "a",
      Lam
        ( "b",
          Lam
            ( "x",
              Lam
                ( "y",
                  App
                    ( App (Var "b", App (App (Var "a", Var "x"), Var "y")),
                      Var "y" ) ) ) ) )
(* λa. λb. λx. λy. b (a x y) y *)

let myor =
  Lam
    ( "a",
      Lam
        ( "b",
          Lam
            ( "x",
              Lam
                ( "y",
                  App
                    ( App (Var "b", Var "x"),
                      App (App (Var "a", Var "x"), Var "y") ) ) ) ) )
(* λa. λb. λx. λy.  b x (a x y) *)

let iszero =
  Lam
    ( "n",
      App
        ( App (Var "n", Lam ("x", Lam ("x", Lam ("y", Var "y")))),
          Lam ("x", Lam ("y", Var "x")) ) )
(* λn. n (λx. (λx.λy. y)) (λx. λy. x) *)

let mypred =
  Lam
    ( "n",
      Lam
        ( "f",
          Lam
            ( "x",
              App
                ( App
                    ( App
                        ( Var "n",
                          Lam
                            ( "g",
                              Lam ("h", App (Var "h", App (Var "g", Var "f")))
                            ) ),
                      Lam ("u", Var "x") ),
                  Lam ("u", Var "u") ) ) ) )
(* λn. λf. λx. n (λg.  λh.  h (g f)) (λu.x) (λu. u) *)

let minus =
  Lam
    ( "m",
      Lam
        ( "n",
          App
            ( App
                ( Var "n",
                  Lam
                    ( "n",
                      Lam
                        ( "f",
                          Lam
                            ( "x",
                              App
                                ( App
                                    ( App
                                        ( Var "n",
                                          Lam
                                            ( "g",
                                              Lam
                                                ( "h",
                                                  App
                                                    ( Var "h",
                                                      App (Var "g", Var "f") )
                                                ) ) ),
                                      Lam ("u", Var "x") ),
                                  Lam ("u", Var "u") ) ) ) ) ),
              Var "m" ) ) )
(* λm. λn. (n λn. λf. λx. n (λg.  λh.  h (g f)) (λu.x) (λu. u)) m *)

let div =
  Lam
    ( "n",
      App
        ( App
            ( Lam
                ( "f",
                  App
                    ( Lam ("x", App (Var "x", Var "x")),
                      Lam ("x", App (Var "f", App (Var "x", Var "x"))) ) ),
              Lam
                ( "c",
                  Lam
                    ( "n",
                      Lam
                        ( "m",
                          Lam
                            ( "f",
                              Lam
                                ( "x",
                                  App
                                    ( Lam
                                        ( "d",
                                          App
                                            ( App
                                                ( App
                                                    ( Lam
                                                        ( "n",
                                                          App
                                                            ( App
                                                                ( Var "n",
                                                                  Lam
                                                                    ( "x",
                                                                      Lam
                                                                        ( "a",
                                                                          Lam
                                                                            ( "b",
                                                                              Var
                                                                                "b"
                                                                            ) )
                                                                    ) ),
                                                              Lam
                                                                ( "a",
                                                                  Lam
                                                                    ( "b",
                                                                      Var "a" )
                                                                ) ) ),
                                                      Var "d" ),
                                                  App
                                                    ( App
                                                        ( Lam
                                                            ( "f",
                                                              Lam ("x", Var "x")
                                                            ),
                                                          Var "f" ),
                                                      Var "x" ) ),
                                              App
                                                ( Var "f",
                                                  App
                                                    ( App
                                                        ( App
                                                            ( App
                                                                ( Var "c",
                                                                  Var "d" ),
                                                              Var "m" ),
                                                          Var "f" ),
                                                      Var "x" ) ) ) ),
                                      App
                                        ( App
                                            ( Lam
                                                ( "m",
                                                  Lam
                                                    ( "n",
                                                      App
                                                        ( App
                                                            ( Var "n",
                                                              Lam
                                                                ( "n",
                                                                  Lam
                                                                    ( "f",
                                                                      Lam
                                                                        ( "x",
                                                                          App
                                                                            ( App
                                                                                ( 
                                                                                App
                                                                                ( 
                                                                                Var
                                                                                "n",
                                                                                Lam
                                                                                ( 
                                                                                "g",
                                                                                Lam
                                                                                ( 
                                                                                "h",
                                                                                App
                                                                                ( 
                                                                                Var
                                                                                "h",
                                                                                App
                                                                                ( 
                                                                                Var
                                                                                "g",
                                                                                Var
                                                                                "f"
                                                                                )
                                                                                )
                                                                                )
                                                                                )
                                                                                ),
                                                                                Lam
                                                                                ( 
                                                                                "u",
                                                                                Var
                                                                                "x"
                                                                                )
                                                                                ),
                                                                              Lam
                                                                                ( 
                                                                                "u",
                                                                                Var
                                                                                "u"
                                                                                )
                                                                            ) )
                                                                    ) ) ),
                                                          Var "m" ) ) ),
                                              Var "n" ),
                                          Var "m" ) ) ) ) ) ) ) ),
          App
            ( Lam
                ( "n",
                  Lam
                    ( "f",
                      Lam
                        ( "x",
                          App (Var "f", App (App (Var "n", Var "f"), Var "x"))
                        ) ) ),
              Var "n" ) ) )

(* Y combinator *)
let yfix =
  Lam
    ( "f",
      App
        ( Lam ("x", App (Var "f", App (Var "x", Var "x"))),
          Lam ("x", App (Var "f", App (Var "x", Var "x"))) ) )
(*  "λf.(λx. f (x x)) (λx. f (x x))" *)
