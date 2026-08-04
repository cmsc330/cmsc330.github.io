(* A small demo program. Run it with: dune exec bin/main.exe *)
open Inventory

let () =
  let stock =
    Stock.empty
    |> Stock.add (Item.make "USB cable" 4.99 20)
    |> Stock.add (Item.make "Keyboard" 29.99 10)
    |> Stock.add (Item.make "Monitor" 129.00 4)
  in
  let stock = Stock.remove "Keyboard" 3 stock in
  (match Stock.find "Keyboard" stock with
   | Some it -> Printf.printf "Keyboard: %d in stock\n" (Item.qty it)
   | None -> print_endline "Keyboard: out of stock");
  Printf.printf "Total value: $%.2f\n" (Stock.total_value stock)
