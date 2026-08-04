(* Test suite for the store inventory exercise.
   Run with: dune runtest *)
open OUnit2
open Inventory

let feq = cmp_float ~epsilon:1e-9

(* A small stock used by several tests:
   20 USB cables at $4.99, 10 keyboards at $29.99 *)
let sample =
  Stock.empty
  |> Stock.add (Item.make "USB cable" 4.99 20)
  |> Stock.add (Item.make "Keyboard" 29.99 10)

let item_tests =
  "Item" >::: [
    "make_and_accessors" >:: (fun _ ->
      let it = Item.make "Monitor" 129.00 4 in
      assert_equal "Monitor" (Item.name it);
      assert_equal ~cmp:feq ~printer:string_of_float 129.00 (Item.price it);
      assert_equal ~printer:string_of_int 4 (Item.qty it));

    "make_negative_price" >:: (fun _ ->
      assert_raises (Invalid_argument "Item.make")
        (fun () -> Item.make "Monitor" (-129.00) 4));

    "make_negative_qty" >:: (fun _ ->
      assert_raises (Invalid_argument "Item.make")
        (fun () -> Item.make "Monitor" 129.00 (-4)));

    "value" >:: (fun _ ->
      let it = Item.make "Monitor" 129.00 4 in
      assert_equal ~cmp:feq ~printer:string_of_float 516.00 (Item.value it));

    "restock" >:: (fun _ ->
      let it = Item.make "Monitor" 129.00 4 in
      let it = Item.restock 6 it in
      assert_equal ~printer:string_of_int 10 (Item.qty it));

    "restock_negative" >:: (fun _ ->
      let it = Item.make "Monitor" 129.00 4 in
      assert_raises (Invalid_argument "Item.restock")
        (fun () -> Item.restock (-1) it));
  ]

let stock_tests =
  "Stock" >::: [
    "empty_is_empty" >:: (fun _ ->
      assert_bool "empty stock" (Stock.is_empty Stock.empty);
      assert_equal ~printer:string_of_int 0 (Stock.size Stock.empty));

    "add_and_find" >:: (fun _ ->
      assert_equal ~printer:string_of_int 2 (Stock.size sample);
      match Stock.find "USB cable" sample with
      | None -> assert_failure "USB cable should be in stock"
      | Some it -> assert_equal ~printer:string_of_int 20 (Item.qty it));

    "find_missing" >:: (fun _ ->
      assert_equal None (Stock.find "Webcam" sample));

    "add_merges_quantities" >:: (fun _ ->
      (* Adding an item that already exists merges the quantities
         and keeps the most recent price. *)
      let s = Stock.add (Item.make "USB cable" 5.49 5) sample in
      assert_equal ~printer:string_of_int 2 (Stock.size s);
      match Stock.find "USB cable" s with
      | None -> assert_failure "USB cable should be in stock"
      | Some it ->
        assert_equal ~printer:string_of_int 25 (Item.qty it);
        assert_equal ~cmp:feq ~printer:string_of_float 5.49 (Item.price it));

    "remove_some" >:: (fun _ ->
      let s = Stock.remove "Keyboard" 3 sample in
      match Stock.find "Keyboard" s with
      | None -> assert_failure "Keyboard should still be in stock"
      | Some it -> assert_equal ~printer:string_of_int 7 (Item.qty it));

    "remove_all_drops_item" >:: (fun _ ->
      let s = Stock.remove "Keyboard" 10 sample in
      assert_equal None (Stock.find "Keyboard" s);
      assert_equal ~printer:string_of_int 1 (Stock.size s));

    "remove_missing_raises" >:: (fun _ ->
      assert_raises Not_found
        (fun () -> Stock.remove "Webcam" 1 sample));

    "remove_too_many_raises" >:: (fun _ ->
      assert_raises (Invalid_argument "Stock.remove")
        (fun () -> Stock.remove "Keyboard" 11 sample));

    "total_value" >:: (fun _ ->
      (* 20 * 4.99 + 10 * 29.99 = 99.80 + 299.90 = 399.70 *)
      assert_equal ~cmp:feq ~printer:string_of_float
        399.70 (Stock.total_value sample));

    "persistence" >:: (fun _ ->
      (* Stocks are immutable: removing from [sample] must not
         change [sample] itself. *)
      let _ = Stock.remove "Keyboard" 10 sample in
      match Stock.find "Keyboard" sample with
      | None -> assert_failure "sample must be unchanged"
      | Some it -> assert_equal ~printer:string_of_int 10 (Item.qty it));
  ]

let () =
  run_test_tt_main ("inventory" >::: [item_tests; stock_tests])
