# Store Inventory Exercise — Reference Solution

This is the complete solution to the exercise in `../inventory`,
including the `.mli` signature files from task 3 (`lib/item.mli` and
`lib/stock.mli` make both types abstract and hide the `drop` helper).

Build, test, and run:

```
dune build
dune runtest        # 16 tests, all pass
dune exec bin/main.exe
```

Expected demo output:

```
Keyboard: 7 in stock
Total value: $825.73
```

Notes on the tasks:

- **Task 4:** after the `.mli` files are added, the record literal
  `Inventory.Item.{ name = ...; price = -129.0; qty = 4 }` is rejected
  with `Unbound record field name`: outside the module, `Item.t` is
  abstract, so its fields (and any way to build a value other than
  `make`) are invisible. That is exactly what protects the
  no-negative-price invariant.
- **Task 5:** switching `Stock.t` to `(string * Item.t) list` requires
  touching only `stock.ml`. `stock.mli` keeps `type t` abstract, so no
  client (neither `main.ml` nor the tests) can observe the change.
