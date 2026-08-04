# Exercise: A Store Inventory Library

In this exercise you build a small library for managing a store's
inventory. It practices everything from the modules chapter: defining
modules, writing signatures (`.mli` files), hiding representations with
abstract types, maintaining invariants, and building and testing a
multi-file project with `dune`.

## Project layout

```
inventory/
├── dune-project
├── lib/
│   ├── dune
│   ├── item.ml        <- module Item  (you complete this)
│   └── stock.ml       <- module Stock (you complete this)
├── bin/
│   ├── dune
│   └── main.ml        <- demo executable (given)
└── test/
    ├── dune
    └── test_inventory.ml  <- OUnit test suite (given)
```

The `lib/` directory is a dune **library** named `inventory`. Dune wraps
the files of the library into one top-level module, so from the outside
the two modules are `Inventory.Item` and `Inventory.Stock`.

## The modules

### `Item` (`lib/item.ml`)

An item held in stock: a name, a unit price, and a quantity on hand.

| Function | Specification |
|---|---|
| `make name price qty` | Creates an item. Raises `Invalid_argument "Item.make"` if `price < 0.0` or `qty < 0`. |
| `name`, `price`, `qty` | Accessors (given). |
| `restock n item` | `item` with `n` more units. Raises `Invalid_argument "Item.restock"` if `n < 0`. |
| `value item` | Unit price times quantity on hand. |

### `Stock` (`lib/stock.ml`)

A persistent collection of items with **at most one item per name**
(an invariant your functions must maintain).

| Function | Specification |
|---|---|
| `empty` | The empty stock (given). |
| `is_empty s` | Whether `s` has no items. |
| `size s` | Number of distinct items. |
| `find name s` | `Some item` with that name, or `None`. |
| `add item s` | Adds `item`; if the name already exists, quantities merge and the most recent price wins. |
| `remove name n s` | Removes `n` units; the item disappears when its quantity reaches 0. Raises `Not_found` (unknown name) or `Invalid_argument "Stock.remove"` (`n < 0` or more than on hand). |
| `total_value s` | Sum of the values of all items. |

All operations are functional: they return a *new* stock and never
modify the old one.

## Your tasks

1. **Implement the modules.** Replace every `failwith "TODO"` in
   `lib/item.ml` and `lib/stock.ml` following the specifications above
   (they are also repeated in the comments). Build as you go:

   ```
   dune build
   ```

2. **Test with dune.** The test suite in `test/test_inventory.ml` is the
   executable specification of this exercise. You need the `ounit2`
   package (`opam install ounit2`). Run:

   ```
   dune runtest
   ```

   You are done with this task when all tests pass.

3. **Define signatures.** Right now the library has no `.mli` files, so
   *everything* is exposed: clients of the library can see that
   `Item.t` is a record and `Stock.t` is a list, and can bypass `make`
   to build items that violate the invariants (e.g., a negative price,
   or a stock with two entries for the same name).

   Write `lib/item.mli` and `lib/stock.mli` that declare **abstract**
   types `t` and expose *only* the functions in the tables above. Any
   helper functions you wrote must not appear in the signatures.
   Rebuild and rerun the tests: they must still pass.

4. **Check the abstraction.** Try adding this line to `bin/main.ml`:

   ```ocaml
   let broken = Inventory.Item.{ name = "Free monitor"; price = -129.0; qty = 4 }
   ```

   Before task 3 it compiles; after task 3 it must be rejected. Explain
   why in a comment. (Remove the line again afterwards.)

5. **Swap the representation.** Change `Stock.t` from a list of items to
   an association list `(string * Item.t) list`, updating your function
   bodies. Which files other than `stock.ml` did you have to touch?
   Why?

## Running the demo

Once the library is implemented:

```
dune exec bin/main.exe
```

Expected output:

```
Keyboard: 7 in stock
Total value: $825.73
```

## Solution

A reference solution (including the `.mli` files) is in
[`../inventory-solution`](../inventory-solution). Try the exercise
yourself before looking at it.
