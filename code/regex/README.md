# regex

Search *War and Peace* for words matching a regex pattern using OCaml's [Re.Posix](https://ocaml.org/p/re/latest/doc/Re/index.html) library.

## Usage

1. **Build:**

   ```sh
   dune build
   ```

2. **Run**, passing a POSIX regular expression as an argument:

   ```sh
   dune exec bin/main.exe -- '^[^b]*b[^b]*b[^b]*$'
   ```

   The example above matches all words containing exactly two `b`s.

   All unique words from *War and Peace* that match the pattern are printed to stdout, sorted alphabetically, one per line.

## Notes

- Words are lowercased and deduplicated before matching.
- Delimiters (spaces, punctuation, etc.) are used to split lines into words.
- See the [Re library docs](https://ocaml.org/p/re/latest/doc/Re/index.html) for pattern syntax reference.
