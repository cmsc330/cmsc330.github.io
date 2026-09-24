(*
  Lists all words from "war and peace" that match the pattern

  More documentation about the Re library:
  https://ocaml.org/p/re/latest/doc/Re/index.html
*)

module WordSet = Set.Make (String)

(* The corpus lives at the project root, but this may run from any
   subdirectory (e.g. under [dune test] or [dune exec]), so search upward
   from the current directory rather than assuming cwd is the root. *)
let find_file name =
  let rec search dir =
    let candidate = Filename.concat dir name in
    if Sys.file_exists candidate then candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith (Printf.sprintf "Could not locate %s" name)
      else search parent
  in
  search (Sys.getcwd ())

(* Read a file line by line*)
let read_lines file =
  let contents =
    In_channel.with_open_bin (find_file file) In_channel.input_all
  in
  String.split_on_char '\n' contents

let str2re t = Re.Posix.compile (Re.Posix.re t)

let word_delimiter = str2re "[<>/,?!. '-()\":;*]"

let split_into_words delimiter lines =
  List.concat_map
    (fun line -> Re.split delimiter line |> List.map String.lowercase_ascii)
    lines

(* Lowercased words are read and split once per process and reused across
   calls, rather than re-reading and re-splitting the corpus every time. *)
let corpus_words =
  lazy (split_into_words word_delimiter (read_lines "war_peace.txt"))

(* Pure matching logic, independent of the corpus file, so it can be tested
   against a small in-memory word list instead of the full book. *)
let words_matching pattern words =
  let re = str2re pattern in
  List.fold_left
    (fun acc word -> if Re.execp re word then WordSet.add word acc else acc)
    WordSet.empty words

(* Return all unique, lowercased words from "war_peace.txt" matching [pattern] *)
let match_words pattern = words_matching pattern (Lazy.force corpus_words)
