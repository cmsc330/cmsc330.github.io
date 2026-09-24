let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <pattern>\n" Sys.argv.(0);
    exit 1);
  let pattern = Sys.argv.(1) in
  match Regex.match_words pattern with
  | words -> Regex.WordSet.iter (Printf.printf "%s\n") words
  | exception Re.Posix.Parse_error ->
    Printf.eprintf "Error: invalid pattern %S\n" pattern;
    exit 1
  | exception Re.Posix.Not_supported ->
    Printf.eprintf "Error: pattern %S uses unsupported POSIX syntax\n" pattern;
    exit 1
