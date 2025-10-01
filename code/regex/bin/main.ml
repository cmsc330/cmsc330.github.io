let () =
  let pattern = "^[^b]*b[^b]*b[^b]*$" in
  Hashtbl.iter
    (fun word _ -> Printf.printf "%s\n" word)
    (Regex.match_words pattern)
