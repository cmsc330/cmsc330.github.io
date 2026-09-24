let check name expected actual =
  if not (Regex.WordSet.equal expected actual) then (
    Printf.printf "FAIL: %s\n  expected: %s\n  actual:   %s\n" name
      (String.concat ", " (Regex.WordSet.elements expected))
      (String.concat ", " (Regex.WordSet.elements actual));
    exit 1)
  else Printf.printf "PASS: %s\n" name

let words = [ "cat"; "dog"; "bobby"; "rabbit"; "abba" ]
let set = Regex.WordSet.of_list

let () =
  check "substring match" (set [ "cat" ]) (Regex.words_matching "cat" words);
  check "words containing a letter"
    (set [ "bobby"; "rabbit"; "abba" ])
    (Regex.words_matching "b" words);
  check "anchored: exactly two b's"
    (set [ "rabbit"; "abba" ])
    (Regex.words_matching "^[^b]*b[^b]*b[^b]*$" words);
  check "no matches" Regex.WordSet.empty
    (Regex.words_matching "^zzz$" words);
  check "duplicate words are deduplicated" (set [ "cat" ])
    (Regex.words_matching "cat" [ "cat"; "cat"; "cat" ]);
  (match Regex.words_matching "(" words with
  | exception Re.Posix.Parse_error -> Printf.printf "PASS: invalid pattern raises Parse_error\n"
  | _ ->
    Printf.printf "FAIL: invalid pattern raises Parse_error\n";
    exit 1);
  print_endline "All tests passed."
