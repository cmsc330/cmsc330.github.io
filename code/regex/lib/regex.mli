(** Search "war_peace.txt" for words matching a POSIX regular expression. *)

module WordSet : Set.S with type elt = string

val match_words : string -> WordSet.t
(** [match_words pattern] returns the set of unique, lowercased words from
    "war_peace.txt" that match the POSIX regular expression [pattern].

    Raises [Re.Posix.Parse_error] or [Re.Posix.Not_supported] if [pattern]
    is not a valid POSIX regular expression. *)

val words_matching : string -> string list -> WordSet.t
(** [words_matching pattern words] returns the subset of [words] that match
    [pattern]. Exposed independently of the corpus file for testing.

    Raises [Re.Posix.Parse_error] or [Re.Posix.Not_supported] if [pattern]
    is not a valid POSIX regular expression. *)
