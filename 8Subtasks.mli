val compare : 'a -> 'a list -> bool
val equal_second_components : 'a * 'b -> ('c * 'b) list -> bool
val evens_eq_evens_odds_eq_odds : int -> int list -> bool
val member : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val testing_member : unit -> int list
val count_occurrences_longer : 'a list -> ('a * int) list
val count_occurrences : 'a list -> ('a * int) list
val testing_count_occurrences : unit -> int list
val drop_last_but_longer : int list -> int list
val drop_last : 'a list -> 'a list
val testing_drop_last : unit -> int list
val drop_last_opt : 'a list -> 'a list option
val testing_drop_last_opt : unit -> int list
val zip_with : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val testing_zip_with : unit -> int list
val without_fold : ('a * 'b) list -> 'a list * 'b list
val unzip : ('a * 'b) list -> 'a list * 'b list
val testing_unzip : unit -> int list
type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha
val wc22_C : (team * string list * team * string list) list
val wc22_H : (team * string list * team * string list) list
val table_and_scorers :
  ('a * 'b list * 'a * 'b list) list ->
  ('a * int * int * int * int * int * int * int) list * ('b * 'a * int) list
val testing_table_and_scorers : unit -> int list
