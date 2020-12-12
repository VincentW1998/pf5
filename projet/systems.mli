(** Words, rewrite systems, and rewriting *)

type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }

(** Put here any type and function interfaces concerning systems *)

(** return char list from string**)
val explode : string -> char list

val insert : 'a -> 'a list -> 'a list

(** function insert sort **)
val sort : 'a list -> 'a list

(**function return sorted list of symb without replication**)
val list_of_symb_loop : 'a word list ->
              'a list -> 'a word list

val list_of_symb : char list -> char word list

 (** return a list from the first ']' **)
val cutBrackets : char list -> int -> char list

(** function return a char word from  'a list**)
val createWord_loop : char word list -> char list -> char word list


val createWord : char list -> char word

(** funtion return 's word from string **)
val stringToWord : string -> char word

(** function return axiom from string **)
val stringToAxiom : char word

val pairCharString : string -> char * string

val listPair_loop :
  (char * string) list -> string list -> (char * string) list

val listPair : string list -> (char * string) list

val rewrite_loop : 'a -> ('a * string) list -> char word

val rewrite : char -> char word

val charToCommand : int -> char -> Turtle.command list

val inter_loop : 'a -> ('a * string) list -> Turtle.command list

val inter : char -> Turtle.command list
