open List
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

(** Put here any type and function implementations concerning systems *)

(**A[P[PA]A]**)

(**
Seq [Symb A; 
  Branch (Seq [Symb P; 
    Branch (Seq [Symb P; Symb A]); Symb A])]
 **)

(**function change string to char list
 code from my project of Logique**)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(**function return sorted list of symb without replication**)
let rec list_of_symb_loop accu sl = match sl with
  | [] -> sort accu
  | h :: t -> if mem h accu then list_of_symb_loop accu t else
        list_of_symb_loop (h :: accu) t

let list_of_symb sl = list_of_symb_loop []
    (filter (fun x -> x <> '[' && x <> ']') sl)


