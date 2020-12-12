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

(** return char list from string**)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec insert x l = match l with
  | [] -> [x]
  | t :: q -> if t < x then t :: (insert x q)
      else if x < t then x :: t :: q else l

(** function insert sort **)
let rec sort l = match l with
  |[] -> []
  |x :: xs -> insert x (sort xs)


(**function return sorted list of symb without replication**)
let rec list_of_symb_loop accu = function
  | [] -> sort accu
  | h :: t -> if mem (Symb h) accu then list_of_symb_loop accu t else
        list_of_symb_loop ( Symb h :: accu) t

let list_of_symb sl = list_of_symb_loop []
    (filter (fun x -> x <> '[' && x <> ']') sl)


(** return a list from the first ']' **)
let rec cutBrackets l n = match l with 
  | [] -> l
  | '[' :: t -> cutBrackets t (n+1)
  | ']' :: t -> if n = 0 then t else cutBrackets t (n-1)
  | h :: t -> cutBrackets t n

(** function return a char word from  'a list**)
let rec createWord_loop (accu : 'a list) = function
  | [] | ']' :: _ ->  (List.rev accu)
  | '[' :: t  ->
      createWord_loop ((Branch (Seq ( createWord_loop [] t))) :: accu)
        (cutBrackets t 0)
  | h :: t -> createWord_loop (Symb h :: accu) t


let createWord cl = Seq (createWord_loop [] cl)

