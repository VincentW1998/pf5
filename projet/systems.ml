open List
open Turtle
(* open Read *)
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

(** funtion return 's word from string **)
let stringToWord str =
  let s = explode str in
  createWord s

(** (c, sub)
 c = first char of string
 sub = substring **)
let pairCharString str =
  let c = String.get str 0 in
  let sub = String.sub str 2 (String.length str - 2) in
  (c, sub)


let rec listPair_loop accu = function
  | [] -> (List.rev accu)
  | h :: t -> listPair_loop (pairCharString h :: accu) t

(** return list of (char * string) **)
let listPair listStr = listPair_loop [] listStr


let rec rewrite_loop  c lr = match lr with
  | [] -> raise Not_found
  | (a, b) :: t-> if a = c then  stringToWord b else rewrite_loop c t;;

(**function rewrite 's word with rules**)
let rewriteFunc =
  let lr = listPair (Read.getRules()) in
  (fun x -> rewrite_loop x lr)



(** return a list of Turtle.command **)
let charToCommand i = function
  |'L' -> [Line i]
  |'M' -> [Move i]
  |'T' -> [Turn i]
  |'S' -> [Store]
  |'R' -> [Restore]
  | _ -> raise (failwith "votre commande n'existe pas")


let rec inter_loop  c li= match li with
  | [] -> []
  | (a, b) :: t -> if a = c then
        let i = int_of_string (String.sub b 1 (String.length b - 1)) in
        let firstChar = String.get b 0 in
        charToCommand i firstChar else inter_loop c t


(**return a list of Turtle.command from a char **)
let interFunc =
  let li = listPair (Read.getInter()) in
  (fun x -> inter_loop x li)

(**create a Lsys from a string **)
let createLsys ax = {
  axiom = stringToWord ax;
  rules = rewriteFunc;
  interp = interFunc }

(* apply rules to word once *)
let rec  substitution_loop word  =
  match word with
  |Symb s ->  (try rewriteFunc s with Not_found -> Symb s)
  |Seq s -> Seq (List.map (substitution_loop) s )
  |Branch s -> Branch(substitution_loop s)

  (* apply n times rules to the word *)
let rec substitution word n =
  if (n > 0) then substitution  (substitution_loop word) (n-1)
  else word

(* interWord (iter (createWord l) 1) *)
let rec interWord word  =
  match word with
  |Symb s -> interFunc s
  |Seq s -> concat(map(interWord) s)
  |Branch s -> [Store] @ (interWord s) @ [Restore]

let rewrite lr=
  let lrules = listPair lr in
  (fun x -> rewrite_loop x lrules)

let interp li =
  let linterp = listPair li in
  (fun x -> inter_loop x linterp)

let rec  substitution_loop2 lr = function
  |Symb s ->  (try rewrite lr s with Not_found -> Symb s)
  |Seq s -> Seq (List.map (substitution_loop2 lr)  s )
  |Branch s -> Branch(substitution_loop2 lr s)

let rec substitution2 lr word n =
  if (n > 0) then substitution2 lr (substitution_loop2 lr word) (n-1)
  else word

let rec interWord2 li = function
  |Symb s -> interp li s
  |Seq s -> concat(map(interWord2 li) s)
  |Branch s -> [Store] @ (interWord2 li s) @ [Restore]



