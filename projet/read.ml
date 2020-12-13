open String
let lsys = ref [];;

(* seperate the file into [axiome;rules;interpretation] then send it to lsys *)
let rec print l =
  match l with
  |[] -> print_string "fin"; ()
  |a :: l' -> print_string (a) ; print l'

let read_file name =
  lsys := [];
  let f = open_in name in
  let rdLine () =
    try Some (input_line f) with End_of_file -> None in
  (* return the block of string until it finds /n *)
    let rec loopSeparator str =
      match rdLine () with
      | Some s -> if(String.length(s) = 0)then str (* if s length = 0 then it is \n *)
                  else (match s.[0] with
                        |'#' -> loopSeparator str
                        | _ -> if(String.length(str) = 0) then loopSeparator s
                               else loopSeparator (concat "\n" [str; s]))
      | None -> close_in f; str in
      (* List.rev([loopSeparator "";loopSeparator "";loopSeparator ""]) *)
    lsys :=  List.rev([loopSeparator "";loopSeparator "";loopSeparator ""])

(* let lsys name = read_file name *)

let rec nth l k = match l with
  | [] -> failwith "liste vide"
  | x :: _  when k = 0 -> x
  | _ :: xs -> nth xs (k - 1)

  let errFile () = print_string "Type filename : "; let file = read_line() in read_file file
  let rec getAxiome () = if List.length (!lsys) <> 0 then nth (!lsys) 0 else( errFile (); getAxiome())
  let rec getRules () = if List.length (!lsys) <> 0 then String.split_on_char '\n' (nth(!lsys)1) else(errFile (); getRules())
  let rec getInter () = if List.length (!lsys) <> 0 then String.split_on_char '\n' (nth(!lsys)2) else (errFile (); getInter())
