open String
let lsys = ref []

(* seperate the file into [axiome;rules;interpretation] then send it to lsys *)
let read_file name =
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
    lsys :=  List.rev([loopSeparator "";loopSeparator "";loopSeparator ""])

let getAxiome () =
  match !lsys with
  |[] -> failwith ("no file")
  |a :: l' -> a

let getRules () =
    match !lsys with
    |[] -> failwith ("no file")
    |a :: r :: l' -> String.split_on_char '\n' r
    |_ -> failwith ("read error : Rules missing")

let getInter () =
  match !lsys with
  |[] -> failwith ("no file")
  |a :: r :: i :: l' -> String.split_on_char '\n' i
  |_ -> failwith ("read error : Interpretation missing")
