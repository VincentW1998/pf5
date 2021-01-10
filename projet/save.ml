open String
open Sys
open Systems

let outputThenRead output =
  print_string output;
  read_line ()

let rec newfileName () =
  let name = outputThenRead "how would you like to call your file :" in
  if (Sys.file_exists name) then
    (print_string "this file already exist, try a new name\n"; newfileName())
  else name

let ensembleFini () =
  let input = explode (outputThenRead "what is the symbol set :") in
  let rec filter l acc =
    match l with
    |[] -> acc
    |i :: l' -> if(i != ',') then filter l' (i::acc)
                else filter l' acc in
  print_string (String.of_seq (List.to_seq input));
  filter input []

let rec checkListValid l set =
  match l with
  |[] -> true
  |i :: l' -> if(List.mem i set) then checkListValid l' set else false ;;

let rec baseExpression set =
  let input = outputThenRead "Base Expression: " in
  let expression = explode input in
  if checkListValid expression set then input
    else (print_string "invalid Expression"; baseExpression set)

  (* cree un fichier donner avec un nom choisi *)
  let writeFile() =
    let fname = newfileName() in
    let newFile = open_out fname in
    output_string newFile (String.concat " " ["###";fname;"\n"]);
    let set = ensembleFini () in
    let baseExp = baseExpression set in

    output_string newFile (String.concat "" [baseExp;"\n"]);
    close_out newFile
