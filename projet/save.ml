open String
open Sys
open Systems
open Str

let fileName = ref "";;
let newFile = ref stdout;;
let symbolSet = ref [];;

let outputThenRead output =
  print_string output;
  read_line ()

let rec newfileName () =
  let name = outputThenRead "how would you like to call your file :" in
  if (Sys.file_exists name) then
    (print_string "this file already exist, try a new name\n"; newfileName())
  else name

let rec printCharSet l =
  match l with
  |[] -> ()
  |',' :: l' -> printCharSet l'
  |i :: l' -> print_string (String.concat "" [(String.make 1 i);" | "]); printCharSet l'

(* demande un ensemble fini de symbol pour le
lsysteme et retourne la liste de char correspondant *)
let ensembleFini () =
  let input = explode (outputThenRead "what is the symbol set :") in
  (* retire les , de la liste *)
  let rec filter l acc =
    match l with
    |[] -> acc
    |i :: l' -> if(i != ',') then filter l' (i::acc)
                else filter l' acc in
  printCharSet input;
  print_string ("\n");
  filter input []


let rec checkListValid l =
  match l with
  |[] -> true
  |i :: l' -> if(List.mem i !symbolSet) then checkListValid l' else false ;;

let rec baseExpression () =
  let input = outputThenRead "Base Expression: " in
  let expression = explode input in
  if checkListValid expression then input
    else (print_string "invalid Expression\n"; baseExpression ())

let rec interpretation s =
  let input = outputThenRead (String.concat "" ["do you want to do something for ";String.make 1 s;" ? y/n: "]) in
  match input with
  |"n" -> ()
  |"y" -> let interp = outputThenRead "what do you want to do ? : " in
          if checkListValid (explode interp) then
              output_string !newFile (String.concat " " [String.make 1 s ;interp; "\n"])
          else (print_string "invalid interpretation\n"; interpretation s)
  |_ -> interpretation s
  (* |_ -> failwith "wrong answer" *)

let numeroValide n =
    try int_of_string n |> ignore; true
    with Failure _ -> false

let rec askNumber () =
  let input = outputThenRead "how much ? :" in
  (* if(Str.string_match (Str.regexp "[0-9+-]+$") input 0) then input *)
  if numeroValide input then input
  else (print_string "invalid answer, please enter a number \n"; askNumber())


let rec assignCharToCmd s =
  let input = outputThenRead (String.concat " " ["Which cmd for ";String.make 1 s;" ?: "]) in
  match input with
  | "line" -> output_string !newFile (String.concat "" [String.make 1 s ;" L"; askNumber () ; "\n"])
  | "move" -> output_string !newFile (String.concat "" [String.make 1 s ;" M"; askNumber () ; "\n"])
  | "turn" -> output_string !newFile (String.concat "" [String.make 1 s ;" T"; askNumber () ; "\n"])
  | "store" -> output_string !newFile (String.concat "" [String.make 1 s ;" S";"\n"])
  | "restore" -> output_string !newFile (String.concat "" [String.make 1 s ;" R";"\n"])
  | _ -> (print_string "invalid command\n"; assignCharToCmd s)

  (* cree un fichier donner avec un nom choisi *)
  let writeFile() =
    fileName := newfileName();
    newFile := open_out !fileName;
    output_string !newFile (String.concat " " ["###";!fileName;"\n"]);
    symbolSet := List.rev (ensembleFini());
    let baseExp = baseExpression () in
    output_string !newFile (String.concat "" [baseExp;"\n\n### Regles:\n"]);
    List.iter (interpretation) !symbolSet; (* lance interpretation sur tout les symboles *)
    output_string !newFile "\n## Interpretations:\n";
    List.iter (assignCharToCmd) !symbolSet;
    print_string (String.concat " "[!fileName;"have been created successfully"]);
    (* assign !symbolSet; *)
    close_out !newFile
