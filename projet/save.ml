open String
open Sys
open Systems
open Str

let fileName = ref "";;
let newFile = ref stdout;;
let symbolSet = ref [];;

(* ecrit sur la sortie standard et lis l'entree *)
let outputThenRead output =
  print_string output;
  read_line ()

(*  demande un nom pour le nouveau fichier et renvoie ce nom *)
let rec newfileName () =
  let name = outputThenRead "how would you like to call your file : " in
  if (Sys.file_exists name) then
    (print_string "this file already exist, try a new name\n"; newfileName())
  else name

(* affiche la liste de char separer par des | *)
let rec printCharSet l =
  match l with
  |[] -> ()
  |i :: l' -> print_string (String.concat "" [(String.make 1 i);" | "]); printCharSet l'

(* demande un ensemble fini de symbol pour le
lsysteme et retourne la liste de char correspondant *)
let ensembleFini () =
  let input = explode (outputThenRead "what is the symbol set : ") in
  List.iter (fun x -> print_string (String.concat " "[String.make 1 x ; "| "])) input;
  print_newline();
  print_newline();
  input

(* verifie que toutes les valeurs de la liste sont bien dans symbolSet hormis les crochets *)
let checkListValid lst =
  let rec loop l op cl=
    match l with
    |[] -> if op == cl then true else false
    |'[' :: l' -> loop l' (op+1) cl
    |']' :: l' -> loop l' op (cl+1)
    |i :: l' -> if(List.mem i !symbolSet) then loop l' op cl else false in
  loop lst 0 0

(* demande une expression de base et la renvoie si elle est valides *)
let rec baseExpression () =
  let input = outputThenRead "Base Expression : " in
  let expression = explode input in
  if checkListValid expression then input
    else (print_string "invalid Expression\n"; baseExpression ())

(* demande une interpretation pour chaque l'element s *)
let rec interpretation s =
  let input = outputThenRead (String.concat "" ["do you want to add an interpretation for ";String.make 1 s;" ? y/n: "]) in
  match input with
  |"n" -> ()
  |"y" -> let interp = outputThenRead "what do you want to do : " in
          if checkListValid (explode interp) then
              output_string !newFile (String.concat " " [String.make 1 s ;interp; "\n"])
          else (print_string "invalid interpretation\n"; interpretation s)
  |_ -> interpretation s

(* verifie si le string donnee est bien un nombre *)
let numeroValide n =
    try int_of_string n |> ignore; true
    with Failure _ -> false

(* demande une valeur pour la commande *)
let rec askNumber () =
  let input = outputThenRead "how much : " in
  if numeroValide input then input
  else (print_string "invalid answer, please enter a number \n"; askNumber())


(* assigne une commande a un symbole *)
let rec assignCharToCmd s =
  let input = outputThenRead (String.concat " " ["Which cmd for ";String.make 1 s;" : "]) in
  match input with
  | "line" -> output_string !newFile (String.concat "" [String.make 1 s ;" L"; askNumber () ; "\n"])
  | "move" -> output_string !newFile (String.concat "" [String.make 1 s ;" M"; askNumber () ; "\n"])
  | "turn" -> output_string !newFile (String.concat "" [String.make 1 s ;" T"; askNumber () ; "\n"])
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
    print_string (String.concat " "[!fileName;"have been created successfully\n"]);
    close_out !newFile
