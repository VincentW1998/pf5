open String
open Sys

let outputThenRead output =
  print_string output;
  read_line ()

let rec fileName () =
  let name = outputThenRead "how would you like to call your file :" in
  if (Sys.file_exists name) then
    (print_string "this file already exist, try a new name\n"; fileName())
  else name

(* cree un fichier donner avec un nom choisi *)
let writeFile() =
  let fname = fileName() in
  let newFile = open_out fname in
  output_string newFile (String.concat " " ["###";fname;"\n"]);
  close_out newFile
