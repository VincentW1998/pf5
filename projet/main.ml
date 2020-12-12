(* open Graphics;; *)

open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle
open Printf
open Read



(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)
(* let file = ref "." *)

(* let setFile f = file := f; read_dir;; *)

(* let read_file file =
  let op = open_in file in
  let rdLine = input_line op;in
  let rec printall = print_endline rdLine; printall in
  try
    printall
  with End_of_file -> flush stdout;
  close_in op;;
  (* with e -> close_in_noerr op;
            raise e *) *)
(* let read_file name =
  let f = open_in name in
  let rdLine () =
    try Some (input_line f) with End_of_file -> None in
  let rec loop () =
    match rdLine () with
    | Some s -> print_endline s; loop ()
    | None -> flush stdout; close_in f in
  loop () *)


let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let action_what () = Printf.printf "%s\n" usage; exit 0




let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
("-f", Arg.String (read_file), "lit le fichier de sauvegarde");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)

let main () =
  (* open_graph " 500x500"; *)
  Arg.parse cmdline_options extra_arg_action usage;
  print_string "Bye\n"

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)


let () = if not !Sys.interactive then main ()

(* keyStrokes listners *)
(* let rec loop t =
  let event = wait_next_event [Key_pressed] in
  if event.keypressed
  then match event.key with
       | 'q'  -> close_graph ()
       | _    -> loop t
  else loop t

let _ = loop 5 *)
