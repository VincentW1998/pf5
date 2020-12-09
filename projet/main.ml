open Graphics;;
let _ = open_graph " 500x500"
open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle


(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)

let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let action_what () = Printf.printf "%s\n" usage; exit 0

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
("-f", Arg.String (read_dir), "lit le fichier de sauvegarde");
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  print_string "Pour l'instant je ne fais rien\n"

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)
let read_dir dir =
  let _ = try open_out dir with Sys_error printf("Permission denied \n");


let () = if not !Sys.interactive then main ()

let rec loop t =
  let event = wait_next_event [Key_pressed] in
  if event.keypressed
  then match event.key with
       | 'q'  -> close_graph ()
       | _    -> loop t
  else loop t

let _ = loop 5
