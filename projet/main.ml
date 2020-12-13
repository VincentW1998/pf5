open Graphics
open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle
open Printf
open Read


(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)

let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals"

let action_what () = Printf.printf "%s\n" usage; exit 0

let cmdline_options = [
("--what" , Arg.Unit action_what, "description")
]

let extra_arg_action = fun s -> failwith ("Argument inconnu :"^s)


(*let trace () =
  let n = nthIter() in
  let niter = substitution (createWord (explode (getAxiome()))) n in
  let lcmd = interWord(niter) in
  clear_graph();
  turtleToGraphics lcmd (move_point ({x = 400.; y = 10.; a = 90}) 0.)*)

let trace () =
  let n = nthIter() in
  let niter = substitution2 (getRules())
              (createWord (explode (getAxiome()))) n in
  let lcmd = interWord2 (getInter()) (niter) in
  clear_graph();
  turtleToGraphics lcmd (move_point ({x = 400.; y = 10.; a = 90}) 0.)


(* keyStrokes listners  *)
 let rec loop ()=
  let event = wait_next_event [Key_pressed] in
  if event.keypressed
  then match event.key with
    |'o' -> let filename = fileName() in read_file filename ; loop()
    | 't' -> trace();print_string(getAxiome());print_newline() ; loop()
    | 'c' -> clear_graph(); loop()
    | 'q'  -> close_graph ()
    | _    -> loop ()
  else loop ()




let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  open_graph " 500x500";
  loop ();
  print_string "Bye\n"

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)


let () = if not !Sys.interactive then main ()
