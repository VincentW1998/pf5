open Graphics
open Unix
open Lsystems (* Librairie regroupant le reste du code. Cf. fichier dune *)
open Systems (* Par exemple *)
open Turtle
open Printf
open Read
open Save

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

(**fonction auxiliare pour la fonction animation**)
let rec animation_loop i n pos =
if i > n then () else
  let niter = substitution (getRules())
              (createWord (explode (getAxiome()))) i in
  let lcmd = interpWord (getInter()) (niter) in
  let facteur = (1. /. 3.) ** (float_of_int i) in
  auto_synchronize false;
  Unix.sleepf 0.3;
  clear_graph();
  let newPos,newFacteur = getNewPosFacteur lcmd pos facteur in
  turtleToGraphics lcmd (move_point newPos 0.) newFacteur;
  synchronize();
  animation_loop (i+1) n newPos

(**animation**)
let animation n =
  let pos = {x = 60.; y = 60.; a = 0} in
    animation_loop 0 n pos

(**draw n iteration about one Lsystem with animation**)
let trace () =
  let n = nthIter() in
  animation n


(* keyStrokes listners  *)
 let rec loop ()=
  let event = wait_next_event [Key_pressed] in
  if event.keypressed
  then match event.key with
    |'o' -> let filename = fileName() in read_file filename ; loop()
    |'t' -> trace(); loop()
    |'c' -> clear_graph(); loop()
    |'q' -> close_graph ()
    |'n' -> writeFile(); loop()
    | _    -> loop ()
  else loop ()




let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  open_graph " 600x600";
  set_window_title "L-Systeme";
  loop ();
  print_string "Bye\n"

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = Printexc.record_backtrace true
let () = if not !Sys.interactive then main ()
