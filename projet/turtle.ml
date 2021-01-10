open Stack
open Graphics

exception Erreur_out;;

type command =
| Line of int
| Move of int
| Turn of int
| Store
| Restore

type position = {
  x: float;      (** position x *)
  y: float;      (** position y *)
  a: int;        (** angle of the direction *)
}

(** Put here any type and function implementations concerning turtle *)

(* New stack, initially empty *)
let stackOfPos = create()

(** constant pi **)
let pi = 4.*.atan(1.)

(** function change degree to rad**)
let degreeToRad = (fun x -> (float_of_int x) *. (pi /. 180.))

(** get the sign of x and add this sign to 0.5**)
let getSign = (fun x -> if x < 0. then (-0.5) else 0.5)

(** round the float **)
let roundFloat x =
  int_of_float (x +. getSign x)

(* Polar cordinate to cartesian for axe X *)
let cordinateX p length =
   int_of_float (p.x) + roundFloat(length  *. cos (degreeToRad p.a))

(* Polar cordinate to cartesian for axe Y *)
let cordinateY p length =
  int_of_float (p.y) + roundFloat(length *. sin (degreeToRad p.a))

(* Polar cordinate to cartesian for axe X and Y *)
let cordinateXY p length =
  let x1 = cordinateX p length and
    y1 = cordinateY p length in
    (x1, y1);;

(* draw line with Graphics.lineto *)
let draw_line pos taille =
  let (x1, y1)  = cordinateXY pos taille in lineto x1 y1;
  {x = (float_of_int x1); y = (float_of_int y1); a = pos.a};;

(* move the current point *)
let move_point pos a =
  let (x1, y1) = cordinateXY pos a in moveto x1 y1;
  {x = (float_of_int x1); y = (float_of_int y1); a = pos.a};;

(*push pos to Stack *)
let pushToStack pos = push pos stackOfPos; pos


(*remove pos from Stack *)
let popStack pos =
  let pos = pop stackOfPos in
  moveto (int_of_float pos.x) (int_of_float pos.y); pos

let depassement ()=
  let x0,y0 = current_point() in
  let x1 = size_x () in
  let y1 = size_y () in
  if (x0 < 0 || x0 > x1) || (y0 < 0 || y0 > y1) then
  raise Erreur_out

(** true if the min and the max is out of window **)
let minMaxOut = (fun min max b ->
    (min < 0) && (max > b))



(* Interpret Turtle command to graphics command *)
let rec turtleToGraphics command pos =
  match command with
  | [] -> ()
  | Line a :: l-> turtleToGraphics l  (draw_line pos (float_of_int a))
  | Move a :: l->  turtleToGraphics l (move_point pos (float_of_int a))
  | Turn ang :: l-> turtleToGraphics l
  ({x = pos.x; y = pos.y; a = pos.a + ang})
  | Store :: l->  turtleToGraphics l (pushToStack pos)
  | Restore :: l-> turtleToGraphics l (popStack pos)



