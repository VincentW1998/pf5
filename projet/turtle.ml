open Stack
open Graphics

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


(*The current position*)
let currentPos = 
  let (x0, y0) = current_point() in 
    {
      x = float_of_int x0;
      y = float_of_int y0;
      a = 0;
    }

(* Polar cordinate to cartesian for axe X*)
let cordinateX p length = 
  int_of_float (p.x +. (length  *. cos (float_of_int p.a)))

(* Polar cordinate to cartesian for axe Y*)
let cordinateY p length = 
  int_of_float (p.y +. (length *. sin (float_of_int p.a)))

(* Polar cordinate to cartesian for axe X and Y*)
let cordinateXY p length =
  let x1 = cordinateX p length and
    y1 = cordinateY p length in
    (x1, y1);;

(* draw line with Graphics.lineto*)
let draw_line pos taille = 
  let (x1, y1)  = cordinateXY pos taille in  ();
  {x = (float_of_int x1); y = (float_of_int y1); a = pos.a};;

(* move the current point *)
let move_point pos a =
  let (x1, y1) = cordinateXY pos a in moveto x1 y1;
  {x = (float_of_int x1); y = (float_of_int y1); a = pos.a};;

let pushToStack pos = push pos stackOfPos; pos

let popStack pos = pop stackOfPos


(* Interpret Turtle command to graphics command*)
let turtleToGraphics command pos = match command with
  | Line a ->  draw_line pos (float_of_int a)
  | Move a ->  move_point pos (float_of_int a)
  | Turn ang ->  {x = pos.x; y = pos.y; a = pos.a + ang}
  | Store -> pushToStack pos
  | Restore -> popStack pos






