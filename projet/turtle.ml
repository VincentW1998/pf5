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
      x = x0;
      y = y0;
      a = 0;
    }

(* Polar cordinate to cartesian for axe X*)
let cordinateX p length = 
  p.x +. (length  *. cos (float_of_int p.a))

(* Polar cordinate to cartesian for axe Y*)
let cordinateY p length = 
  p.y +. (length *. sin (float_of_int p.a))

(* Polar cordinate to cartesian for axe X and Y*)
let cordinateXY p length =
  let x1 = cordinateX p length and
    y1 = cordinateY p length in
    (x1, y1);;

(* draw line with Graphics.lineto*)
let draw_line pos a = 
  let (x1, y1)  = cordinateXY pos a in lineto x1 y1;;


(* move the current point *)
let move_point pos a = 
  let (x1, y1) = cordinateXY pos a in moveto x1 y1


(* Interpret Turtle command to graphics command*)
let turtleToGraphics command pos = match command with
  | Line a ->  draw_line pos (float_of_int a)
  | Move a ->  move_point pos (float_of_int a)
  | Turn a ->  currentPos.a + a
  | Store -> push pos stackOfPos
  | Restore -> pop stackOfPos

