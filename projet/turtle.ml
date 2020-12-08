open Graphics
open Stack

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


let cordinateX p length = 
  p.x +. (length  *. cos (float_of_int p.a))

let cordinateY p length = 
  p.y +. (length *. sin (float_of_int p.a))

let cordinateXY p length =
  let x1 = cordinateX p length and
  y1 = cordinateY p length in
  (x1, y1);;

let draw_line pos a = 
  let (x1, y1)  = cordinateXY pos a in 
  lineto x1 y1;;


let turtleToGraphics command pos = match command with
  | Line a ->  draw_line pos (float_of_int a)
  | Store -> push pos stackOfPos
  | Restore -> pop stackOfPos

