
(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)

(** Position and angle of the turtle *)
type position = {
  x: float;        (** position x *)
  y: float;        (** position y *)
  a: int;          (** angle of the direction *)
}

(** Put here any type and function signatures concerning turtle *)

(* New stack, initially empty *)
val stackOfPos : position Stack.t

(** get the sign of x and add this sign to 0.5**)
val getSign : float -> float

(** function change degree to rad**)
val degreeToRad : int -> float

(** round the float **)
val roundFloat : float -> int

(* Polar cordinate to cartesian for axe X *)
val cordinateX : position -> float -> int

(* Polar cordinate to cartesian for axe Y *)
val cordinateY : position -> float -> int

(* Polar cordinate to cartesian for axe X and Y *)
val cordinateXY : position -> float -> int * int

(* draw line with Graphics.lineto *)
val draw_line : position -> float -> position

(* move the current point *)
val move_point : position -> float -> position

(* push pos to Stack *)
val pushToStack : position -> position

(* remove pos from Stack *)
val popStack : 'a -> position

val depassement : unit -> bool

(* Interpret Turtle command to graphics command *)
val turtleToGraphics : command list -> position -> unit


