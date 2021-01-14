
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

val stackXmin : int Stack.t

val stackYmin : int Stack.t

val stackXmax : int Stack.t

val stackYmax : int Stack.t

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

val cordinate : float -> int -> float -> (float -> float) -> int

(* Polar cordinate to cartesian for axe X and Y *)
val cordinateXY : position -> float -> int * int

(* draw line with Graphics.lineto *)
val draw_line : position -> float -> position

val initStackMin : int Stack.t -> int -> unit

val initStackMax : int Stack.t -> int -> unit

val initStack : int -> int -> unit

val drawFake : position -> float -> position

(* move the current point *)
val move_point : position -> float -> position

(* push pos to Stack *)
val pushToStack : position -> position

(* remove pos from Stack *)
val popStack : 'a -> position

(** true if the min and the max is out of window **)
val minMaxOut : int -> int -> int -> bool

val minOut : int -> bool

val delta : int -> float -> float -> float

val setPosX : int -> position -> float -> int Stack.t -> position

val setPosY : int -> position -> float -> int Stack.t -> position

val origine : position -> position

val newFact : position -> float -> float

val firstPass : command list -> position -> float -> unit

(* Interpret Turtle command to graphics command *)
val turtleToGraphics : command list -> position -> float -> int -> unit

val getNewPosFacteur : command list -> position -> float -> (position * float)

