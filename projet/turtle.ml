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

let stackXmin = create()

let stackXmax = create()

let stackYmin = create()

let stackYmax = create()


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

(** stack xMin**)
let initXmin x1 =
  if is_empty stackXmin || (top stackXmin > x1) then push x1 stackXmin

(** fake lineto **)
let drawFake pos taille =
  let (x1, y1) = cordinateXY pos taille in
  initXmin x1;
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
  (x0 < 0 || x0 > x1) || (y0 < 0 || y0 > y1)

(** true if the min and the max is out of window **)
let minMaxOut = (fun min max b ->
    (min < 0) && (max > b))

let minOut = (fun min ->
    (min < 0))

let delta = (fun min -> abs (min) + 120)

let getCurrentPos () =
  let x0, y0 = current_point() in
  {x = (float_of_int x0); y = (float_of_int y0); a = 90}

let setPosX x1 =
  let x2 = delta x1 in
    clear stackXmin;
    {x = (float_of_int x2); y = 60.; a = 90}

let origine () =
  let pos = {x = 60.; y = 60.; a = 90} in
  let x1 = top stackXmin in
  if x1 < 0 then setPosX x1 else pos


(** first analysis of the lsystem**)
let rec firstPass command pos facteur =
  match command with
  | [] -> ()
  | Line a :: l -> firstPass l (drawFake pos ((float_of_int a) *. facteur)) facteur
  | Move a :: l -> firstPass l (move_point pos ((float_of_int a) *. facteur)) facteur
  | Turn ang :: l -> firstPass l
  ({x = pos.x; y = pos.y; a = pos.a + ang}) facteur
  | Store :: l->  firstPass l (pushToStack pos) facteur
  | Restore :: l-> firstPass l (popStack pos) facteur


(* Interpret Turtle command to graphics command *)
let rec turtleToGraphics command pos facteur =
  match command with
  | [] -> ()
  | Line a :: l-> turtleToGraphics l  (draw_line pos ((float_of_int a) *. facteur)) facteur
  | Move a :: l->  turtleToGraphics l (move_point pos ((float_of_int a) *. facteur)) facteur
  | Turn ang :: l-> turtleToGraphics l
  ({x = pos.x; y = pos.y; a = pos.a + ang}) facteur
  | Store :: l->  turtleToGraphics l (pushToStack pos) facteur
  | Restore :: l-> turtleToGraphics l (popStack pos) facteur



