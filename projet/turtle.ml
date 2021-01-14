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

(* initiate the stack of min value*)
let initStackMin stack min =
  if is_empty stack || (top stack > min) then push min stack

let initStackMax stack max =
  if is_empty stack || (top stack < max) then push max stack

let initStack x1 y1 =
  initStackMin stackXmin x1;
  initStackMin stackYmin y1;
  initStackMax stackXmax x1;
  initStackMax stackYmax y1;;


(** fake lineto **)
let drawFake pos taille =
  let (x1, y1) = cordinateXY pos taille in
    initStack x1 y1;
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

(** true if the min and the max is out of window **)
let minMaxOut = (fun min max b ->
    (min < 0) && (max > b))

let minOut = (fun min ->
    (min < 0))

(** add base to and last position, xmin or ymin**)
let delta = (fun min pos base -> float_of_int(abs (min)) +. pos +. base)

(*base = 60. si x ou y < 0 sinon base = ?*)
let setPosX x1 pos base stack =
  let x2 = delta x1 pos.x base in
  clear stack;
    {x = x2; y = pos.y; a = pos.a}

(*base = 60. si x ou y < 0 sinon base = ?*)
let setPosY y1 pos base stack=
  let y2 = delta y1 pos.y base in
  clear stack;
    {x = pos.y; y = y2; a = pos.a}

let origine pos =
  let xmin = top stackXmin in
  let ymin = top stackYmin in
  let pos1 = if xmin < 0 then setPosX xmin pos 60. stackXmin else pos in
  if ymin < 0 then setPosY ymin pos 60. stackYmin else pos1

let newFact pos facteur =
  let xmin = top stackXmin in
  let ymin = top stackYmin in
  let xadd = if xmin < 0 then int_of_float (delta xmin pos.y 60.) else 0 in
  let yadd = if ymin < 0 then int_of_float (delta ymin pos.x 60.) else 0 in
  let xmax = top stackXmax in
  let ymax = top stackYmax in
    clear stackYmax;
    clear stackXmax;
    if (ymax + yadd > 540) || (xmax + xadd > 540) then
    facteur *. (1./.1.5) else facteur

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

let getNewPosFacteur lcmd pos facteur=
  firstPass lcmd (move_point pos 0.) facteur;
  (origine pos, newFact pos facteur)

