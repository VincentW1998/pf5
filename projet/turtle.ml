open Stack
open Graphics
open Unix
open Random

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

let cordinate p a length f =
  int_of_float p + roundFloat(length *. f(degreeToRad a))

(* Polar cordinate to cartesian for axe X and Y *)
let cordinateXY p length =
  let x1 = cordinateX p length and
    y1 = cordinateY p length in
    (x1, y1);;

(* draw line with Graphics.lineto *)
let draw_line pos taille =
let taille2 = if taille < 1. then 1. else taille in
  let (x1, y1)  = cordinateXY pos taille2 in lineto x1 y1;
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
  let taille2 = if taille < 1. then 1. else taille in
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

(** true if the min x or y is lower than 0**)
let minOut = (fun min ->
    (min < 0))

(** true if the max x or y is bigger than window**)
let maxOut = (fun max ->
    (max > size_x))

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

(** nouvelle position en fonction de la taille de la figure**)
let origine pos =
  let xmin = top stackXmin in
  let ymin = top stackYmin in
  let pos1 = if minOut xmin then setPosX xmin pos 60. stackXmin else pos in
  if minOut ymin then setPosY ymin pos 60. stackYmin else pos1

(** nouveau facteur en fonction de la taille de la figure**)
let newFact pos facteur =
  let xmin = top stackXmin in
  let ymin = top stackYmin in
  let xadd = if minOut xmin then int_of_float (delta xmin pos.y 60.) else 0 in
  let yadd = if minOut ymin then int_of_float (delta ymin pos.x 60.) else 0 in
  let xmax = top stackXmax in
  let ymax = top stackYmax in
    clear stackYmax;
    clear stackXmax;
    if (ymax + yadd > 600) || (xmax + xadd > 600) ||
    (minMaxOut xmin xmax (size_x()) ) || (minMaxOut ymin ymax (size_y())) then
    facteur *. (1./.1.05) else facteur

(** first analysis of the lsystem**)
let rec firstPass command pos facteur =
  match command with
  | [] -> ()
  | Line a :: l ->
  firstPass l (drawFake pos ((float_of_int a) *. facteur)) facteur
  | Move a :: l ->
  firstPass l (move_point pos ((float_of_int a) *. facteur)) facteur
  | Turn ang :: l -> firstPass l
  ({x = pos.x; y = pos.y; a = pos.a + ang}) facteur
  | Store :: l->  firstPass l (pushToStack pos) facteur
  | Restore :: l-> firstPass l (popStack pos) facteur

let stackCouleur = create()

let couleur = (fun r g b -> rgb r g b)

let initialiseRGB ()= if is_empty stackCouleur then
  let r = Random.int 256 in
  let g = Random.int 256 in
  let b = Random.int 256 in
  push (r,g,b) stackCouleur

let setRGB () =
  let r1,g1,b1 = top stackCouleur in
  let r2,g2,b2 =  ((r1+1 mod 255), (g1+1 mod 255), (b1+1 mod 255)) in
  push (r2,g2,b2) stackCouleur

(** change la couleur a chaque iteration**)
let change_color () =
  initialiseRGB ();
  setRGB();
  let r,g,b = top stackCouleur in
  let col = couleur r g b in
  Graphics.set_color col

(* Interpret Turtle command to graphics command *)
let rec turtleToGraphics command pos facteur n =
  Unix.sleepf (0.005 *. (1./.2. ** (float_of_int n)));
  change_color();
    match command with
  | [] -> clear stackCouleur
  | Line a :: l->
  turtleToGraphics l  (draw_line pos ((float_of_int a) *. facteur)) facteur n
  | Move a :: l->
  turtleToGraphics l (move_point pos ((float_of_int a) *. facteur)) facteur n
  | Turn ang :: l-> turtleToGraphics l
  ({x = pos.x; y = pos.y; a = pos.a + ang}) facteur n
  | Store :: l->  turtleToGraphics l (pushToStack pos) facteur n
  | Restore :: l-> turtleToGraphics l (popStack pos) facteur n

(** return (position * facteur) **)
let getNewPosFacteur lcmd pos facteur=
  firstPass lcmd (move_point pos 0.) facteur;
  (origine pos, newFact pos facteur)


