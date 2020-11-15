
(* typ 'wartosc' opisana w treści zadania *)
(* zawiera dwa konstruktory *)
(* Przedzial (a, b) - przechowuje informacje o zbiorze [a, b] *)
(* Dopelnienie (x, y) - przechowuje informacje o zbiorze [-inf, x] u [y, inf] *)

type wartosc = Przedzial of float * float |
               Dopelnienie of float * float;;

let is_nan x = compare x nan = 0

let is_pusty w =
  match w with
  | Przedzial(a, b) -> (is_nan a) || (is_nan b)
  | Dopelnienie(a, b) -> (a = neg_infinity) && (b = infinity)

(* ****************** *)
(*    KONSTRUKTORY    *)
(* ****************** *)

let wartosc_dokladnosc x p =
    let dokladnosc = abs_float (x *. p /. 100.) in
    Przedzial(x -. dokladnosc, x +. dokladnosc)

let wartosc_od_do x y =
    Przedzial(x, y)

let wartosc_dokladna x =
    wartosc_dokladnosc x 0.

let zero = wartosc_dokladna 0.

(* *************** *)
(*    SELEKTORY    *)
(* *************** *)

let in_wartosc w x =
    match w with
    | Przedzial(a, b)   -> (a <= x && x <= b)
    | Dopelnienie(a, b) -> (x <= a || b <= x)

let min_wartosc w =
    match w with
    | Przedzial(a, b) -> a
    | Dopelnienie(_, _) -> neg_infinity

let max_wartosc w =
    match w with
    | Przedzial(a, b) -> b
    | Dopelnienie(_, _) -> infinity

let sr_wartosc w =
    ((min_wartosc w) +. (max_wartosc w)) /. 2.


(* ****************** *)
(*    MODYFIKATORY    *)
(* ****************** *)

(* DEBUG *)
let rec print_list_of_floats li =
  match li with
  | [] -> print_endline ""
  | (h :: t) -> print_float h; print_string " "; print_list_of_floats t;;

(* DEBUG *)
let print_wartosc w =
  match w with
  | Przedzial(a, b) -> print_string "Przedzial: "; print_float a; print_string " "; print_float b; print_endline "";
  | Dopelnienie(a, b) -> print_string "Dopelnienie: "; print_float a; print_string " "; print_float b; print_endline ""

let normalize w =
    match w with
    | Dopelnienie(a, b) when a >= b -> pelny
    | Przedzial(a, b) when a = -.0. -> Przedzial(0., b)
    | Przedzial(a, b) when b = 0. -> Przedzial(a, -.0.)
    | _ -> w

let min_num x y =
    if x < y then x
    else y;;

let max_num x y =
    if x > y then x
    else y;;

let rec extremum_in_list li f =
    match li with
    | []  -> nan
    | [x] -> x
    | h :: t -> f h (extremum_in_list t f);;

let add_p_p (Przedzial(a, b)) (Przedzial(c, d)) =
    let kandydaci = (a +. c) :: (a +. d) :: (b +. c) :: (b +. d) :: [] in
    normalize (Przedzial((extremum_in_list kandydaci (min_num)), (extremum_in_list kandydaci (max_num))));;

let add_d_d (Dopelnienie(a, b)) (Dopelnienie(c, d)) =
    pelny;;

let add_d_p (Dopelnienie(a, b)) (Przedzial(c, d)) =
    normalize (Dopelnienie(a +. d, b +. c));;

let plus x y =
    match (x, y) with
    | (Przedzial(_, _), Przedzial(_, _))     -> add_p_p x y
    | (Dopelnienie(_, _), Dopelnienie(_, _)) -> add_d_d x y
    | (Dopelnienie(_, _), Przedzial(_, _))   -> add_d_p x y
    | (Przedzial(_, _), Dopelnienie(_, _))   -> add_d_p y x;;

let przeciwna w =
  match w with
    | Przedzial(a, b) -> normalize(Przedzial(~-.b, ~-.a))
    | Dopelnienie(a, b) -> normalize(Dopelnienie(~-.b, ~-.a));;

let minus x y = plus x (przeciwna y);;

let znak a =
  match a with
  | 0. -> 0.
  | a -> a /. abs_float(a);;

let czy_ten_sam_znak a b =
  ((znak a) *. (znak b)) > 0.;;

let czy_zawiera_zero w =
  match w with
  | Przedzial(a, b) -> not (czy_ten_sam_znak a b)
  | Dopelnienie(a, b) -> czy_ten_sam_znak a b

let czy_pusty w =
  match w with
  | Przedzial(a, b) -> (is_nan a) || (is_nan b)
  | Dopelnienie(a, b) -> (a = neg_infinity) && (b = infinity);;

let multi_p_p (Przedzial(a, b)) (Przedzial(c, d)) =
  let kandydaci = (a *. c) :: (a *. d) :: (b *. c) :: (b *. d) :: [] in
  normalize (Przedzial((extremum_in_list kandydaci (min_num)), (extremum_in_list kandydaci (max_num))));;

let multi_d_d (Dopelnienie(a, b) as x) (Dopelnienie(c, d) as y) =
  if czy_zawiera_zero x || czy_zawiera_zero y then
    pelny
  else
    normalize (Dopelnienie((max_num (a *. d) (b *. c)), (min_num (a *. c) (b *. d))));;

let multi_d_p (Dopelnienie(a, b) as x) (Przedzial(c, d) as y) =
  if czy_zawiera_zero x || czy_zawiera_zero y then
    pelny
  else
    let kandydaci = (a *. b) :: (a *. c) :: (b *. c) :: (b *. d) :: [] in
    let maks_ujemny =
      List.fold_left (fun acc x -> if x < 0. then (max_num acc x) else acc) neg_infinity kandydaci in
    let min_dodatni =
      List.fold_left (fun acc x -> if x > 0. then (min_num acc x) else acc) infinity kandydaci in
    normalize (Dopelnienie(maks_ujemny, min_dodatni))

let rec razy x y =
  match (x, y) with
  | (Przedzial(0., 0.), Przedzial(0., 0.)) -> Przedzial(0., 0.)
  | (Przedzial(0., 0.), _) -> razy y x
  | (_, Przedzial(0., 0.)) ->
    if czy_pusty x then Przedzial(nan, nan) else Przedzial(0., 0.)
  | (Przedzial(_, _), Przedzial(_, _)) -> multi_p_p x y
  | (Dopelnienie(_, _), Dopelnienie(_, _)) -> multi_d_d x y
  | (Przedzial(_, _), Dopelnienie(_, _)) -> multi_d_p y x
  | (Dopelnienie(_, _), Przedzial(_, _)) -> multi_d_p x y

let odwrotnosc w =
  match w with
  | Przedzial(a, b) when (a = 0. && b = 0.) -> Przedzial(nan, nan)
  | Przedzial(a, b) ->
    if czy_zawiera_zero w then
      if b = 0. then normalize (Przedzial(neg_infinity, 1. /. a))
      else if a = 0. then normalize (Przedzial(1. /. b, infinity))
      else normalize (Dopelnienie(1. /. a, 1. /. b))
    else
      Przedzial(1. /. b, 1. /. a)
  | Dopelnienie(a, b) when (a = neg_infinity && b = infinity) -> Przedzial(nan, nan)
  | Dopelnienie(a, b) ->
    if czy_zawiera_zero w then
      normalize (Przedzial(1. /. a, 1. /. b))
    else
      normalize (Dopelnienie(1. /. b, 1. /. a));;


let podzielic x y = razy x (odwrotnosc y);;


(* Mover *)
