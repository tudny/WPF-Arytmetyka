
(* typ 'wartosc' opisana w treści zadania *)
(* zawiera dwa konstruktory *)
(* Przedzial (a, b) - przechowuje informacje o zbiorze [a, b] *)
(* Dopelnienie (x, y) - przechowuje informacje o zbiorze [-inf, x] u [y, inf] *)

type wartosc = Przedzial of float * float |
               Dopelnienie of float * float;;


let pusty = Przedzial(nan, nan);;
let pelny = Przedzial(neg_infinity, infinity);;

(* ****************** *)
(*    KONSTRUKTORY    *)
(* ****************** *)

let wartosc_dokladnosc x p =
    let dokladnosc = abs_float (x *. p /. 100.) in
    Przedzial(x -. dokladnosc, x +. dokladnosc);;

let wartosc_od_do x y =
    Przedzial(x, y);;

let wartosc_dokladna x =
    wartosc_dokladnosc x 0.;;


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
    | Dopelnienie(_, _) -> neg_infinity;;

let max_wartosc w =
    match w with
    | Przedzial(a, b) -> b
    | Dopelnienie(_, _) -> infinity;;

let sr_wartosc w =
    ((min_wartosc w) +. (max_wartosc w)) /. 2.;;


(* ****************** *)
(*    MODYFIKATORY    *)
(* ****************** *)

let normalize w =
    match w with
    | Dopelnienie(a, b) when a > b -> pelny
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
    Przedzial((extremum_in_list kandydaci (min_num)), (extremum_in_list kandydaci (max_num)));;

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
    | Przedzial(a, b) -> Przedzial(~-.b, ~-.a)
    | Dopelnienie(a, b) -> Dopelnienie(~-.b, ~-.a);;

let minus x y = plus x (przeciwna y);;
