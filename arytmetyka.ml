
(* typ 'wartosc' opisana w treści zadania *)
(* zawiera dwa konstruktory *)
(* Przedzial (a, b) - przechowuje informacje o zbiorze [a, b] *)
(* Dopelnienie (x, y) - przechowuje informacje o zbiorze [-inf, x] u [y, inf] *)

type wartosc = Przedzial of float * float |
               Dopelnienie of float * float;;
			   
(* ****************** *)
(*    KONSTRUKTORY    *)
(* ****************** *)

let wartosc_dokladnosc x p =
    let dokladnosc = abs_float (x *. p /. 100.) in 
    Przedzial(x -. dokladnosc, x +. dokladnosc);;

let wartosc_od_do x y =
    Przedzial(x, y);;

let wartosc_dokladna x = 
    wartosc_od_do x x;;



