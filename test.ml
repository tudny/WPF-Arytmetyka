
#use "arytmetyka.ml";;

(* Tests for Arytmetyka task                 *)
(* Performance is not tested                 *)
(* Licensed under GNU General Public License *)
(* Copyright (C) 2017 Mateusz Gienieczko     *)

let a = wartosc_od_do 3. 7.;;                (* [3., 7.]                      *)

assert(min_wartosc a = 3.0);;
assert(max_wartosc a = 7.0);;
assert(in_wartosc a 4.);;
assert(not (in_wartosc a 2.));;

let b = wartosc_od_do (-2.) 5.;;             (* [-2., 5.]                     *)

assert(sr_wartosc b = 1.5);;
assert(min_wartosc b = -2.);;
assert(max_wartosc b = 5.);;
assert(in_wartosc b (-0.));;

let c = podzielic a b;;                      (* [-inf, -1.5] U [0.6, inf]     *)

assert(not (in_wartosc c 0.));;
assert(in_wartosc c 100.);;

let d = podzielic c b;;                      (* [-inf, -0.3] U [0.12, inf]    *)

assert(compare (sr_wartosc d) nan = 0);;
assert(in_wartosc d (-3.01 /. 10.));;
assert(not (in_wartosc d (-3. /. 10.000000001)));;
assert(max_wartosc d = infinity);;
assert(min_wartosc d = neg_infinity);;

let e = plus d (wartosc_dokladna 2.);;       (* [-inf, 1.7] U [2.12, inf]     *)

assert(in_wartosc e 0.);;
assert(in_wartosc e 1.7);;
assert(in_wartosc e 2.12);;
assert(not (in_wartosc e 1.700000000001));;
assert(not (in_wartosc e 2.119999999999));;

let f = razy d b;;                           (* [-inf, inf]                   *)

assert(in_wartosc f 1000000.231232333);;
assert(in_wartosc f (-3.14159));;
assert(min_wartosc f = neg_infinity);;
assert(max_wartosc f = infinity);;
assert(compare (sr_wartosc f) nan = 0);;
assert(in_wartosc f (-0.2));;
assert(in_wartosc f (0.11));;
assert(in_wartosc f 0.0);;
assert(in_wartosc f (-0.0));;

let g = minus d (wartosc_dokladna 3.);;      (* [-inf, -3.3] U [-2.88, inf]   *)

assert(compare (sr_wartosc g) nan = 0);;
assert(in_wartosc g (-4.));;
assert(not (in_wartosc g (-3.)));;
assert(in_wartosc g (-2.));;

let h = wartosc_dokladna 0.;;                (* [0., 0.]                      *)
let i = wartosc_dokladna (-0.);;             (* [0., 0.]                      *)

assert((min_wartosc h) = (min_wartosc i));;
assert((max_wartosc h) = (max_wartosc i));;
assert((sr_wartosc h) = (sr_wartosc i));;
assert((min_wartosc h) = 0.);;
assert((max_wartosc h) = 0.);;
assert((sr_wartosc h) = 0.);;


let j = podzielic f i;;                      (* empty set                     *)
let k = podzielic b h;;                      (* empty set                     *)

assert(compare (min_wartosc j) (min_wartosc k) = 0);;
assert(compare (max_wartosc j) (max_wartosc k) = 0);;
assert(compare (min_wartosc j) nan = 0);;
assert(compare (max_wartosc j) nan = 0);;
assert(compare (sr_wartosc j) (sr_wartosc k) = 0);;
assert(compare (sr_wartosc j) nan = 0);;

let l = razy g (wartosc_dokladna (1.1));;    (* [-inf, -3.63] U [-3.168, inf] *)

assert(compare (sr_wartosc l) nan = 0);;
assert(in_wartosc l (-3.63));;
assert(not (in_wartosc l (-3.62)));;
assert(not (in_wartosc l (-3.169)));;
assert(in_wartosc l (-3.168));;
assert(in_wartosc l 0.0);;















let a = in_wartosc ( minus ( wartosc_dokladnosc (-10.000000) (0.000000) ) ( wartosc_od_do (-10.000000) (-2.000000) ) ) (-9.000000);;
assert (a = false);;
let a = min_wartosc ( razy ( podzielic ( podzielic ( wartosc_dokladna (2.000000) ) ( razy ( wartosc_dokladnosc (0.000000) (1.000000) ) ( wartosc_dokladna (6.000000) ) ) ) ( minus ( wartosc_od_do (-6.000000) (5.000000) ) ( plus ( wartosc_dokladnosc (0.000000) (9.000000) ) ( wartosc_od_do (-5.000000) (9.000000) ) ) ) ) ( wartosc_od_do (1.000000) (8.000000) ) ) ;;
assert ((classify_float a) == FP_nan);;
let a = min_wartosc ( minus ( wartosc_dokladnosc (7.000000) (8.000000) ) ( podzielic ( minus ( wartosc_od_do (0.000000) (0.000000) ) ( plus ( wartosc_od_do (0.000000) (0.000000) ) ( wartosc_dokladna (3.000000) ) ) ) ( wartosc_od_do (-4.000000) (4.000000) ) ) ) ;;
assert (a = neg_infinity);;
let a = in_wartosc ( razy ( wartosc_od_do (-4.000000) (0.000000) ) ( razy ( wartosc_dokladna (-8.000000) ) ( wartosc_dokladna (-1.000000) ) ) ) (0.000000);;
assert (a = true);;
let a = sr_wartosc ( razy ( wartosc_dokladnosc (4.000000) (0.000000) ) ( wartosc_od_do (-1.000000) (0.000000) ) ) ;;
assert (a = -2.);;
let a = max_wartosc ( plus ( wartosc_dokladnosc (-3.000000) (5.000000) ) ( wartosc_od_do (-9.000000) (9.000000) ) ) ;;
assert (a = 6.15);;
let a = in_wartosc ( podzielic ( wartosc_dokladna (9.000000) ) ( wartosc_dokladnosc (-5.000000) (9.000000) ) ) (-10.000000);;
assert (a = false);;
let a = sr_wartosc ( plus ( minus ( wartosc_dokladnosc (-2.000000) (2.000000) ) ( wartosc_dokladna (6.000000) ) ) ( wartosc_dokladna (3.000000) ) ) ;;
assert (a = -5.);;
let a = min_wartosc ( podzielic ( wartosc_dokladnosc (4.000000) (6.000000) ) ( wartosc_dokladnosc (7.000000) (2.000000) ) ) ;;
assert (a = 0.526610644257703098);;
let a = in_wartosc ( razy ( wartosc_od_do (0.000000) (0.000000) ) ( wartosc_od_do (4.000000) (4.000000) ) ) (0.000000);;
assert (a = true);;
let a = min_wartosc ( podzielic ( wartosc_dokladna (3.000000) ) ( wartosc_od_do (-1.000000) (0.000000) ) ) ;;
assert (a = neg_infinity);;

let jeden = wartosc_dokladna 1.;;

let a = wartosc_od_do (-1.) 1.;;

let a = podzielic jeden a;;  (* [-∞,-1]•[1,+∞] *)

let b = plus jeden a;;  (* [-∞,0]*[2,+∞] *)

let c = razy a b;; (* [-∞,+∞] *)

assert (min_wartosc c = neg_infinity);;




let n = wartosc_dokladna 9.;;
let m = wartosc_dokladnosc (-5.) (9.);;
let m_o = odwrotnosc m;;
let p = podzielic n m;;


print_endline "All tests OK.";;
