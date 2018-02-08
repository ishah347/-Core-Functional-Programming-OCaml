(*
			 CS 51 Problem Set 1
		Core Functional Programming -- Testing
			     Spring 2017
 *)			     

open Ps1 ;;

let () = assert ((reversed []) = true);;
let () = assert ((reversed [1]) = true);;
let () = assert ((reversed [1; 2; 3]) = false);;
let () = assert ((reversed [3; 2; 1]) = true);;

let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
let () = assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
let () = assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-1]) = [-1]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;

let () = assert ((unzip []) = ([],[]));;
let () = assert ((unzip [(6,2);(2,4);(5,6)]) = ([6;2;5],[2;4;6]));;
let () = assert ((unzip [(1,1);(2,2);(3,3)]) = ([1;2;3],[1;2;3]));;

let () = assert ((variance []) = None);;
let () = assert ((variance [1.0]) = None);;
let () = assert ((variance [1.0; 2.0; 3.0; 4.0; 5.0]) = Some 2.5);;

let () = assert ((few_divisors 19 3) = true);;
let () = assert ((few_divisors 10 3) = false);;
let () = assert ((few_divisors 10 4) = true);;

let () = assert ((concat_list "" []) = "");;
let () = assert ((concat_list ", " []) = "");;
let () = assert ((concat_list "" ["a"; "b"; "c"]) = "abc");;
let () = assert ((concat_list "..." ["a"]) = "a");;
let () = assert ((concat_list "..." ["Moo"; "Baa"; "La"]) = "Moo...Baa...La");;
let () = assert ((concat_list ", " ["Moo"; "Baa"; "La"]) = "Moo, Baa, La");;
let () = assert ((concat_list "," ["Moo"; "Baa"; "La"]) = "Moo,Baa,La");;

let () = assert ((to_run_length []) = []);;
let () = assert ((to_run_length ['a'; 'a'; 'a'; 'b'; 'a'; 'd'; 'd']) = 
            [(3, 'a'); (1, 'b'); (1, 'a'); (2, 'd')]);;
let () = assert ((to_run_length ['a'; 'a'; 'a'; 'b'; 'd'; 'd']) = 
            [(3, 'a'); (1, 'b'); (2, 'd')]);;
let () = assert ((to_run_length ['a']) = [(1, 'a')]);;

let () = assert ((from_run_length []) = []);;
let () = assert ((from_run_length [(3, 'a'); (1, 'b'); (1, 'a'); (2, 'd')]) = 
            ['a'; 'a'; 'a'; 'b'; 'a'; 'd'; 'd']);;
let () = assert ((from_run_length [(3, 'a'); (1, 'b'); (2, 'd')]) = 
            ['a'; 'a'; 'a'; 'b'; 'd'; 'd']);;
let () = assert ((from_run_length [(1, 'a')]) = ['a']);;





(*
(* sample tests *)
let () = assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]);;
let () = assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]);;
let () = assert ((merge [1;2] [1;2]) = [1;1;2;2]);;
let () = assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]);;
let () = assert ((merge [] []) = []);;
let () = assert ((merge [1] []) = [1]);;
let () = assert ((merge [] [-1]) = [-1]);;
let () = assert ((merge [1] [-1]) = [-1;1]);;
*)
