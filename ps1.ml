(* 
                         CS 51 Problem Set 1
                     Core Functional Programming
                             Spring 2018
 *)

(*======================================================================
Problem 1: Dealing with types

........................................................................
For each of the expressions below (prob0, prob1a, etc.), figure out
the type of the expression. Then write the type in as a string as the
value for prob0_answer, prob1a_answer, etc. The first one, prob0, is
done for you.

  prob0        42
  prob1a       let greet y = "Hello " ^ y in greet "World!"
  prob1b       [Some 4; Some 2; None; Some 3]
  prob1c       ((None, Some 42.0), true)

......................................................................*)

let prob0_answer = "int" ;;

let prob1a_answer = "string" ;;

let prob1b_answer = "int option list" ;;

let prob1c_answer = "('a option * float option) * bool" ;;

(*......................................................................
There are several values defined below that do not type check. 

Explain in a comment above each corresponding value why the following
definitions will not type check, and then provide a fixed version of 
each definition as an OCaml value (outside of a comment). Your fix should
change the code minimally.

(Note that the variable names begin with underscore (_) to disable the
warning noting that the values are otherwise unused. You'll want to
leave the underscores in as well.)
......................................................................*)

(*
The type "string * int list" implies that _prob1d is a tuple of two arguments,
one that is an integer and one that is a list of integers. In reality, 
_prob1d, from its appearance, seems to be a list of tuples of two arguments,
one of type integer, the other of type string, and so, a set of parentheses
is required around "string * int" to allow OCaml to understand this.

let _prob1d : string * int list = [("CS", 51); ("CS", 50)] ;;
*)
  
let _prob1d : (string * int) list = [("CS", 51); ("CS", 50)] ;;

(*
Due to the use of the operation "+" and not "+." while defining add, OCaml
expects both values inputted into add to be integers. 3.9 is a float value. 

let _prob1e : int =
  let add (x, y) = x + y in
  if add (4, 3.9) = 10 then 4 else 2 ;;
*)

let _prob1e : int =
  let add (x, y) = x + y in
  if add (4, 4) = 10 then 4 else 2 ;;

(*
_prob1f is not a list of tuples of two arguments that are both of type string.
Instead, it is a list of tuples of two arguments, the first of which is a 
string, while the second appears to be either an 'a option or an integer. To 
make the second argument in each tuple consistent, they should all be
converted to type int option. 

let _prob1f : (string * string) list =
  [("January", None); ("February", 1); ("March", None); 
   ("April", None); ("May", None); ("June", 1); 
   ("July", None); ("August", None); ("September", 3);
   ("October", 1); ("November", 2); ("December", 3)] ;;
*)

let _prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); 
   ("April", None); ("May", None); ("June", Some 1); 
   ("July", None); ("August", None); ("September", Some 3);
   ("October", Some 1); ("November", Some 2); ("December", Some 3)] ;;

(*======================================================================
Problem 2 - Writing functions

........................................................................
For each subproblem, you must implement a given function, providing
appropriate unit tests in the accompanying file pset1_tests.ml. You
are provided a high level description as well as a type signature of
the function you must implement. Keep in mind the CS51 style guide and
what you've learned so far about efficiency and elegance. You are
*not* allowed to use library functions (i.e., the List module) for
*this* problem unless you implement the functionality yourself.
......................................................................*)

(*......................................................................
Problem 2a: The function "reversed" takes a list of integers and
returns true if the list is in nonincreasing order. The empty list is
considered to be reversed in this sense. Consecutive elements of the
same value are allowed in a reversed list.

For example:

# reversed [1;2;3] ;;
- : bool = false
# reversed [3;2;1] ;;
- : bool = true
# reversed [5;2;2;2;1;1] ;;
- : bool = true

Here is its signature: 

  reversed : int list -> bool

Replace the line below with your own definition of "reversed".
......................................................................*)

(* Checks whether or not each term in a list is >= to the previous term *)
let rec reversed (lst : int list) : bool = 
  match lst with
  | [] -> true
  | _head1 :: [] -> true
  | head1 :: head2 :: tail -> if head1 >= head2 then reversed (head2 :: tail) 
                              else false ;;

(*......................................................................
Problem 2b: The function "merge" takes two integer lists, each
*sorted* in increasing order, and returns a single merged list in
sorted order.  For example:

merge [1;3;5] [2;4;6] ;;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;2;5] [2;4;6] ;;
- : int list = [1; 2; 2; 4; 5; 6]
merge [1;3;5] [2;4;6;12] ;;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12] ;;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

Here is its signature:

  merge : int list -> int list -> int list

Replace the line below with your own definition of "merge".
......................................................................*)

(* Compares terms in two lists to place them in one list in increasing order *)
let rec merge (x : int list) (y : int list) : int list =
  match x, y with
  | _, [] -> x
  | [], _ -> y
  | head_x :: tail_x, head_y :: tail_y -> if head_x <= head_y 
                                      then (head_x :: (merge tail_x y)) 
                                      else (head_y :: (merge x tail_y)) ;;


(*......................................................................
Problem 2c: The function "unzip", given a list of integer pairs,
returns a pair of lists, the first of which contains each first
element of each pair, and the second of which contains each second
element.  The returned list should have elements in the order in which
they were provided. For example:

unzip [(6,2);(2,4);(5,6)] ;;
- : int list * int list = ([6;2;5],[2;4;6])

Here is its signature:

  unzip : (int * int) list -> int list * int list)

Replace the line below with your own definition of "unzip".
......................................................................*)

(* Seperates a list of integer pairs into two lists *)
let rec unzip (lst : (int * int) list) : int list * int list = 
  match lst with
  | [] -> [], []
  | (h_1, h_2) :: tail -> let a, b = unzip tail in (h_1 :: a), (h_2 :: b) ;;

(*......................................................................
Problem 2d: The function "variance" takes a float list and returns
None if the list has fewer than two elements. Otherwise, it should
return Some of the variance of the floats. Recall that the variance of
a sequence of numbers is given by the following equation:
                                                
        1/(n-1) * sum (x_i - m)^2

where n indicates the number of elements in the list, m is the
arithmetic mean of the list, and x_i is element in the ith index of
the list. If you want to compare your output with an online
calculator, make sure you find one that calculates the (unbiased)
sample variance.  For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0] ;;
- : float option = Some 2.5
variance [1.0] ;;
- : float option = None

Remember to use the floating point version of the arithmetic operators
when operating on floats (+., *., etc). The function "float" can
convert ("cast") an int to a float.

Here is its signature:

  variance : float list -> float option

Replace the line below with your own definition of "variance".
......................................................................*)

(* Calculates variance of a list using formula 1/(n-1) * sum (x_i - m)^2 *)
let variance (lst : float list) : float option = 
  let rec sum (lst_1 : float list) : float =
    match lst_1 with
    | [] -> 0.0
    | head :: tail -> head +. sum tail 
  in
  let rec length (lst_2 : float list) : int =
    match lst_2 with
    | [] -> 0
    | _head :: tail -> 1 + length tail
  in
  let mean (values : float list) : float = 
    match values with
    | [] -> 0.0
    | _head :: _tail -> sum values /. float (length values)
  in
  let mean_value = mean lst in
  let rec difference_squared (diff_sq : float list) : float list =
    match diff_sq with
      | [] -> []
      | hd :: tl -> 
          (hd -. mean_value) *. (hd -. mean_value) :: difference_squared tl 
  in match lst with
  | [] -> None
  | _head :: [] -> None
  | _head :: _tail -> 
      Some (sum (difference_squared lst) /. float (length lst - 1)) ;;      


(*......................................................................
Problem 2e: The function "few divisors" takes two integers, x and y, and
returns true if x has fewer than y divisors (including 1 and x). Note:
this is *not* the same as x having fewer divisors than y does. For
example: 

few_divisors 17 3 ;;
- : bool = true
few_divisors 4 3 ;;
- : bool = false
few_divisors 4 4 ;;
- : bool = true

Do not worry about zero or negative integers at all. We will not test
your code using zero or negative values for x and y. Do not consider
negative integers for divisors (i.e. -2 being a divisor for 4).

Here is its signature:

  few_divisors : int -> int -> bool 

Replace the line below with your own definition of "few_divisors".
......................................................................*)

(* Checks if a number has less than a maximum amount of divisors *)
let few_divisors (divid : int) (max : int) : bool =
  (* Counts how many divisors a number has that is < a certain value *)
  let rec count_divisors (dividend : int) (divisor : int) : int =
    let next_divisor = divisor - 1 in
      if divisor > 0 then 
        if dividend mod divisor = 0 
        then 1 + count_divisors dividend next_divisor
        else count_divisors dividend next_divisor
      else 0  
  in if count_divisors divid divid < max then true else false ;;  

(*......................................................................
Problem 2f: The function "concat_list" takes two arguments: sep, a
string, and lst, a string list. It returns one string with all the
elements of lst concatenated together but separated by the string
sep. For example:

concat_list ", " ["first"; "second"; "third"] ;;
- : string = "first, second, third"
concat_list "..." ["Moo"; "Baa"; "Lalala"] ;;
- : string = "Moo...Baa...Lalala"
concat_list ", " [] ;;
- : string = ""
concat_list ", " ["Moo"] ;;
- : string = "Moo"

Here is its signature:

  concat_list : string -> string list -> string

Replace the lines below with your own definition of "concat_list"
......................................................................*)

(* Inserts a string value between subsequent terms from a list *)
let rec concat_list (sep : string) (lst : string list) : string =
  match lst with
  | [] -> ""
  | head :: [] -> head
  | head :: tail -> head ^ sep ^ concat_list sep tail ;;

(*......................................................................
Problem 2g: One way to compress a list of characters is to use
run-length encoding. The basic idea is that whenever we have repeated
characters in a list such as

  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'd'; 'd'; 'd'; 'd'] 

we can (sometimes) represent the same information more compactly as a
list of pairs like 

  [(5, 'a'); (3, 'b'); (1, 'a'); (4, 'd')]      . 

Here, the numbers represent how many times the character is
repeated. For example, the first character in the string is 'a' and it
is repeated 5 times, followed by 3 occurrences of the character 'b',
followed by one more 'a', and finally 4 copies of 'd'.

Write a function "to_run_length" that converts a list of characters
into the run-length encoding, and then write a function
"from_run_length" that converts back. Writing both functions will make
it easier to test that you've gotten them right.

Here are their signatures:

  to_run_length : char list -> (int * char) list
  from_run_length : (int * char) list -> char list

Replace the lines below with your own definitions of "to_run_length"
and "from_run_length".
......................................................................*)

(* Converts a list of characters into run-length encoding *)
let to_run_length (lst : char list) : (int * char) list =
  (* Displays in a tuple how many times a char appears in a row in a list *)
  let rec compressor i ch lst_1 = 
    match lst_1 with
    | [] -> [(i, ch)]
    | head :: tail -> if ch != head then (i, ch) :: compressor 0 head lst_1
                      else compressor (i + 1) ch tail
  in match lst with
  | [] -> [] 
  | head :: _tail -> compressor 0 head lst ;;

(* Converts run-length encoding back into a list of characters *)
let rec from_run_length (lst : (int * char) list) : char list =
    match lst with
    | [] -> [] 
    | (h1, h2) :: tail -> if h1 > 0 
                          then h2 :: from_run_length ((h1 - 1, h2) :: tail)
                          else from_run_length tail ;;                      


(*======================================================================
Problem 3: Challenge problem: Permutations

........................................................................
The function "permutations" takes a list of integers and should
return a list containing every permutation of the list. For example:

  permutations [1; 2; 3] =
  - : int list list = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; 
  [3; 1; 2]; [3; 2; 1]]

It doesn't matter what order the permutations appear in the returned
list.  Note that if the input list is of length n, then the answer
should be of length n! (that is, the factorial of n).

Hint: One way to do this is to write an auxiliary function, interleave
: int -> int list -> int list list, that yields all interleavings of
its first argument into its second. For example:

  interleave 1 [2; 3] = 
  - : int list list = [ [1; 2; 3]; [2; 1; 3]; [2; 3; 1] ]

You may also use list module functions for this question and may find 
List.map and List.concat helpful. 

Here is the signature of permutations:

  permutations : int list -> int list list

Replace the line below with your own definition of "permutations".
......................................................................*)

let permutations = (fun _ -> failwith "permutations not implemented") ;;


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = failwith "not provided" ;;
