module assignment1
(*
Write a function sqr:int->int so that sqr x returns x^2
*)

let sqr x =  x*x;;

(*
Write a function pow : float -> float -> float so that pow x n returns x n.You can use the library function: System.Math.Pow
*)

let pow x n = System.Math.Pow(x,n);;

(*
Exercise 1.1 from the book - Declare a function g: int -> int, where g(n) = n + 4.
*)

let g n = n + 4;;


(*
Exercise 1.2 from the book
*)

let h (x,y) = System.Math.Sqrt((x*x) + (y*y));;


(*
Exercise 1.4 from the book  
Declare a recursive function
State the recursion formula corresponding to the declaration.
Give an evaluation for f(4).
*)

let rec fact = function
| 0 -> 1
| n -> n + fact(n-1);;

let evaluation = fact(4);;

    (*
        Recursion formula
        0 = 1                     (clause 1)
        n = n + (n-1)  n > 0      (clause 2)
    *)




(*
Exercise 1.5 from the book 
Declare an F# function to compute Fn. Use a declaration with three clauses, where the patterns
correspond to the three cases of the above definition.
Give an evaluations for F4
*)
let rec fibo = function
| 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo(n-2);;

let evaulation = fibo(4);;


(*
Exercise 1.6 from the book 
Declare a recursive function sum: int * int -> int where 
sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
for m ≥ 0 and n ≥ 0. (Hint: use two clauses with (m,0) and (m,n) as patterns.)
Give the recursion formula corresponding to the declaration.
*)

let rec sum = function
|(m,0) -> 1
|(m,n) -> (m + sum(m,(n-1))) + (m+n);;

    (* 
        Recursion formula
        |(m,0) -> 1                             for  m >= 0  &  n = 0 (clause 1)
        |(m,n) -> (m + sum(m,(n-1))) + (m+n)    for  m >= 0  &  n > 0 (clause 2)
    *)


 (*
Exercise 1.7 from the book 
Determine a type for each of the expressions:
(System.Math.PI, fact -1)
fact(fact 4)
power(System.Math.PI, fact 2)
(power, fact) 
*)

   //(System.Math.PI, fact -1)  --> is of type float * int
   //fact(fact 4) --> is of type int
   //power(System.Math.PI, fact 2) --> is of type float
   //(power, fact)  --> is of type int * int



(*
Exercise 1.8 from the book 
Consider the declarations:
let a = 5;;
let f a = a + 1;;
let g b = (f b) + a;;
Find the environment obtained from these declarations and write the evaluations of the expressions f 3 and g 3.
*)
    
    //       __                   __
    //       | a    |--> 5         |
    // env = | f(a) |--> a + 1     |
    //       | g(b) |--> f(b) + a  |
    //       __                   __

   
   
(*
Write a function dup:string->string that concatenates a string with itself.
You can either use + or ˆ. For example:
val dup : string -> string
> dup "Hi ";;
val it : string = "Hi Hi "

*)

let dup s : string = s + s ;;
dup("hi");;


(*
Exercise 1.11 Write a function 
dupn:string->int->string so that dupn s n 
creates the concatenation of n copies of s. For example:
*)

let rec dupn s n = 
match (s,n) with
|(s,0) -> ""
|(s,n) -> s + dupn s (n-1);;


(*
Exercise 1.12
Assume the time of day is represented as a pair (hh, mm):int*int.
Write a function timediff:int*int->int*int->int so that timediff t1 t2 computes the difference
in minutes between t1 and t2, i.e., t2-t1. A few examples:
*)
let timeOfDayinMin  = function
| (0,mm) -> mm
| (hh,mm) -> (hh*60) + mm;;

let timeDiff (h1,m1) (h2,m2) = timeOfDayinMin(h2,m2) - timeOfDayinMin(h1,m1);;
timeDiff (10,30) (11,30);;
timeDiff (0,30) (1,30);;

(*
Exercise 1.13 Write a function minutes:int*int->int to compute the number of minutes since midnight.
Easily done using the function timediff. A few examples:

val minutes : int * int -> int
> minutes (14,24);;
val it : int = 864
> minutes (23,1);;
val it : int = 1381
*)
let minutes (h,m) = timeDiff(0,0) (h,m);; 
minutes (14,24);;
minutes (23,1);;


(*
Declare an F# function pow: string * int -> string, where:
pow(s,n) = s · s ····· · s
           \---- n -----/
where we use · to denote string concatenation. (The F# representation is +.)
*)


let rec pow1 (s,n) =
    match (s,n) with
    |(s,0) -> ""
    |(s,n) -> s + pow1 (s,n-1);;

pow1("hello ",2);;

(*
Exercise 1.15 Solve HR, exercise 2.8
The following figure gives the first part of Pascal’s triangle:
        1
        11
        121
        1331
        14641
              
The entries of the triangle are called binomial coefficients. The k’th binomial coefficient of the n’th row is denoted
(n/k), for n >= 0 and 0 <= k <= n. For example (2/1)=2 and (4/2) = 6. The first and last binomial coefficients, that is,
(n/0) and (n/n), of row n are both 1. A binomial coefficient inside a row is the sum of the two binomial coefficients immediately above it. These properties
can be expressed as follows:  

*)

let rec bin (n,k) = 
    match (n,k) with 
    | (n,0) -> 1
    | (n,k) when n=k -> 1
    | (n,k) when n> k -> bin(n-1,k-1) + bin(n-1,k)
    | (n,k) -> 0;;

bin(1,1);;
bin(20,10);;
bin(1,2);; //n < k which is set to return 0

(*
Exercise 1.16 Solve HR, exercise 2.9

Consider the declaration:
    let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y);;
1. Determine the type of f.
2. For which arguments does the evaluation of f terminate?
3. Write the evaluation steps for f(2,3).
4. What is the mathematical meaning of f(x, y)?

*)


// 1) (int*(int*int))
// 2) for all integer x and y such that x > 0 and y is any integer the function will terminate.
// 3)
    (*
      f(2,3) --> f(2-1,2*3)
      f(1,6) --> f(1-1,6*1) 
      f(0,6) --> 6
    *)

// 4) it is a mathematical function that corresponds to x! * y


(*
Exercise 1.17 Solve HR, exercise 2.10
Consider the following declaration:
let test(c,e) = if c then e else 0;;
1. What is the type of test?
2. What is the result of evaluating test(false, fact(-1))?
3. Compare this with the result of evaluating
if false then fact -1 else 0

*)

//1) bool * int 
//2) the result is 0 because c is false
//3) This is the opposite and will thus return fact -1 because c is false and input is false thus it is true.

(*
Exercise 1.18 Solve HR, exercise 2.13
The functions curry and uncurry of types
curry : (’a * ’b -> ’c) -> ’a -> ’b -> ’c
uncurry : (’a -> ’b -> ’c) -> ’a * ’b -> ’c
are defined in the following way:
curry f is the function g where g x is the function h where h y = f(x, y).
uncurry g is the function f where f(x, y) is the value h y for the function h = g x.
Write declarations of curry and uncurry.
*)

//curry takes 3 functions 
let curry f x y = f(x,y);;
//Uncurry takes function with tuple
let uncurry f (x,y) = f x y;;