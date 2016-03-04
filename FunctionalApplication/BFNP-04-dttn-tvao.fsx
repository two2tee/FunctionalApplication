//module assignment 4

(*
Exercise 4.1 - 9.1
Consider the function g declared on Page 202 and the stack and heap after the evaluation of g 2
shown in Figure 9.2. Reproduce this resulting stack and heap by a systematic application of push
and pop operations on the stack, and heap allocations that follow the step by step evaluation of g 2.

*)


(*
Exercise 4.2 HR exercise 9.3
Declare an iterative solution to exercise 1.6.

*)

(*
//old version
    let rec sum = function 
     |(m,0) -> 0
     |(m,n) -> (sum(m,n-1) + (m+n));;
     *)


 //iterativ version

 let rec sum (m,n) r =  if n<>0  //p
                        then sum(m,n-1)(r+(m+n)) // Tail recursive because the call does not hold on to intermediate results
                        else r + m;;
 

 sum (1,3) 0;;

 (*
   Exercise 4.3
   Give iterative declaration of a list function List.length
 *)

 let rec length (l : List<'a>) r = if l<>[]
                                    then length (l.Tail) r+1                                  
                                    else r

//Example
//length [1;2;3] 0;;
length ["a";"b";"c"] 0;; //returns size of 3


(*
  Exercise 4.4
  Declare a continuation-based version of the factorial function and compare the run time with
  the results in Section 9.4


*)

//Given function

let rec factA = function
| (0,m) -> m
| (n,m) -> factA(n-1,n*m);;

let xs16 = List.init 1000000 (fun i -> 16);;

#time
for i in xs16 do let _ = factA(i,1) in ();;

//Continuation function
let rec factContinuation n c =
    if n <= 0 then c 1
    else  factContinuation (n-1) (fun res -> c(res*n));;

#time
for i in xs16 do let _ = factContinuation(i) in ();;

// Result: Continuation is twice as fast

(*
Exercise 4.5
Declare a function for computing Fibonacci numbers Fn (see Exercise 1.5) using a while
loop. Hint: introduce variables to contain the two previously computed Fibonacci numbers

*)

//Old
let rec fibo = function
| 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo(n-2);;

//While loop



let fiboW n =
    let mutable x = 0
    let mutable y = 0
    let mutable m = n
    let mutable result = 1
    while m > 1 do
        m <- m-1
        x <- y
        y <- result
        result <- x+y
    result;;

fiboW 10;; //returns 55

(*
Exercise 4.6 

Develop the following three versions of functions computing Fibonacci numbers Fn (see Exercise
1.5):

    1. A version fibA: int -> int -> int -> int with two accumulating parameters n1 and
    n2, where fibAnn1 n2 = Fn, when n1 = Fn−1 and n2 = Fn−2. Hint: consider suitable
    definitions of F−1 and F−2.

    2. A continuation-based version fibC: int -> (int -> int) -> int that is based on the
    definition of Fn given in Exercise 1.5.

Compare these two functions using the directive #time, and compare this with the while-loop
based solution of Exercise 8.6.
*)

//Standard version 
let rec fiboA = function
| 0 -> 0
| 1 -> 1
| n -> fibo(n-1) + fibo(n-2);;

//Accumulating parameter fibo
let fibA n =
    let rec fibAcc n1 n2 = function
    | n when n = 0 -> n1
    | n -> fibAcc n2 (n1+n2) (n-1)
    fibAcc 0 1 n;;

fibA 10;;

//Continuation based fibo
let rec fiboB n1 c = 
    if n1=0 then c 0
    else  fiboB (n1-1) (fun n2 -> c(n2+(n1)));;
    

fiboB 10 (fun x -> x);


(*
Exercise 4.7
Develop a version of the counting function for binary trees
countA: int -> BinTree<’a> -> int
that makes use of an accumulating parameter. Observe that this function is not tail recursive.
*)

// Tree type 
type BinTree<'a> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a> ;; 

// Standard count 
let rec count  = function
| Leaf -> 0 
| Node(tl, n, tr) -> count tl + count tr + 1 ;; 

// Accumulated parameter with tree t and accumulated parameter a  
// First counts tl recursively in accumulated parameter a and increments with 1 for each level in tree and then repeats for tr
let rec countA a t = 
    match t with
    | Leaf -> a // Result in accumulated parameter 
    | Node(tl, n, tr) -> countA (countA (a+1) tl) tr ;; // Result of counting tl and tr in accumulated parameter 

// Version with function keyword 
let rec countAFunc a t = function
| Leaf -> a 
| Node(tl, n, tr) -> countA (countA (a+1) tl) tr ;; 



(*
Exercise 4.8 HR exercise 9.9
Declare a tail-recursive functions with the type
countAC : BinTree<’a> -> int -> (int -> ’b) -> ’b
such that count t = countAC t 0 id. The intuition with countAC t a c is that a is the
number of nodes being counted so far and c is the continuation.

*)


// Id function returns what you get and is used to make tail-recursive continuations 
// Function is used to find parameters based on pattern match and it thus omitted below because the parameters are already shown 
// Arguments are the BinTree t, accumulator a and continuation c 
let rec countAC t a c  = 
    match t with
    | Leaf -> c a // Calls continuation of count with result from accumulated counter a in left tree  
    | Node(tl, n, tr) -> countAC tl (a+1) (fun countLeft -> (countAC tr countLeft c )) ;;// countAC (countAC (a+1) tl) tr ;; 
    // accumulator generates value while continuation generates a calculation of the remaining count 

// Tree example with depth 4 
let t1 = Node(Node(Leaf, 2, Node(Leaf, 2, Node(Leaf, 2, Leaf))), 3, Leaf) ;; 

// Count tree nodes 
countAC t1 0 id ;; 






(*
Exercise 4.9 HR exercise 9.10

Consider the following list-generating function:

      let rec bigListK n k =
          if n=0 then k []
          else bigListK (n-1) (fun res -> 1::k(res));;

The call bigListK 130000 id causes a stack overflow. Analyze this problem.

*)

let rec bigListK n k =
          if n=0 then k []
          else bigListK (n-1) (fun res -> 1::k(res));; // The function is gradually built up by calling the continuation function 

// Function takes res and cons id of res tail recursively
// Continuation is too big because it is not tail-recursive by consing 1 on a continuation function 
// Example: 1::1::1:: ... 1:[] will overflow the stack when consing 1, 130.000 times to the rest of the list where continuation function "res" becomes too big  







(*
Exercise 4.10 HR exercise 9.11

Declare tail-recursive functions leftTree and rightTree. 
By use of leftTree it should be possible to generate a big unbalanced tree to the left
containing n + 1 values in the nodes so that n is the value in the root, 
n − 1 is the value in the root of the left subtree, and so on. 

All subtree to the right are leaves. 
Similarly, using rightTree it should be possible to generate a big unbalanced tree to the right.

1. Use these functions to show the stack limit when using count and countA from Exer- cise 9.8.

2. Use these functions to test the performance of count C and count AC from Exercise 9.9.
*)

// Function generate a big unbalanced tree to the left   
let rec leftTree n c = 
    match n with
    | 0 -> c <| Leaf
    | n -> leftTree (n-1) (fun t -> c(Node(t, n, Leaf))) ;; 

// Function generate a big unbalanced tree to the right 
let rec rightTree n c = 
    match n with
    | 0 -> c <| Leaf
    | n -> rightTree(n-1) (fun t -> c(Node(Leaf, n, t))) ;; 


let testTree = leftTree 100000 id ;; // 5.000.000 nodes max  


// 1) Stack limit of count and count A 9.8 
// Stack limit is about 100.000 nodes in mono 
// countA does not cause stack ovrflow before running out of memory at 50.000.000 nods 

count testTree ;; // Stack limit of 1.000.000 nodes on I7 Quad core processor, mac 
countA 0 testTree ;; 

// 2) Performance of count AC from exercise 9.9
// We had issues with testing in Mono on Mac that did not allow creating bigger trees than with 100.000 nodes so all count functions work. 

let rec countC t c =
    match t with
        | Leaf -> c 0
        | Node(tl,n,tr) ->
            countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)));;

countC testTree id ;; 

countAC testTree 0 id ;; 





(*
Exercise 4.11 HR exercise 11.1

11.1 Make a declaration for the sequence of odd numbers.
*)

// All odd numbers in finite set 
let odds n = Seq.where(fun x -> (x % 2 <> 0)) n ;; 

// Example returns a sequence of odd numbers from list with elements from 0 to 10 
odds [0 .. 10] ;;

// An infinite sequence of natural numbers 
let nat = Seq.initInfinite(fun x -> x) ;;  

// Infinite sequence of odd numbers 
let uneven = Seq.filter(fun x -> (x % 2 <> 0)) nat ;; // Correct answer 

// Find n=1000 odd numbers in infinite sequence of odd numbers 'uneven'
Seq.toList(Seq.take 1000 uneven) ;; 









(*
Exercise 4.12 HR exercise 11.2

Make a declaration for the sequence of numbers 1, 1, 2, 6, . . . , n!, . . ..
// Sequence for all factorial numbers from 0 and to infinity 
*)

let factorial = Seq.initInfinite(fun x -> (factA (x, 1))) ;; // 0 represents accumulator m and x the number of factorials

// Find 10 first factorial numbers in infinite sequence of factorial numbers 
Seq.toList(Seq.take 10 factorial) ;;

// Curried function takes a function given a state to calculate a new state
// Given a state x, it calculates a new polymorphic t (int sequence) starting with initial state 1 
Seq.unfold (fun x -> if x = 0 then Some (1,1) else Some (x, x*(x+1))) 0 ;; // state is accumulator x, Some is a "nullable" type that either has something or nothing 
// Some is an option type either "Some" or "None" used to make 
// Returns an int * int * option