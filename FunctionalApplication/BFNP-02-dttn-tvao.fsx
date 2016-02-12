

//Exercise 2.1

let downTo n = 
    if n > 0 
    then [n.. -1 ..1]
    else [];;


let downTo2 n =
    match n with 
    | n when n <= 0 -> []
    | n when n > 0 -> [n.. -1 ..1]
    | _ -> [];;


//Exercise 2.2 

let rec removeOddIdx xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | [x0;x1] -> [x0]
    | x :: _ :: tail ->  x :: removeOddIdx tail ;;

removeOddIdx [1;2;3;4;5;6];;

//Exercise 2.3
let rec combinePair xs =
    match xs with
    | [] -> []
    | [x] -> []
    | x0 :: x1 :: tail  -> (x0,x1) :: combinePair tail;;


//Exercise 2.4
(*
The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of
integers, and declare the functions when a representation by records is used. Declare the functions
in infix notation with proper precedences, and use patterns to obtain readable declarations

 12 pence = 1 shilling
 20 shillings = 1 pound
*)

type amount = {pounds:int ; shillings:int ; pence:int};;

//Converts amount represented as triple (pounds,shillings,pence) to pence.
let denormalizeAmount x = {pounds = 0; shillings = 0; pence=(x.pounds*240)+(x.shillings*12)+x.pence};;
   

   
//Converts pence to shillings and pounds as a triple (pounds,shillings,pence)
let rec normalizeAmount xs =
    match xs with
    | {pounds = 0; shillings = 0; pence = y} ->    if y < 12                               
                                                   then xs
                                                   else normalizeAmount {pounds = 0; shillings = xs.pence/12; pence = xs.pence%12} // Converts pence to shillings
    | {pounds = 0; shillings = x; pence = y} ->    if x < 20                                  
                                                   then xs
                                                   else {pounds = x/20; shillings = x%20; pence = y};;                // Converts shillings to pounds


let amountToTriple x = (x.pounds, x.shillings, x.pence);;
 
 let addition (a1:amount) (a2:amount) =
    let amountToPence = denormalizeAmount {pounds = a1.pounds+a2.pounds ; shillings = a1.shillings+a2.shillings ; pence = a1.pence+a2.pence}
    let result = normalizeAmount amountToPence
    amountToTriple result;;

let total1 = addition {pounds = 0;shillings=0;pence=1} {pounds = 0;shillings=0;pence=1};;
let total2 = addition {pounds = 10;shillings=10;pence=10} {pounds = 10;shillings=10;pence=1};;


let substract (a1:amount) (a2:amount) =
    let amountToPence = denormalizeAmount {pounds = a1.pounds-a2.pounds ; shillings = a1.shillings-a2.shillings ; pence = a1.pence-a2.pence}
    normalizeAmount amountToPence;;
 

 (*
    Exercise 2.5
 *)

 //1) infix Addition and subtract functions


 let  (.+) (x1,y1) (x2,y2) = (x1+x2,y1+y2);;
 let  (.*) (x1,y1) (x2,y2) = (x1*x2)-(y1*y2) , (y1*x1)+(x1*y2);;

 //2)
 let (./) (x1,y1) (x2,y2) = ( (x1/x1*x1 + y1*y1) , (-y1/x1*x1 + y1*y1) ) .* (x2,y2);;
 let (.-) (x1,y1) (x2,y2) = ((x1,y1) .+ (-x2,-y2)) ;;  
 
 //3 
  let (../) (x1,y1) (x2,y2) = 
    let a = (x1/x1*x1 + y1*y1)
    let b = (-y1/x1*x1 + y1*y1)
    (a,b) .* (x2,y2);;

 (*
    Exercise 2.6
    Give a declaration for altsum (see Page 76) containing just two clauses.
 *)

 let rec altSum = function 
    | [] -> 0
    | x0::xs -> x0 - altSum xs;;
 

 (*
    Exercise 
 *)


let explode (s:string) = List.ofArray(s.ToCharArray()) // gives exploded list based on char array 

let rec explode2 (s:string) = if s.Length = 0 then [] else s.Chars(0) :: explode2 (s.Remove(0,1)) ;; // Remove is used to remove first index and then explodes tail 

(* 
Exercise 2.8

Write a function implode:char list->string
so that implode s returns the characters concatenated into a string: implode [’a’;’b’;’c’] = "abc"
Hint: Use List.foldBack. Now write a function
implodeRev:char list->string
so that implodeRev s returns the characters concatenated in reverse order into a string:
implodeRev [’a’;’b’;’c’] = "cba"
Hint: Use List.fold.

*)

let implode (list : char list) =
    List.foldBack(fun c s -> c.ToString() + s) list"";;


let implodeRev (list : char list) =
    List.fold(fun c s -> s.ToString() + c) "" list;;

(*
Exercise 2.9
*)

let toUpper (s:string) =  explode s |> List.map System.Char.ToUpper |> implode;;

let toUpper1 (s:string) = s |> (explode >> List.map System.Char.ToUpper >> implode);;  //(a >> b >> c)

let toUpper2 (s:string) = s |> (implode << List.map System.Char.ToUpper << explode);;  //(c << b << a)


(*
Exercise 2.10
palindrome:string->bool,
so that palindrome s returns true if the string s is a palindrome; otherwise false.
A string is called a palindrome if it is identical to the reversed string, eg, “Anna” is a palindrome but “Ann” is not.
The function is not case sensitive.
*)


let palindrome (s:string) = 
            let upperCase = toUpper (s)
            let firstHalf =  9//upperCase.Substring(s,s.Length/2)
            let secondHalf = 9//upperCase.Substring(s.Length/2,s.Length) |> (explode >> implodeRev)
            if firstHalf = secondHalf
            then true 
            else false;;
   