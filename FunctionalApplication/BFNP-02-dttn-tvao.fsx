
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

//Exercise 2.3let rec combinePair xs =
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


let shillingToPence x = x*12;;

let poundsToPence x = x*240;;

let rec normalizeAmount x =
    match x with
    | (0,0,x) -> 


 
  

 