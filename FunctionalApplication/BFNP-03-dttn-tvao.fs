(*
Exercise 3.1
Consider the definition of type ’a BinTree on slide 30. Write a function
inOrder : ’a BinTree -> ’a list
that makes an in-order traversal of the tree and collect the elements in a result list. In-order traversal is defined on
slide 32.
*)



type 'a BinTree = 
    |Leaf
    |Node of 'a * 'a BinTree * 'a BinTree;;

let rec inOrder tree = 
    match tree with
    |Leaf -> []
    |Node(x,tl,tr) -> (inOrder tl) @ [x] @ (inOrder tr);; 

let floatBinTree =
    Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
    Node(562.0, Leaf, Node(78.0, Leaf,Leaf)));;

inOrder floatBinTree;;


(*
    Exercise 3.2 Write a function
    mapInOrder : (’a -> ’b) -> ’a BinTree -> ’b BinTree
    that makes an in-order traversal of the binary tree and apply the function on all nodes in the tree.
    Can you give an example of why mapInOrder might give a result different from mapPostOrder, but the
    result tree returned in both cases is still the same.
*)

let rec mapInOrder f tree =
    match tree with
    | Leaf -> Leaf
    | Node(x,tl,tr) -> Node(f x, mapInOrder f tl, mapInOrder f tr);;
    
mapInOrder (fun x -> x+2.0) floatBinTree;;  //Applies the function f(x) = x+2 on every nodes in a given tree


(*
Exercise 3.3 Write a function

    foldInOrder : (’a -> ’b -> ’b) -> ’b -> ’a BinTree -> ’b
that makes an in-order traversal of the tree and folds over the elements.

For instance, given the tree
    let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
    Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

the application

foldInOrder (fun n a -> a + n) 0.0 floatBinTree
*)



let foldInOrder f e tree = 
    let list = inOrder tree
    List.fold f e list;;

foldInOrder (fun n a -> a + n) 0.0 floatBinTree //returns 764.0. which is the correct answer(*Exercise 3.4 Complete the program skeleton for the interpreter presented on slide 28 in the slide deck from the
lecture 5 about finite trees.
Define 5 examples and evaluate them.
The declaration for the abstract syntax for arithmetic expressions follows the grammar (slide 23):
type aExp = (* Arithmetical expressions *)
| N of int (* numbers *)
| V of string (* variables *)
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp (* subtraction *)
The declaration of the abstract syntax for boolean expressions is defined as follows (slide 25type bExp = (* Boolean expressions *)
| TT (* true *)
| FF (* false *)
| Eq of aExp * aExp (* equality *)
| Lt of aExp * aExp (* less than *)
| Neg of bExp (* negation *)
| Con of bExp * bExp (* conjunction *)
The conjunction of two boolean values returns true if both values are true.
The abstract syntax for the statements are defined as below (slide 26):
type stm = (* statements *)
| Ass of string * aExp (* assignment *)
| Skip
| Seq of stm * stm (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)*)