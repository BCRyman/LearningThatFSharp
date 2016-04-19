// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open Test

// Define your library scripting code here

let square x = x * x

let s2 = square 2
let s3 = square 3
let s4 = square 4


let sumOfSquares n =
    [1..n] 
    |> List.map square 
    |> List.sum

sumOfSquares 100

//recursive quicksort
let rec quicksort list =
    match list with      //like a C# switch case
    | [] ->                                 //If List is Empty
         []                                 //Returns empty list
    | firstElem::otherElements ->           //If List is not empty
        let smallerElements  =              //Extract smaller one
            otherElements
            |> List.filter (fun e -> e < firstElem)
            |> quicksort                    //sort them
        let largerElements =                //extracts the larger ones
            otherElements                   
            |> List.filter (fun e -> e > firstElem)
            |> quicksort
        //combine the 3 parts into a new list and return it
        List.concat [smallerElements; [firstElem]; largerElements]

//test quicksort
printfn "%A" (quicksort [1;5;23;18;9;1;3])

//A shorter more concise quicksort

let rec quicksort2 = function
    | [] -> []
    | first::rest ->
        let smaller,larger = List.partition ((>=) first) rest
        List.concat [quicksort2 smaller; [first]; quicksort2 larger]