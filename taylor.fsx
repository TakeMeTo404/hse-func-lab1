// Print a table of a given function f, computed by taylor series

// function to compute
let f = fun x -> log (x + 2.)

let a = -1.0
let b = 1.0
let n = 10

// kid
let rec pow__ a b =
    if b = 0. then
        1.
    else
        (*) a (pow__ a (b - 1.0))

// boy
let rec iter f acc a b =
    if a > b then acc
    else iter f (f a acc) (a + 1.) b

let pow a b =
    iter (fun i -> fun acc -> (*) a acc) 1. 1. b

// man
(* maybe, later *)

let rec sum_series_naive f n =
    iter (fun i -> fun acc -> acc + (f i)) 0. 1. n


// calc n'th item
let nth x n =
    pow -1.0 (n - 1.0)
    |> (*) (pow x n)
    |> fun x -> (/) x n // please, write in comments, how to do that in smarter way
    |> fun x -> (/) x (pow 2. n) // I, actually, have to curry the 2nd argument of (/)

let taylor_naive x =
    log 2. + sum_series_naive (nth x) 20.

// Define a function to do the same in a more efficient way

type Acc = { prev: float; sum: float }

let sum_series next n =
    iter
        (fun i ->
            fun (acc: Acc) ->
                let ith = next i acc.prev
                { prev = ith; sum = acc.sum + ith })
        { prev = 0.; sum = 0. } // start for accumulator
        1.
        n
        
let taylor = fun x ->
    let next = fun i -> fun prev -> if prev=0. then x/2. else prev*(-1.)*x*(i-1.)/(i*2.)
    log 2. + (sum_series next 20.).sum
    
let main =
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        printfn "%5.2f  %10.6f  %10.6f   %10.6f" x (f x) (taylor_naive x) (taylor x)
// make sure to improve this table to include the required number of iterations
// for each of the methods

main
