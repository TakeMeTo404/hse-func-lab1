let eps = 0.00001

let rec dichotomy f a b =
    if b-a < eps then a
    else
    let c = (a+b)/2.
    match c with
    | c when sign (f c) = 0 -> c
    | c when sign (f c) = sign (f a) -> dichotomy f c b
    | c when sign (f c) = sign (f b) -> dichotomy f a c
    | c -> dichotomy f a c // for example, dichotomy (x^2 - 1) -10 10

let diff (a: float) (b: float) = abs (abs a - abs b)

let rec iterations phi x0 =
    match phi x0 with
    | x1 when (System.Double.IsInfinity x1) -> x1 
    | x1 when (diff x0 x1 < eps) -> x1
    | x1 -> iterations phi x1

let rec newthon f f' x0 =
    match x0 - (f x0 / f' x0) with
    | x1 when (System.Double.IsInfinity x1) -> x1 
    | x1 when diff x1 x0 < eps -> x1
    | x1 -> newthon f f' x1 

// Solve 3 equations using three methods defined above
let f1 = fun x -> 1. - x + sin x - log (1.+x)
let f2 = fun x -> 3.*x - 14. + exp x + exp -x
let f3 = fun x -> sqrt (1.-x) - tan x

let f1' = fun x -> -1. + cos x - (1. / (1. + x))
let f2' = fun x -> 3. + 2.*(exp x) 
let f3' = fun x -> (-1./2. / sqrt (1.-x) - (1./cos x / cos x))

let phi1 = fun x -> x - (f1 x)/(f1' x)
let phi2 = fun x -> x - (f2 x)/(f2' x)
let phi3 = fun x -> x - (f3 x)/(f3' x)

let main =
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 1. 1.5) (iterations phi1 1.) (newthon f1 f1' 1.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 1. 3.) (iterations phi2 1.) (newthon f2 f2' 1.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 0. 1.) (iterations phi3 0.) (newthon f3 f3' 0.)