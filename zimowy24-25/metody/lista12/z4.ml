type empty = | 
type 'a not = 'a -> empty

let pierce3 ( z : 'a not not -> 'a) ( x : 'a not -> 'a) : 'a = 
    (* y jest typu 'a not czyli x y jest typu 'a , zatem y (x y) jest empty*)
    (* czyli p jest typu 'a not -> empty czyli 'a not not*)
    (* czyli z p jest typu 'a *)
    let p = fun y -> y (x y) in z p

let f z = fun x -> z (fun y -> y (x y)) ;;