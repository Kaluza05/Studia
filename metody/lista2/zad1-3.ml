let rec fib n =
  if n = 0
      then 0
  else if n = 1
      then 1
  else fib (n-1) + fib (n-2);;


let fib_it n = 
  let rec iter k before1 before2 =
      if k = 0
      then
          before2
      else iter (k-1) (before1+before2) before1
  in iter n 1 0;;

type matrix = int*int*int*int;;

let matrix_id = ((1,0,0,1) : matrix);;
let matrix_mult ((a,b,c,d):matrix) ((e,f,g,h):matrix) : matrix = (a*e+b*g,a*f+b*h,c*e+d*g,c*f+d*h);;

let matrix_expt m k =
    let rec it n k =
      if k = 0
        then matrix_id
      else if k = 1
      then n
      else it (matrix_mult n m) (k-1)
    in it m k;;

let first (x,_,_,_) = x;;
let fib_matrix n =
    let x = (1,1,1,0)
    in
    first (matrix_expt x (n-1));;

let matrix_expt_fast m k = 
    let rec exp n pow =
      if pow = 0
        then matrix_id
      else
        if pow mod 2 = 0
          then exp (matrix_mult n n) (pow/2)
        else matrix_mult n (exp (matrix_mult n n) (pow/2))
    in exp m k;;

    
let fib_fast n = 
  let x = (1,1,1,0)
  in
  first (matrix_expt_fast x (n-1));;