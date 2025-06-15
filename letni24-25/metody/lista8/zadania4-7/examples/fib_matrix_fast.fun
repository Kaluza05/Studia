let id = (1,(0,(0,1))) in 

let matrix_mult = fun a b -> 
(fst a * fst b + fst (snd a) * fst (snd (snd b)),
(fst a * fst (snd b) + fst (snd a) * snd (snd (snd b)),
(fst (snd (snd a)) * fst b + snd (snd (snd a)) * fst (snd (snd b)),
 fst (snd (snd a)) * fst (snd b) + snd (snd (snd a)) * snd (snd (snd b))))) in

let matrix_expt_fast = fun m k -> 
(funrec exp n -> fun pow -> if pow = 0 then id else 
if pow mod 2 = 0 then (exp (matrix_mult n n)) (pow /2) 
else matrix_mult n ((exp (matrix_mult n n)) (pow/2))) m k
in
let fib_fast = fun n -> let x = (1,(1,(1,0))) in fst (matrix_expt_fast x (n - 1))
in fib_fast 10
