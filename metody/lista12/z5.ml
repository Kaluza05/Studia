type 'v formula =
| Lit of  'v
| Conj of 'v formula * 'v formula
| Disj of 'v formula * 'v formula
| Impl of 'v formula * 'v formula
| Neg of  'v formula

type 'v nnf =
| NNFLit of bool * 'v
| NNFConj of 'v nnf * 'v nnf
| NNFDisj of 'v nnf * 'v nnf

let rec to_nnf (f : 'v formula) : 'v nnf =
  match f with
  | Lit (f)   -> NNFLit (true, f)
  | Conj(f1,f2) -> NNFConj(to_nnf f1, to_nnf f2)
  | Disj(f1,f2) -> NNFDisj(to_nnf f1, to_nnf f2)
  | Impl(f1,f2) -> NNFDisj(to_nnf (Neg f1), to_nnf f2)
  | Neg f -> (match f with
    | Lit(f)    -> NNFLit (false, f)
    | Conj(f1,f2) -> NNFDisj(to_nnf (Neg f1), to_nnf (Neg f2))
    | Disj(f1,f2) -> NNFConj(to_nnf (Neg f1), to_nnf (Neg f2))
    | Impl(f1,f2) -> NNFConj(to_nnf f1      , to_nnf (Neg f2))
    | Neg f -> to_nnf f)


let rec to_nnf2 (f : 'v formula) : 'v nnf =
  match f with
  | Lit (f)   -> NNFLit (true,f)
  | Conj(f1,f2) -> NNFConj(to_nnf f1, to_nnf f2)
  | Disj(f1,f2) -> NNFDisj(to_nnf f1, to_nnf f2)
  | Impl(f1,f2) -> NNFDisj(neg f1, to_nnf f2)
  | Neg f -> neg f
and neg (f : 'v formula) : 'v nnf = 
  match f with
    | Lit(f)    -> NNFLit (false, f)
    | Conj(f1,f2) -> NNFDisj(neg f1   , neg f2)
    | Disj(f1,f2) -> NNFConj(neg f1   , neg f2)
    | Impl(f1,f2) -> NNFConj(to_nnf f1, neg f2)
    | Neg f -> to_nnf f