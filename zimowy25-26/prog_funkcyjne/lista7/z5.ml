module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
end

module BT : sig
  include Monad

  val fail : 'a t
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  type 'a t = 'a Seq.t

  let return x = Seq.return x
  let rec bind m f = Seq.concat_map f m

  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]

  let run m = m
end

type 'a regexp =
| Eps
| Lit of ('a -> bool)
| Or of 'a regexp * 'a regexp
| Cat of 'a regexp * 'a regexp
| Star of 'a regexp

let ( +% ) r1 r2 = Or(r1, r2)
let ( *% ) r1 r2 = Cat(r1, r2)


let rec match_regex (reg : 'a regexp) (w : 'a list) : 'a list option BT.t = 
  let open BT in 
  let (let*) = bind in
  match reg with
  | Eps -> return None (* dopasowalismy się ale nic nie zuzyliśmy*)
  | Lit(p) -> 
    begin match w with
    | [] -> fail
    | x :: xs when p x -> return (Some xs)
    | _ -> fail
    end
  | Or (r',r'') -> 
    (* tutaj powinniśmy zejść w dwie różne gałęzie, probójemy dopasować r' albo r''*)
      let* b = flip in 
      if b then match_regex r' w
      else      match_regex r'' w
  | Cat (r',r'') -> 
    let* res = match_regex r' w in
      begin match res with
        | None -> match_regex r'' w
        | Some(w) -> 
          let* res = match_regex r'' w in
            match res with
            | None -> return (Some w)
            | Some(w') -> return (Some (w'))
        end

  | Star (r') -> 
    let* b = flip in (* decydujemy czy używamy gwiazdki raz czy więcej razy*)
    if b then 
    let* res = match_regex r' w in
      match res with
      | None -> fail (*do nieczego sie nie dopasowalismy wiec nie probujemy dalej*)
      | Some(l) ->
        let* res = match_regex (Star (r')) l in 
        begin match res with 
        | None -> return (Some l)  (*jesli udalo sie dopasowac tylko raz to zwracamy od razu*)
        | Some x -> return res
        end
    else return None  (*dopasowalismy się 0 razy, wiec puste dopasowanie*)
    (* próbujemy dopasować r' dopóki się da? czyli też seria flipów, albo się dopasowujemy raz albo dwa albo... dopoki nie mozemy*)
    

let reg1 = Star (Star (Lit ((<>) 'b')) +% (Lit ((=) 'b') *% Lit ((=) 'a')))
let reg5 = Star (Lit ((<>) 'b')) +% (Lit ((=) 'b') *% Lit ((=) 'a'))
let reg2 = Star (Lit ((=) 'b'))
let reg3 = Star ((Lit ((=) 'b')) +% (Lit ((<>) 'b'))) 
let reg4 = Star ((Lit ((=) 'b')) *% (Lit ((<>) 'b')))

let t5 = List.of_seq (BT.run (match_regex reg5 (['a';'b';'a';'b';'a';'a';'a';'b'])));; 
let t2 = List.of_seq (BT.run (match_regex reg2 (['b';'b';'b';'a';'b';'a';'b'])));;
let t3 = List.of_seq (BT.run (match_regex reg3 (['b';'b';'b';'a';'b';'a';'b'])));;
let t4 = List.of_seq (BT.run (match_regex reg4 (['b';'a';'b';'a';'b';'a';'b'])));;

let t1 = List.of_seq (BT.run (match_regex reg1 (['a';'b';'a';'b';'a';'a';'a';'b'])));;