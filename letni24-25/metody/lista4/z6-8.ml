module LeftistHeap = struct
  type ('a , 'b ) heap =
  | HLeaf
  | HNode of int * ('a , 'b ) heap * 'a * 'b * ('a , 'b ) heap 
  let rank = function HLeaf -> 0 | HNode (n , _ , _ , _ , _ ) -> n
  let heap_ordered p = function
  | HLeaf -> true
  | HNode (_ , _ , p', _ , _ ) -> p <= p'
  let rec is_valid = function
    | HLeaf -> true
    | HNode (n , l , p , v , r ) ->
      rank r <= rank l
      && rank r + 1 = n
      && heap_ordered p l
      && heap_ordered p r
      && is_valid l
      && is_valid r
  let make_node p v l r = let (l,r) = if rank l > rank r then (l,r) else (r,l) in HNode (1 + rank r ,l , p , v , r)
  let rec heap_merge heap1 heap2 =
    match heap1,heap2 with
    | HLeaf , HLeaf -> HLeaf
    | heap1 ,HLeaf -> heap1
    | HLeaf , heap2 -> heap2
    | HNode (_,l,p,e,r) , HNode (_,l',p',e',r') -> if p <= p' then make_node p e l (heap_merge r heap2) else make_node p' e' l' (heap_merge heap1 r' )
  end

module type PRIO_QUEUE = sig
  type priority
  type 'a queue
  val empty : 'a queue
  val insert : priority -> 'a -> 'a queue -> 'a queue
  val pop : 'a queue -> 'a queue

  val min : 'a queue -> 'a
  val min_prio : 'a queue -> priority
end

module PQSORT (M:PRIO_QUEUE) = struct
  let pqsort xs =
    let rec make_pq xs =
      match xs with 
      | [] -> M.empty
      | x::xs' -> M.insert x x (make_pq xs')
    and sort_pq  queue =
      if queue = M.empty then [] else let e = M.min queue and rest = M.pop queue in e :: sort_pq rest
  in xs |> make_pq |> sort_pq
end


module  LH :PRIO_QUEUE = struct
  open LeftistHeap
  type priority = int
  type 'a queue = (priority,'a) heap
  let empty = HLeaf
  let insert x v heap = heap_merge (make_node x v HLeaf HLeaf) heap
  let pop = function 
  | HLeaf -> HLeaf
  | HNode (_,l,_,_,r) -> heap_merge l r

  let min = function 
    HLeaf -> failwith "puste"
  | HNode (_,_,_,v,_) ->  v
  let min_prio = function
  | HLeaf -> failwith "puste"
  | HNode (_,_,p,_,_) ->  p
end

module HeapSort = PQSORT(LH)


