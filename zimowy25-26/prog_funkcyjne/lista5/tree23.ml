module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty      : t
  val is_empty   : t -> bool
  val mem        : elt -> t -> bool
  val add        : elt -> t -> t
  val remove_min : t -> (elt * t) option
  val remove     : elt -> t -> t
end

module Make (Elt : OrderedType) : S with type elt = Elt.t = struct
  type elt = Elt.t

  type z = Z
  type 'a s = S of 'a

  type _ tree =
    | Leaf  : z tree
    | Node2 : 'n tree * elt * 'n tree -> 'n s tree
    | Node3 : 'n tree * elt * 'n tree * elt * 'n tree -> 'n s tree

  type t =
    | Tree : 'n tree -> t

  let empty = Tree Leaf

  let is_empty (Tree t) =
    match t with
    | Leaf -> true
    | Node2 _ | Node3 _ -> false

  let rec tree_mem : type n. elt -> n tree -> bool =
    fun x t ->
    match t with
    | Leaf -> false
    | Node2(l, y, r) ->
      begin match Elt.compare x y with
      | 0            -> true
      | c when c < 0 -> tree_mem x l
      | _            -> tree_mem x r
      end
    | Node3(l, y, m, z, r) ->
      begin match Elt.compare x y with
      | 0            -> true
      | c when c < 0 -> tree_mem x l
      | _ ->
        begin match Elt.compare x z with
        | 0            -> true
        | c when c < 0 -> tree_mem x m
        | _            -> tree_mem x r
        end
      end

  let mem x (Tree t) =
    tree_mem x t

  (* ======================================================================= *)
  (* Addition *)

  type 'n add_result =
    | Added of 'n tree
    | Split of 'n tree * elt * 'n tree

  (* Smart constructors for addition *)
  let node2a1 l y r =
    match l with
    | Added l          -> Added (Node2(l, y, r))
    | Split(ll, x, lr) -> Added (Node3(ll, x, lr, y, r))

  let node2a2 l x r =
    match r with
    | Added r          -> Added (Node2(l, x, r))
    | Split(rl, y, rr) -> Added (Node3(l, x, rl, y, rr))

  let node3a1 l y m z r =
    match l with
    | Added l          -> Added (Node3(l, y, m, z, r))
    | Split(ll, x, lr) -> Split (Node2(ll, x, lr), y, Node2(m, z, r))

  let node3a2 l x m z r =
    match m with
    | Added m          -> Added (Node3(l, x, m, z, r))
    | Split(ml, y, mr) -> Split (Node2(l, x, ml), y, Node2(mr, z, r))

  let node3a3 l x m y r =
    match r with
    | Added r          -> Added (Node3(l, x, m, y, r))
    | Split(rl, z, rr) -> Split (Node2(l, x, m), y, Node2(rl, z, rr))

  let rec tree_add : type n. elt -> n tree -> n add_result =
    fun x t ->
    match t with
    | Leaf -> Split(Leaf, x, Leaf)
    | Node2(l, y, r) ->
      begin match Elt.compare x y with
      | 0            -> Added t
      | c when c < 0 -> node2a1 (tree_add x l) y r
      | _            -> node2a2 l y (tree_add x r)
      end
    | Node3(l, y, m, z, r) ->
      begin match Elt.compare x y with
      | 0            -> Added t
      | c when c < 0 -> node3a1 (tree_add x l) y m z r
      | _            ->
        begin match Elt.compare x z with
        | 0            -> Added t
        | c when c < 0 -> node3a2 l y (tree_add x m) z r
        | _            -> node3a3 l y m z (tree_add x r)
        end
      end

  let add x (Tree t) =
    match tree_add x t with
    | Added t         -> Tree t
    | Split(l, y, r)  -> Tree (Node2(l, y, r))

  (* ======================================================================= *)
  (* Removal of the minimum element *)

  type _ remove_result =
    | Removed   : 'n tree -> 'n remove_result
    | Underflow : 'n tree -> 'n s remove_result

  type _ pick_result =
    | PickSome : elt * 'n remove_result -> 'n pick_result
    | PickNone : z pick_result

  (* Smart constructors for removal *)
  let node2r1 (type n) (l : n remove_result) x (r : n tree) =
    match l with
    | Removed l   -> Removed (Node2(l, x, r))
    | Underflow l ->
      begin match r with
      (* Case Leaf is impossible.
        Thanks to GADTs, we don't need to handle it. *)
      | Node2(rl, y, rr) ->
        Underflow (Node3(l, x, rl, y, rr))
      | Node3(rl, y1, rm, y2, rr) ->
        Removed (Node2(Node2(l, x, rl), y1, Node2(rm, y2, rr)))
      end

  let node2r2 (type n) l y (r : n remove_result) =
    match r with
    | Removed r   -> Removed (Node2(l, y, r))
    | Underflow r ->
      begin match l with
      | Node2(ll, x, lr) ->
        Underflow (Node3(ll, x, lr, y, r))
      | Node3(ll, x1, lm, x2, lr) ->
        Removed (Node2(Node2(ll, x1, lm), x2, Node2(lr, y, r)))
      end

  let node3r1 (type n) (l : n remove_result) x (m : n tree) z r =
    match l with
    | Removed l   -> Removed (Node3(l, x, m, z, r))
    | Underflow l ->
      begin match m with
      | Node2(ml, y, mr) ->
        Removed (Node2(Node3(l, x, ml, y, mr), z, r))
      | Node3(ml, y1, mm, y2, mr) ->
        Removed (Node3(Node2(l, x, ml), y1, Node2(mm, y2, mr), z, r))
      end

  let node3r2 (type n) l x (m : n remove_result) y r =
    match m with
    | Removed m   -> Removed (Node3(l, x, m, y, r))
    | Underflow m ->
      begin match r with
      | Node2(rl, z, rr) ->
        Removed (Node2(l, x, Node3(m, y, rl, z, rr)))
      | Node3(rl, z1, rm, z2, rr) ->
        Removed (Node3(l, x, Node2(m, y, rl), z1, Node2(rm, z2, rr)))
      end

  let node3r3 (type n) l x m z (r : n remove_result) =
    match r with
    | Removed r   -> Removed (Node3(l, x, m, z, r))
    | Underflow r ->
      begin match m with
      | Node2(ml, y, mr) ->
        Removed (Node2(l, x, Node3(ml, y, mr, z, r)))
      | Node3(ml, y1, mm, y2, mr) ->
        Removed (Node3(l, x, Node2(ml, y1, mm), y2, Node2(mr, z, r)))
      end

  let rec tree_remove_min : type n. n tree -> n pick_result =
    fun t ->
    match t with
    | Leaf -> PickNone
    | Node2(l, y, r) ->
      begin match tree_remove_min l with
      | PickNone       -> PickSome(y, Underflow r)
      | PickSome(x, l) -> PickSome(x, node2r1 l y r)
      end
    | Node3(l, y, m, z, r) ->
      begin match tree_remove_min l with
      | PickNone       -> PickSome(y, Removed (Node2(m, z, r)))
      | PickSome(x, l) -> PickSome(x, node3r1 l y m z r)
      end

  let remove_min (Tree t) =
    match tree_remove_min t with
    | PickNone -> None
    | PickSome(x, Removed t)   -> Some (x, Tree t)
    | PickSome(x, Underflow t) -> Some (x, Tree t)

  (* ======================================================================= *)
  (* Removal of an arbitrary element *)

  let tree_merge (type n) (l : n tree) (r : n tree) : n s remove_result =
    match tree_remove_min r with
    | PickNone -> Underflow l
    | PickSome(x, r) -> node2r2 l x r

  let tree_merge31 (type n) (l : n tree) (m : n tree) y (r : n tree) :
      n s remove_result =
    match tree_remove_min m with
    | PickNone -> Removed (Node2(l, y, r))
    | PickSome(x, m) -> node3r2 l x m y r

  let tree_merge32 (type n) (l : n tree) x (m : n tree) (r : n tree) :
      n s remove_result =
    match tree_remove_min r with
    | PickNone       -> Removed (Node2(l, x, m))
    | PickSome(y, r) -> node3r3 l x m y r

  let rec tree_remove : type n. elt -> n tree -> n remove_result =
    fun x t ->
    match t with
    | Leaf -> Removed Leaf
    | Node2(l, y, r) ->
      begin match Elt.compare x y with
      | 0            -> tree_merge l r
      | c when c < 0 -> node2r1 (tree_remove x l) y r
      | _            -> node2r2 l y (tree_remove x r)
      end
    | Node3(l, y, m, z, r) ->
      begin match Elt.compare x y with
      | 0            -> tree_merge31 l m z r
      | c when c < 0 -> node3r1 (tree_remove x l) y m z r
      | _            ->
        begin match Elt.compare x z with
        | 0            -> tree_merge32 l y m r
        | c when c < 0 -> node3r2 l y (tree_remove x m) z r
        | _            -> node3r3 l y m z (tree_remove x r)
        end
      end

  let remove x (Tree t) =
    match tree_remove x t with
    | Removed t   -> Tree t
    | Underflow t -> Tree t
end
