module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module IdMonad : Monad = struct
  type 'a t = 'a

  let return x = x

  let bind a f = f a
end

module CPSMonad : Monad = struct
  type 'a t = unit -> 'a

  let return x = fun () -> x

  let bind a f = fun () -> f (a ()) ()

end