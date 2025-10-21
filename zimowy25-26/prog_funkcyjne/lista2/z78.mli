module type Heap = sig
  type 'a heap = Leaf | Node of int * 'a heap * 'a * 'a heap

  val insert :  'a -> 'a heap -> 'a heap
  val pop : 'a heap -> 'a heap
end