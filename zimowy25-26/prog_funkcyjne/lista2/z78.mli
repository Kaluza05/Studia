module type Heap = sig
  type 'a heap

  val empty : 'a heap
  val insert :  'a -> 'a heap -> 'a heap
  val pop : 'a heap -> 'a heap
  val find_min : 'a heap -> 'a option
end