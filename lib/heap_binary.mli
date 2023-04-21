type 'a heap = Empty | Node of int * 'a * 'a heap * 'a heap
val empty_heap : 'a heap

val is_empty_heap : 'a heap -> bool
val merge_heap : 'a heap -> 'a heap -> 'a heap
val add_heap : 'a -> 'a heap -> 'a heap
val find_min_heap : 'a heap -> 'a
val delete_min_heap : 'a heap -> 'a heap
