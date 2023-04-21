type 'a binomial_t = Node of 'a * 'a binomial_t list * int
type 'a binomial_heap_t = 'a binomial_t list
val empty_heap : 'a list

val is_empty_heap : 'a binomial_heap_t -> bool
val add_heap : 'a -> 'a binomial_t list -> 'a binomial_t list
val find_min_heap : 'a binomial_t list -> 'a
val delete_min_heap : 'a binomial_t list -> 'a binomial_t list
