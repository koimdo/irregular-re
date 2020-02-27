open Base

type ('a, 'b) t

type ('a, 'b) leaf = 'a list -> 'b option
type 'b branch = 'b list -> 'b option

val empty : ('a,'b) t
val add : ('a, 'b) t -> 'a -> ('a, 'b) t
val push : ('a, 'b) t -> f:'b branch -> ('a, 'b) t
val leaf : ('a, 'b) t -> f:('a, 'b) leaf -> ('a, 'b) t
val pop : ('a, 'b) t -> (('a, 'b) t, 'b) Continue_or_stop.t
