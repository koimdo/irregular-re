open Base

type ('a, 'b) t =
  | Finished of 'b
  | Continue of 'b option * ('a -> ('a, 'b) t)
  | Failed

val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
val fail : 'a -> ('b, 'c) t
val return : 'b -> ('a, 'b) t
val bind : ('a, 'b) t -> f:('b -> ('b, 'c) t) -> ('a, 'c) t

val both : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t

module Infix : sig
  val ( >>= ) : ('a, 'b) t -> ('b -> ('b, 'c) t) -> ('a, 'c) t
end

val fu_kernel : ('a, 'b) t -> 'a -> (('a, 'b) t, ('a, 'b) t) Continue_or_stop.t

val unfold : 'a -> ('a -> 'a) -> ('i, 'a) t

