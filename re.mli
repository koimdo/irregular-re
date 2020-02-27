open Base

type ('a, 'b) t
val eps : ('a, 'b) t
val lit : ?desc:string -> ('a -> bool) -> ('a, 'b) t
val cat : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val alt : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val star : ('a, 'b) t -> ('a, 'b) t
val option : ('a, 'b) t -> ('a, 'b) t
val plus : ('a, 'b) t -> ('a, 'b) t
val capture : name:string -> f:'b Treebuilder.branch -> ('a, 'b) t -> ('a, 'b) t
val leaf : name:string -> f:('a, 'b) Treebuilder.leaf -> ('a, 'b) t -> ('a, 'b) t

module Infix : sig
  val ( <.> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val ( ~= ) : string -> (char, 'b) t
  val ( ~: ) : ('a -> bool) -> ('a, 'b) t
  val ( #: ) : string -> ('a -> bool) -> ('a, 'b) t
  val ( ~* ) : ('a, 'b) t -> ('a, 'b) t
  val ( ~? ) : ('a, 'b) t -> ('a, 'b) t
  val ( ~+ ) : ('a, 'b) t -> ('a, 'b) t
  val ( .%{} ) : string * 'b Treebuilder.branch -> ('a, 'b) t -> ('a, 'b) t
  val ( .={} ) : string * ('a, 'b) Treebuilder.leaf -> ('a, 'b) t -> ('a, 'b) t
end

val start : ('a, 'b) t -> ('a, 'b) Channel.t


module Sexpbuilder : sig
  val leaf : (char, Sexp.t) Treebuilder.leaf
  val branch : string -> Sexp.t Treebuilder.branch
end

module String : sig
  val run : (char, 'a) t
            -> string
            -> (char, (int * 'a)) Channel.t
end
