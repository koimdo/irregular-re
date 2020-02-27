open Base

type ('a, 'b) leaf = 'a list -> 'b option
type 'b branch = 'b list -> 'b option

type ('a, 'b) t = {
    fl : ('a, 'b) leaf option;
    buf : 'a list;
    stack: ('b branch * 'b list) list;
  }

let empty = { fl=None; stack=[]; buf=[] }

let leaf builder ~f = match builder.fl with
  | None -> { builder with fl=(Some f); buf=[] }
  | Some _ -> raise (Invalid_argument "Nested leaf!")

let add builder a = match builder.fl with
  | None -> builder
  | Some _ -> { builder with buf=a::builder.buf }

let push { stack; _} ~f =
  { empty with stack=(f, [])::stack }

let pop b =
  let build f bs = f (List.rev bs) in
  let open Continue_or_stop in
  let leaf = match b.fl, b.buf with
    | Some fl, (_::_ as buf) -> fl (List.rev buf)
    |_, _ -> None
  in
  match leaf, b.stack with
  | Some b, [] -> Stop b
  | Some b, (f, bs) :: xs -> Continue { empty with stack = (f, b::bs)::xs }
  | _, (f, bs) :: ((f', bs') :: xs as rest) ->
     Continue { empty with stack=match build f bs with
                                 | None -> rest
                                 | Some r -> (f', r::bs')::xs }
  | _, [(f, bs)] -> (match build f bs with
                     | None -> Continue empty
                     | Some r -> Stop r)
  | _, [] -> raise (Invalid_argument "Empty stack!")
