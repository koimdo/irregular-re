open Base

type ('a, 'b) t =
  | Finished of 'b
  | Continue of 'b option * ('a -> ('a, 'b) t)
  | Failed

let rec map p ~f = match p with
  | Finished t -> Finished (f t)
  | Continue (v, k) -> Continue (Option.map v ~f, fun a -> map (k a) ~f)
  | Failed -> Failed

let fail _ = Failed

let return a =
  let it = Some a in
  let rec loop = Continue (it, fun _ -> loop) in
  loop

let rec bind p ~f =
  match p with
  | Failed -> Failed
  | Finished r -> (match f r with
                   | Failed     | Continue (None, _)     -> Failed
                   | Finished r | Continue (Some r, _) -> Finished r)

  | Continue (None, k) -> Continue (None, fun a -> bind (k a) ~f)
  | Continue (Some r, k) ->
     (match f r with
      | Failed -> Failed
      | Finished r -> Finished r
      | Continue (v, k') -> Continue (v, fun a -> bind (k a) ~f:k'))

let rec both t t' =
  match t, t' with
  | Failed, _ | _, Failed -> Failed
  | Finished l, Finished r
  | Finished l, Continue (Some r, _)
  | Continue (Some l, _), Finished r -> Finished (l, r)
  | Finished _, Continue (None, _)
  | Continue (None, _), Finished _ -> Failed
  | Continue (v, k), Continue (v', k')
    -> Continue (Option.both v v', fun s -> both (k s) (k' s))

module Infix =struct
  let ( >>= ) x f = bind x ~f
end


let fu_kernel st a =
  match st with
  | Continue (_, k) -> (match k a with
                        | (Continue (_, _) as c) -> Continue_or_stop.Continue c
                        | s -> Continue_or_stop.Stop s)
  | _ -> raise (Invalid_argument "Stream ended!")

let unfold seed f =
  let rec more x _ = Continue (Some x, more (f x))
  in Continue (Some seed, (fun x -> more seed x))
