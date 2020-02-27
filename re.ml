open Base

let debug_prog = false

module Re = struct
  type ('a, 'b) t =
    | Eps
    | Lit of ('a -> bool) * string option
    | Cat of ('a, 'b) t * ('a, 'b) t
    | Alt of ('a, 'b) t * ('a, 'b) t
    | Star of ('a, 'b) t
    | Option of ('a, 'b) t
    | Plus of ('a, 'b) t
    | Capture of string * 'b Treebuilder.branch * ('a, 'b) t
    | Leaf of string * ('a, 'b) Treebuilder.leaf * ('a, 'b) t

  let eps = Eps
  let lit ?desc p = Lit (p, desc)
  let cat e e' = match e, e' with
    | Eps, e | e, Eps -> e
    | _, _ -> Cat (e, e')
  let alt e e' = Alt (e, e')
  let star = function
    | Eps -> Eps
    | (Star _) as e -> e
    | Option e | Plus e -> Star e
    | e -> Star e
  let option = function
    | Eps -> Eps
    | e -> Option e
  let plus = function
    | Eps -> Eps
    | e -> Plus e
  let capture ~name ~f e = Capture (name, f, e)
  let leaf ~name ~f e = Leaf (name, f, e)

  module Infix = struct
    let ( <.> ) r r' = cat r r'
    let ( <|> ) r r' = alt r r'
    let ( ~= ) s = String.fold s ~init:Eps
                     ~f:(fun re c -> cat re (Lit ((Char.equal c), Some (Printf.sprintf "%C" c))))
    let ( ~: ) p = lit p
    let ( #: ) desc p = lit ~desc p
    let ( ~* ) r = star r
    let ( ~? ) r = option r
    let ( ~+ ) r = plus r
    let ( .%{} ) (name, f) re = capture ~name ~f re
    let ( .={} ) (name, f) re = leaf ~name ~f re
  end
end

include Re

open Channel

module Interp = struct
  module Inst = struct
    type ('a, 'b) inst =
      | Done
      | Lit of ('a -> bool) * string option
      | Split of int * int
      | Jmp of int
      | Push of string * 'b Treebuilder.branch
      | Leaf of string * ('a, 'b) Treebuilder.leaf
      | Pop

    let to_string pc inst =
      match inst with
      | Done -> "DONE"
      | Lit (_, None) -> "LIT"
      | Lit (_, Some desc) -> Printf.sprintf "LIT [%s]" desc
      | Split (l, r) -> Printf.sprintf "SPLIT %d, %d" (l+pc) (r+pc)
      | Jmp l-> Printf.sprintf "JMP %d" (l+pc)
      | Push (s, _) -> Printf.sprintf "PUSH [%s]" s
      | Leaf (s, _) -> Printf.sprintf "LEAF [%s]" s
      | Pop -> "POP"

    let print_program prog =
      Array.iteri prog ~f:(fun pc inst -> Stdio.printf "%4d: %s\n" pc (to_string pc inst))
  end

  include Inst

  let compile e =
    let rec aux = function
      | Re.Eps -> []
      | Re.Lit (a, d) -> [Lit (a, d)]
      | Re.Cat (e, e') -> (aux e) @ (aux e')
      | Re.Alt (e, e') ->
         let cl = aux e
         and cr = aux e' in
         Split (1, 2+List.length cl) :: cl @ (Jmp (1+List.length cr)) :: cr
      | Re.Star e -> let c = (aux e) in
                     let lc = List.length c in
                     Split (1, lc+2) :: (c @ [Jmp (-(1+lc))])
      | Re.Option e -> let c = (aux e) in
                       let lc = List.length c in
                       Split (1, lc+1) :: c
      | Re.Plus e -> let c = (aux e) in
                     let lc = List.length c in
                     c @ [Split (-(lc), 1)]
      | Re.Capture (s, f, e) -> Push (s, f) :: (aux e) @ [Pop]
      | Re.Leaf (s, f, e)    -> Leaf (s, f) :: (aux e) @ [Pop]
    in
    List.to_array ((aux e) @ [Done] )

  type ('a, 'b) interp = { prog : ('a, 'b) inst array;
                           thread : int array;
                   }

  let rec scan thunks ch =
    let ( <|> ) o o' = match o with
      | Some _ -> o
      | _ -> o'
    in
    match List.fold (List.rev thunks)
            ~init:(None, [])
            ~f:(fun ((v', konts) as acc) thunk ->
              match thunk ch with
              | Continue (v, k) -> (v' <|> v, k::konts)
              | Finished v -> (v' <|> (Some v), konts)
              | Failed -> acc)
    with
    | (Some v), [] -> Finished v
    | None, [] -> Failed
    | o, (_::_ as konts) -> Continue (o, scan konts)

  exception CleanQueue
  let rec add_thread state pc sp matches res =
    let module Builder = Treebuilder in
    let final = ref None in
    let next = ref [] in
    let th = state.thread in
    let rec go pc result builder =
      if not Int.(th.(pc) = sp) then
        begin
          th.(pc) <- sp;
          match state.prog.(pc) with
          | Split (r, s) -> go (pc+r) result builder;
                            go (pc+s) result builder;
          | Jmp r -> go (pc+r) result builder
          | Push (_, f) -> go (pc+1) result (Builder.push builder ~f)
          | Leaf (_, f) -> go (pc+1) result (Builder.leaf builder ~f)
          | Pop -> (let open Continue_or_stop in
                    match Builder.pop builder with
                    | Continue b -> go (pc+1) result b
                    | Stop r -> go (pc+1) (Some r) Builder.empty)
          | Lit (a, _) ->
             let thunk ch =
               if (a ch) then
                 add_thread state (pc+1) (sp+1) (Builder.add builder ch) result
               else
                 Failed
             in
             next := thunk::!next
          | Done -> final := result;
                    raise CleanQueue (* Drop lower-priority threads *)
        end
    in
    (try go pc res matches with CleanQueue -> ());
    match !final, !next with
    | None, [] -> Failed
    | Some r, [] -> Finished r
    | o, sc -> Continue (o, scan sc)
end

let start re =
  let open Interp in
  let prog = compile re in
  let thread = Array.(create (-2) ~len:(length prog)) in
  let state = {prog; thread } in
  if debug_prog then
    Inst.print_program prog;
  add_thread state 0 0 Treebuilder.empty None

module Sexpbuilder = struct
  let leaf = function
    | [] -> None
    | l -> let b = Buffer.create 64 in
           List.iter l ~f:(Buffer.add_char b);
           Some (Sexp.Atom (Buffer.contents b))
  let branch s bs = Some (Sexp.(List ((Atom s)::bs)))  
end

module String = struct
  let run re s =
    let st = start re in
    let enumerate = Channel.unfold 0 (fun x -> x+1) in
    String.fold_until s
      ~init:(Channel.both enumerate st)
      ~f:Channel.fu_kernel
      ~finish:Fn.id
end
