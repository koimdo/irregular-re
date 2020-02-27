open Base
open Re

let b s = (s, Sexpbuilder.branch s)
let l = ("Leaf", Sexpbuilder.leaf)
let ws = Infix.(~* (~=" " <|> ~="\t"))
let word = Infix.(~+ ("alphanum"#:Char.is_alphanum))
let listof re sep = Infix.(re <.> ~* (sep <.> re))
let phone = Infix.((b "phone").%{ l.={~+ ("digit"#:Char.is_digit)}})
let email = Infix.((b "email").%{ l.={word <.> ~="@" <.>  (listof word (~="."))} })


let record = Infix.((b "name").%{l.={listof word ws}} <.>
                   (~? (ws <.> ~=":" <.> ws <.>
                          (listof (phone <|> email) ws) <.> ws)) <.>
                   ~="\n")

let print_match re s =
  let report kind pos m =
    Stdio.printf "Match (%s) at %d:\n%s\n" kind pos Sexp.(to_string_hum m)
  in
  match String.run re s with
  | Failed -> Stdio.printf "No match!\n"
  | Finished (pos, m) -> report "FULL" pos m
  | Continue (Some (pos, m), _) -> report "PARTIAL" pos m
  | Continue (None, _) -> Stdio.printf "No match (yet)\n"

let run_file re argv =
  let open Stdio.In_channel in
  let s = match argv with
    | [|_|] -> input_all stdin
    | [|_; f|] -> read_all f
    | _ -> ""
  in
  print_match re s

let () =
  run_file Infix.((b "book").%{ ~* ((b "entry").%{record})}) (Sys.get_argv ())
