let read str =
  read_line str


let eval _env ast =
  ast


let print exp =
  print_endline exp


let rep x =
  x |> read |> eval None |> print


let main =
  try
    while true do
      print_string "user> " ;
      rep ();
    done
  with End_of_file -> ()
