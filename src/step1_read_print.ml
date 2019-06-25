let read str =
  Reader.read_str str

let eval _env ast =
  ast

let print exp =
  Printer.print_str exp

let rep str =
  str |> read |> eval None |> print

let main =
  try
    while true do
      print_string "user> " ;
      try
        rep (read_line ()) ;
      with 
      | Reader.Nothing ->
        ()

      | Reader.ReaderErr err ->
        print_endline err
    done
  with End_of_file -> ()
