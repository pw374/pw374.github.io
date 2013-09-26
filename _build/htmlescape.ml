let _ =
try
  let rec esc () =
    match input_char stdin with
    | '&' | '<' | '>' | '\'' | '"' as c ->
      Printf.printf "&#%d;" (int_of_char c);
      esc ()
    | '\n' ->
      Printf.printf "\\n";
      esc ()
    | c -> 
      print_char c; esc ()
  in esc ()
with _ -> ()

