(* running Post(Something) must generate the main contents of the post *)
include Post(struct end)

let _ =
  printf "<div><emph>started on %s, (re)generated on %s</emph></div>" 
    date 
    (input_command "date --rfc-3339=seconds")
