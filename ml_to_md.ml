(* running Post(Something) must generate the main contents of the post *)
include Post(struct end)

let _ =
  if stamp then
  printf "<p style='font-size:80%%;'><em>started on %s, (re)generated on %s</em></p>" 
    date 
    (input_command "date --rfc-3339=seconds")
