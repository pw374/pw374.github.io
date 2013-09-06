(* running Post(Something) must generate the main contents of the post *)
include Post(struct end)

let _ =
  List.iter 
    (fun tag ->
       ignore(Sys.command("mkdir -p tags/" ^ tag))
    )
    tags
