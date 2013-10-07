let _ =
  List.iter
    (fun tag ->
       command ("mkdir -p '../tags/" ^ tag ^ "/'");
       command ("ln -fs \"../../" ^ id ^ "\" '../tags/" ^ tag ^ "/'");
    )
    tags
