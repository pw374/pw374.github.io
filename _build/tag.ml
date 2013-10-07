let _ =
  List.iter
    (fun tag ->
       command ("mkdir -p '../tags/" ^ tag ^ "/'");
       command ("ln -fs \"../../posts/" ^ id ^ "\" '../tags/" ^ tag ^ "/'");
    )
    tags
