let _ =
  List.iter
    (fun tag ->
       command ("mkdir -p '../tags/" ^ tag ^ "/'");
       command ("ln -fs \"../../posts/" ^ id ^ "\" '../tags/" ^ tag ^ "/'");
       command ("touch '../tags/" ^ tag ^ "/index'")
    )
    tags
