let _ =
  List.iter
    (fun tag ->
       command ("mkdir -p 'tags/" ^ tag ^ "/'");
       command ("ln -fs \"../../$self\" 'tags/" ^ tag ^ "/'");
       command ("touch 'tags/" ^ tag ^ "/index'")
    )
    tags

let _ = exit 0
