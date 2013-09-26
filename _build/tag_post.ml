let _ =
  !! "<p>";
  !! "tags:";
  List.iter
    (fun tag ->
      printf "<a href='/%s/'>%s</a>" tag tag
    )
    tags;
  !! "</p>"

