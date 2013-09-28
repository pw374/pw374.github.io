open Lexer

let _ =
  print_string (string_of_tl (read (stream_of_inchannel stdin)))
