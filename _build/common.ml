open Printf

module type Meta =
sig 
  val id : string
  val title : string
  val date : string
  val xmldate : string
  val xmldate_updated : string
  val rssdate : string
  val tags : string list
  val disqus : bool
  val stamp : bool
  val tryocaml : bool
end

let htmlescape s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '&' | '<' | '>' | '\'' | '"' as c ->
      Printf.bprintf b "&#%d;" (int_of_char c)
    | c -> 
      Buffer.add_char b c
  done;
  Buffer.contents b

module type Printer = sig val (!!) : string -> unit val print_string : string -> unit val to_string : string -> string end
module Printer : Printer = struct include Pervasives let to_string x = x let (!!) = print_string end
module Eprinter : Printer = struct let print_string s = print_string (htmlescape s) let (!!) = print_string let to_string = htmlescape end

let input f =
  let ic = open_in f in
  let b = Buffer.create 42 in
  try while true do
      Buffer.add_char b (input_char ic)
    done;
    assert false
  with End_of_file ->
    (Buffer.contents b)

let cat f = print_string (input f)

let command e =
  flush stdout;
  ignore(Sys.command e)

let input_command e =
  flush stdout;
  let tmp = Filename.temp_file ~temp_dir:"./" "tmp" "plop" in
  command (e ^ " > " ^ tmp);
  let r = input tmp in
  Sys.remove tmp;
  r
  
let selfbn = try Sys.getenv "selfbn" with Not_found -> ""

let precontents () = ()
let html = false


let tag_post stamp date tags = 
  print_string "<hr/>";
  if stamp then
  printf "<p style='font-size:80%%;'><em>started on %s, (re)generated on %s</em></p>" 
    date
    (input_command "date --rfc-3339=seconds");
  print_string "<p>";
  List.iter
    (fun tag ->
      printf "• <a href='/%s/'>%s</a> " tag tag
    )
    tags;
  print_string "</p>"
