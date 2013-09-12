# 1 "common.ml"
open Printf

let (!!) s = Printf.printf "%s" s

module type Meta =
sig 
  val id : string
  val title : string
  val date : string
  val xmldate : string
  val rssdate : string
  val tags : string list
  val disqus : bool
  val stamp : bool
end

module type Unit = sig end
module Unit : Unit = struct end

let input f =
  let ic = open_in f in
  let b = Buffer.create 42 in
  try while true do
      Buffer.add_char b (input_char ic)
    done;
    assert false
  with End_of_file ->
    (Buffer.contents b)

let cat f = !! (input f)

let command e =
  flush stdout;
  ignore(Sys.command e)

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

let input_command e =
  flush stdout;
  let tmp = Filename.temp_file ~temp_dir:"./" "tmp" "plop" in
  command (e ^ " > " ^ tmp);
  let r = input tmp in
  Sys.remove tmp;
  r
  
let selfbn = try Sys.getenv "selfbn" with Not_found -> ""
# 1 "tags/software development/index.md.ml"
 let _ = print_string ""
 # 1 "tags/software development/index.md.ml.mpp"
module Meta : Meta = struct
 let title = "blog#software development"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--index"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let rssdate = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "software development"; ]
 let stamp = false
 let disqus = false
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "# "
 let _ = !!title  let _ = print_string "\n\n"
 let _ = cat "tags/software development/index.contents.md"  let _ = print_string "\n"
 end  let _ = print_string ""
# 1 "ml_to_md.ml"
(* running Post(Something) must generate the main contents of the post *)
include Post(struct end)

let _ =
  if stamp then
  printf "<p style='font-size:80%%;'><em>started on %s, (re)generated on %s</em></p>" 
    date 
    (input_command "date --rfc-3339=seconds")
