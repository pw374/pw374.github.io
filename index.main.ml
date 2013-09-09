# 0 "common.ml"
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

let input_command e =
  flush stdout;
  let tmp = Filename.temp_file ~temp_dir:"./" "tmp" "plop" in
  command (e ^ " > " ^ tmp);
  let r = input tmp in
  Sys.remove tmp;
  r
  
let selfbn = try Sys.getenv "selfbn" with Not_found -> ""
# 0 "index.md.ml"
 let _ = print_string ""
 module Meta : Meta = struct
 let title = "Philippe Wang"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--index"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let rssdate = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "root"; ]
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "# "
 let _ = !!title  let _ = print_string "\nI'm **trying** to make this website my new blog, using\n[OMD](https://github.com/pw374/omd/) and\n[MPP](https://github.com/pw374/MPP-language-blender/), with\n[OCaml](http://ocaml.org) of course, but also the (awful) HTML/CSS/JS\ntriplet.\n\n# Last blog posts\n\n"
 let _ = print_string "\n"
 let _ = cat "blogposts.list.tmp.md"  let _ = print_string "\n"
 end  let _ = print_string "\n"
# 0 "ml_to_md.ml"
(* running Post(Something) must generate the main contents of the post *)
include Post(struct end)

let _ =
  printf "<div><emph>started on %s, (re)generated on %s</emph></div>" 
    date 
    (input_command "date --rfc-3339=seconds")
