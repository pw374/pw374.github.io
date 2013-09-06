# 0 "common.ml"
open Printf

let (!!) s = Printf.printf "%s" s

module type Meta =
sig 
  val title : string
  val date : string
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
  
# 0 "_tmp/test_ocaml.md.ml"
 let _ = print_string ""
 module Meta : Meta = struct
 let title = "test ocaml"
 let id = "pw374.github.io--2013-09-06-15-56-12--4361"
 let xmldate = "2013-09-06T15:56:12+01:00"
 let date = "2013-09-06 15:56:12+01:00"
 let tags = [ "ocaml"; ]
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "## "
 let _ = !!title  let _ = print_string "\n```ocaml\nmodule Meta : Meta = struct\n let title = \"\"\n let id = \"pw374.github.io--2013-09-06-15-56-12--4361\"\n let xmldate = \"2013-09-06T15:56:12+01:00\"\n let date = \"2013-09-06 15:56:12+01:00\"\n let tags = [ \"ocaml\"; ]\nend\ninclude Meta\n```\n\n\n"
 end  let _ = print_string "\n"
# 0 "ml_to_md.ml"
(* running Post(Something) must generate the main contents of the post *)
include Post(struct end)

