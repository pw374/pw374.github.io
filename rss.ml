# 0 "common.ml"
open Printf

let (!!) s = Printf.printf "%s" s

module type Meta =
sig 
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
 let _ = print_string "<?xml version=\"1.0\"?>\n<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n  <channel>\n    <title>pw374</title>\n    <link>http://pw374.github.io/</link>\n    <atom:link href=\"http://pw374.net/rss.xml\" rel=\"self\" type=\"application/rss+xml\" />\n    <description>pw374 blog</description>\n    <language>en-gb</language>\n    <pubDate>"
 let _ = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"  let _ = print_string "</pubDate>\n    <lastBuildDate>"
 let _ = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"  let _ = print_string "</lastBuildDate>\n\n    "
 let _ = cat (Sys.getenv "contents")  let _ = print_string "    \n\n  </channel>\n</rss>\n\n"
