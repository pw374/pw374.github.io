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


let precontents () = ()
let html = false
# 1 "projects.md.ml"
 let _ = print_string ""
 module Meta : Meta = struct
 let title = "Projects"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--projects"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let rssdate = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "projects"; ]
 let disqus = false
 let stamp = false
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "# "
 let _ = !!title  let _ = print_string "\nA list of projects...\n\n* omd\n* mpp\n* ...\n\n\n\n"
 end  let _ = print_string "\n"
let html = true
 let _ = print_string "<!DOCTYPE HTML>\n<html>\n  <head>\n    <meta charset=\"utf-8\" />\n    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n    <title>"
 let _ = !!title  let _ = print_string " &ndash; pw374</title>\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />\n    <!-- Google Web Fonts -->\n    <link href=\"http://fonts.googleapis.com/css?family=Lato:400,700,400italic,700italic\" rel=\"stylesheet\">\n    <link href=\"http://fonts.googleapis.com/css?family=Domine:400,700\" rel=\"stylesheet\">\n    <!-- Only part of Bootstrap that we don't load from a CDN is our own customized CSS build. -->\n    <link href=\"/css/bootstrap.css\" rel=\"stylesheet\" media=\"screen\">\n"
 let _ = if selfbn <> "" then printf
"    <link rel=\"alternate\" type=\"application/atom+xml\" title=\"ATOM Feed\" href=\"/%s-atom.xml\">
    <link rel=\"alternate\" type=\"application/rss+xml\" title=\"RSS Feed\" href=\"/%s-rss.xml\">
" selfbn selfbn
 let _ = print_string "    <!--[if lt IE 9]>\n        <script src=\"//cdnjs.cloudflare.com/ajax/libs/html5shiv/3.6.2/html5shiv.js\"></script>\n    <![endif]-->\n  </head>\n  <body>\n    <nav class=\"navbar navbar-inverse navbar-fixed-top\">\n      <div class=\"navbar-inner\">\n        <div class=\"container-fluid\">\n          <button type=\"button\" class=\"btn btn-navbar\" data-toggle=\"collapse\" data-target=\".nav-collapse\">\n            <span class=\"icon-bar\"></span>\n            <span class=\"icon-bar\"></span>\n            <span class=\"icon-bar\"></span>\n          </button>\n          <a class=\"brand\" href=\"/\"><img src=\"https://1.gravatar.com/avatar/bac6aa3e290e47c0aeeb0c6b826d9fa4?d=https%3A%2F%2Fidenticons.github.com%2F81952a25f2c72f330d4eaf04cdbe89e4.png&s=36\" alt=\"[img]\"></a>\n          <div class=\"nav-collapse collapse\">\n            <ul class=\"nav\">\n              <li "
 let _ = if List.mem "root" tags then print_string "class='active'"  let _ = print_string "><a href=\"/\">root</a></li>\n              <li "
 let _ = if List.mem "projects" tags then print_string "class='active'"  let _ = print_string "><a href=\"/projects.html\">projects</a></li>\n              <li "
 let _ = if not (List.mem "root" tags || List.mem "projects" tags || List.mem "ocaml" tags) then print_string "class='active'"  let _ = print_string "><a href=\"/blog.html\">blog</a></li>\n              <li "
 let _ = if List.mem "ocaml" tags then print_string "class='active'"  let _ = print_string "><a href=\"/tags/ocaml/\">#ocaml</a></li>\n            </ul>\n            <!-- <form class=\"navbar-search pull-right\"> -->\n            <!--   <input class=\"search-query\" type=\"text\" placeholder=\"Search\" /> -->\n            <!-- </form> -->\n          </div>\n        </div>\n      </div>\n    </nav>\n    <div class=\"container\">\n      <div class=\"row\">\n        <div class=\"span4\">\n          <nav id=\"nav-secondary\">\n            <ul class=\"nav nav-list\">\n              <li class=\"nav-header\"><a href=\"#\">"
 let _ = if List.mem "root" tags then print_string "Tags" else print_string "Table of Contents"  let _ = print_string "</a></li>\n              "
 let _ = cat (Sys.getenv "toc")  let _ = print_string "            </ul>\n          </nav>\n        </div>\n        <div id=\"content-primary\" class=\"span8\">\n          <div class=\"content\">\n            "
 let _ = cat (Sys.getenv "contents")  let _ = print_string "          </div>\n"
 let _ = if disqus then print_string "
          <div id=\"disqus_thread\">
          <script type=\"text/javascript\">
            /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
            var disqus_shortname = 'pw374'; // required: replace example with your forum shortname
            
            /* * * DON'T EDIT BELOW THIS LINE * * */
            (function() {
            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
            dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
            })();
          </script>
          <noscript>Please enable JavaScript to view the <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
          <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>
          </div>
"  let _ = print_string "        </div>\n      </div>\n    </div>\n    <footer id=\"footer\" class=\"navbar navbar-inverse navbar-fixed-bottom\">\n      <div class=\"navbar-inner\">\n        <div class=\"container-fluid\">\n          <ul class=\"nav pull-right\">\n            <li><a href=\"https://github.com/pw374\">on GitHub</a></li>\n          </ul>\n        </div>\n      </div>\n    </footer>\n    <!-- Load javascript from CDN -->\n    <script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js\"></script>\n    <script src=\"http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.1/js/bootstrap.min.js\"></script>\n  </body>\n</html>\n"
