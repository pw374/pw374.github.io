 let _ = print_string ""
 module Meta : Meta = struct
 let title = "Blog"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--index"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let rssdate = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"
 let date = input_command "date --rfc-3339=seconds"
 let tags = [ ]
 let disqus = false
 let stamp = false
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "# "
 let _ = !!title  let _ = print_string "\nI'm trying to make this website my new blog, using OMD and MPP, \nwith OCaml of course, but also the (awful) HTML/CSS/JS triplet.\n\nHere follow the last blog posts\n\n"
 let _ = cat "blogposts.contents.tmp.md"  let _ = print_string "\n"
 end  let _ = print_string "\n"
