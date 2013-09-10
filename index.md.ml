 let _ = print_string ""
 module Meta : Meta = struct
 let title = "Philippe Wang"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--index"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let rssdate = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "root"; ]
 let disqus = false
 let stamp = false
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "# "
 let _ = !!title  let _ = print_string "\nI'm **trying** to make this website my new blog, using\n[OMD](https://github.com/pw374/omd/) and\n[MPP](https://github.com/pw374/MPP-language-blender/), with\n[OCaml](http://ocaml.org) of course, but also the (awful) HTML/CSS/JS\ntriplet.\n\n# Last blog posts\n\n"
 let _ = print_string "\n"
 let _ = cat "blogposts.list.tmp.md"  let _ = print_string "\n"
 end  let _ = print_string "\n"
