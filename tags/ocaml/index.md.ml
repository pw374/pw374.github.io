 let _ = print_string "# 1 \"tags/ocaml/index.md.ml.mpp\"\n"
 module Meta : Meta = struct
 let title = "blog#ocaml"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--index"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let rssdate = input_command "date '+%a, %d %b %Y %H:%M:%S %z'"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "ocaml"; ]
 let stamp = false
 let disqus = false
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "# "
 let _ = !!title  let _ = print_string "\n\n"
 let _ = cat "tags/ocaml/index.contents.md"  let _ = print_string "\n"
 end  let _ = print_string ""
