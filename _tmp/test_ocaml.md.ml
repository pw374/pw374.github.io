 let _ = print_string ""
 module Meta : Meta = struct
 let title = "this is only a test (it'll be deleted soon)"
 let id = "pw374.github.io--2013-09-06-20-21-41--27952"
 let xmldate = "2013-09-06T20:21:41+01:00"
 let rssdate = "Fri, 06 Sep 2013 20:21:41 +0100"
 let date = "2013-09-06 20:21:41+01:00"
 let tags = [ "ocaml"; "omd"; "markdown"; ]
 let disqus = true
 let stamp = true
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "## "
 let _ = !!title  let _ = print_string "\n\n```ocaml\nmodule Meta : Meta = struct\n let title = \"this is only a test (it'll be deleted soon)\"\n let id = \"pw374.github.io--2013-09-06-15-56-12--4361\"\n let xmldate = \"2013-09-06T15:56:12+01:00\"\n let date = \"2013-09-06 15:56:12+01:00\"\n let tags = [ \"ocaml\"; ]\nend\ninclude Meta\n```\n\n\n\n"
 end  let _ = print_string "\n"
