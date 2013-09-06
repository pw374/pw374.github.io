 let _ = print_string ""
 module Meta : Meta = struct
 let title = "Projects"
 let id = "pw374.github.io--" ^ input_command "date +%Y-%m-%d-%H-%M-%S" ^ "--projects"
 let xmldate = input_command "date --rfc-3339=seconds|tr ' ' T"
 let date = input_command "date --rfc-3339=seconds|tr ' ' T"
 let tags = [ "projects"; ]
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "# "
 let _ = !!title  let _ = print_string "\nA list of projects...\n\n* omd\n* mpp\n* ...\n\n\n\n"
 end  let _ = print_string "\n"
