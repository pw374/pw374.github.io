module
Mpp_charset
= struct
(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
include Set.Make(Char) 

let of_list l =
  List.fold_left (fun r e -> add e r) empty l 

let to_string s =
  let b = Buffer.create 42 in
    iter (Buffer.add_char b) s;
    Buffer.contents b

let to_escaped_string s =
  let b = Buffer.create 42 in
    iter (fun c -> Buffer.add_string b (Char.escaped c)) s;
    Buffer.contents b

end
module
Mpp_charstream
= struct
(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
(* open Mpp_init *)

let debug = ref false

type charstream = {
  take : unit -> char option;
  push : char -> unit;
  insert : charstream -> unit;
  where : unit -> location;
}
and filename = string
and location = filename*line*column
and line = int
and column = int
type t = charstream


let parse_error : ?start:location -> ?msg:string -> location -> unit = 
  fun ?start ?msg location ->
    let f, l, c = location in
      match start with
        | None ->
            begin match msg with
              | None -> 
                  Printf.eprintf
                    "Error in %s:%d:%d.\n%!"
                    f l c
              | Some m ->
                  Printf.eprintf
                    "Error: %s\nIn %s:%d:%d.\n%!"
                    m f l c
            end
        | Some(filename,line,column) ->
            begin match msg with
              | None ->
                  if l <> 0 && c <> 0 then
                    Printf.eprintf
                      "Error from %s:%d:%d to %s:%d:%d.\n%!"
                      filename line column f l c
                  else
                    Printf.eprintf
                      "Error from %s:%d:%d to end of file.\n%!"
                      filename line column
              | Some m ->
                  if l <> 0 && c <> 0 then
                    Printf.eprintf
                      "Error: %s\nFrom %s:%d:%d to %s:%d:%d.\n%!"
                      m filename line column f l c
                  else
                    Printf.eprintf
                      "Error: %s\nFrom %s:%d:%d to end of file.\n%!"
                      m filename line column
            end


let output_charstream out c =
  let rec loop () =
    match c.take () with
      | None -> ()
      | Some c -> output_char out c; loop()
  in loop()

module Mpp_charset = Mpp_charset

let charstream_take_n n charstream =
  let b = Buffer.create n in
    for i = 1 to n do
      match charstream.take() with
        | None -> ()
        | Some c -> Buffer.add_char b c
    done;
    Buffer.contents b

let charstream_peek ?(n=1) charstream =
  let limit = ref n in
  let b = Buffer.create n in
    for i = 1 to n do
      match charstream.take() with
        | Some c -> Buffer.add_char b c
        | None -> limit := min !limit (pred i)
    done;
    let res = Buffer.contents b in
      for i = !limit - 1 downto 0 do
        let x = res.[i] in
          charstream.push x
            (* try charstream.push x with e ->  *)
(*               Printf.printf "WTF: %s%!\n" (Printexc.to_string e); *)
(*               assert false *)
      done;
      res

let rec charstream_of_inchannel filename ?(line=1) ?(column=0) inchan =
  (* Eventually, we might want to get rid of the "double definition". *)
  let buffer : char list ref = ref [] in
  let line = ref line in
  let column = ref [column] in
  let incr_column () =
    (match !column with
       | [] -> assert false;
       | x :: tl -> column := (x+1) :: tl
    )
  in
  let where () =
    match !column with
      | [] -> filename, !line, 0 (* assert false *)
      | c::_ -> filename, !line, c
  in
  let take () =
    match !buffer with
      | [] ->
          begin
            try match input_char inchan with
              | '\n' | '\r' as c -> 
                  incr line;
                  column := 0 :: !column;
                  Some c
              | c -> 
                  incr_column();
                  Some c
            with End_of_file -> None
          end
      | c::tl ->
          match c with 
            | '\n' | '\r' -> 
                incr line;
                column := 0 :: !column;
                buffer := tl;
                Some c
            | c -> 
                incr_column();
                buffer := tl;
                Some c
  in
  let push c =
    buffer := c :: !buffer
  in
  let csl = ref [{take;push;where;insert=fun _ -> assert false}] in
  let insert cs =
    csl := cs :: !csl
  in
  let rec where () =
    match !csl with
      | [] -> 
          filename, !line, (match !column with x::_ -> x| _ -> 0)
      | e::_ -> e.where()
  in
  let rec take () =
    match !csl with
      | [] -> None
      | e::tl ->
          match e.take() with
            | None -> csl := tl; take()
            | Some _ as res -> res
  in
  let rec push c =
    match !csl with
      | [] ->
          csl := [charstream_of_inchannel "/dev/null" (open_in "/dev/null")];
          push c
      | e::tl ->
          begin
            match c with
              | '\n' | '\r' ->
                  begin
                    decr line;
                    match !column with
                      | [] -> 
                          column := [0]
                            (* assert false; *)
                          (* If this happens, either this block is broken 
                             or too many characters have been pushed back,
                             in both cases the program is broken and has to be fixed. *)
                      | _::tl -> column := tl
                  end
              | _ ->
                  match !column with
                    | cl::cls -> column := pred cl::cls
                    | [] -> column := [-1]
          end;
          e.push c
  in
    { take ; push ; insert ; where }

let charstream_of_string  ?(location:location=("<anon-string>",0,0)) (s:string) : charstream =
  (*It's too inconvenient to keep an optimal complete version of this
    function, so I chose to rely on [charstream_of_inchannel], **for
    now**.  Also, it is not good to write the data in a
    file... especially because I never delete the file, so it is not
    safe at all. If I keep relying on [charstream_of_inchannel], then
    I might use Unix.pipe and perhaps threads as well because writing
    to a file descriptor is a blocking operation, however threads are
    not convinient at all.  *)
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let () = at_exit (fun () -> Sys.remove tmp) in
  let octmp = open_out tmp in
  let () = output_string octmp s in
  let () = close_out octmp in
  let name, line, column = location in
    charstream_of_inchannel name ~line:line ~column:column (open_in tmp)


let match_token token charstream =
  if !debug then 
    (let _filename, _line, _col = charstream.where () in
       Printf.eprintf "<token=%s@%d-%d----'%s'>\n%!" token _line _col (String.escaped(charstream_peek ~n:20 charstream)));
  let res =
    token <> "" 
    &&
      let rec loop i taken =
        if i >= String.length token then
          true, []
        else
          match charstream.take() with
            | None ->
                false, taken
            | Some c ->
                if token.[i] = c then 
                  loop (succ i) (c::taken)
                else
                  false, (c::taken)
      in match loop 0 [] with
        | true, _ ->
            true
        | false, taken ->
            List.iter charstream.push taken; (* do not List.rev!! *)
            false
  in
    res


let rec eat (cs:Mpp_charset.t) charstream =
  match charstream.take() with
    | None ->
        ()
    | Some c -> 
        if Mpp_charset.mem c cs then 
          eat cs charstream
        else
          charstream.push c            


let read_until ?(caller="") ?(failsafe=false) c charstream : string =
  if !debug then Printf.eprintf "read_until '%s'\n%!" (Char.escaped c);
  let b = Buffer.create 128 in
  let rec loop () =
    match charstream.take() with
      | Some z ->
          let () =
            if !debug then 
              Printf.eprintf "Peek<%s>\n%!"
                (String.escaped (charstream_peek ~n:20 charstream)) 
          in
            if c = z then
              begin
                charstream.push z;
                Buffer.contents b
              end
            else
              begin
                Buffer.add_char b z;
                loop ()
              end
      | None ->
          if failsafe then
            Buffer.contents b
          else
            begin
              parse_error
                ~msg:(Printf.sprintf "Couldn't read far enough. I could read <%s>." (Buffer.contents b))
                (charstream.where());
              exit 1
            end
  in loop ()

let read_until_one_of ?(caller="") ?(failsafe=false) ?(push_back=false) (cs:Mpp_charset.t) ?(exclude=Mpp_charset.empty) ?(expect:string option) charstream =
  if !debug then Printf.eprintf "read_until_one_of [%s]\n%!" (Mpp_charset.fold (fun c r -> Printf.sprintf "%s%s" r (Char.escaped c)) cs ""); 
  let b = Buffer.create 128 in
  let rec loop () =
    match charstream.take() with
      | Some z ->
          if Mpp_charset.mem z cs  then
            begin
              if push_back then charstream.push z;
              Buffer.contents b
            end
          else if Mpp_charset.mem z exclude then
            begin
              match expect with
                | None ->
                    parse_error ~msg:(Printf.sprintf "Forbidden character: <%s>" (Char.escaped z)) (charstream.where());
                    exit 1
                | Some m ->
                    parse_error ~msg:(Printf.sprintf "Forbidden character: <%s>. Expected: <%s>" (Char.escaped z) m) (charstream.where());
                    exit 1
            end
          else
            begin
              Buffer.add_char b z;
              loop ()
            end
      | None ->
          if failsafe then
            Buffer.contents b
          else
            begin
              parse_error
                ~msg:(Printf.sprintf "Couldn't read far enough. I could read <%s>. I was looking for [%s].%s" (Buffer.contents b) (Mpp_charset.to_escaped_string cs) (if caller <> "" then Printf.sprintf " Caller: %s." caller else ""))
                (charstream.where());
              exit 1
            end
  in loop ()


(* This has to be patched to accept other integers than natural numbers. *)
let parse_int charstream =
  if !debug then Printf.eprintf "parse_int\n%!";
  (*   let start = charstream.where() in *)
  let res = Buffer.create 42 in
  let rec loop () =
    match charstream.take() with
      | Some ('0' .. '9' as c) -> Buffer.add_char res c; loop()
      | Some '_' -> loop()
      | Some c -> charstream.push c
      | None -> ()
  in
    loop ();
    int_of_string (Buffer.contents res)


(* Reads until an exact word is found.
   The word should be deleted from the charstream. *)
let read_until_word ?(failsafe=false) ?(success=ref true) charstream word =
  assert(word<>"");
  let start_location = charstream.where() in
  let res = Buffer.create (String.length word * 8) in
  let buf = Buffer.create (String.length word) in
  let rec loop i =
    if i >= String.length word then
      begin
        success := true;
        Buffer.contents res
      end
    else
      match charstream.take() with
        | None ->
            success := false;
            if failsafe then
              Buffer.contents res
            else
              begin
                parse_error
                  ~start:start_location
                  ~msg:(Printf.sprintf "Could not reach the end of block ending with \"%s\"." (String.escaped word))
                  (charstream.where());
                exit 1
              end
        | Some c ->
            if c = word.[i] then
              begin
                Buffer.add_char buf c;
                loop (succ i)
              end
            else
              begin
                Buffer.add_buffer res buf;
                Buffer.clear buf;
                Buffer.add_char res c;
                loop 0
              end
  in
    loop 0


let split_on_char ?(keep_empty_strings=false) c s =
  let buf = Buffer.create 42 in
  let sl = String.length s in
  let make_res res =
    match Buffer.contents buf with
      | "" -> if keep_empty_strings then "" :: res else res
      | b -> b :: res
  in
  let rec loop index res =
    if index >= sl then
      make_res res
    else if s.[index] = c then
      let l = make_res res in (*here, order is important*)
        Buffer.clear buf;
        loop (index+1) l
    else
      begin
        Buffer.add_char buf s.[index];
        loop (index+1) res
      end
  in List.rev (loop 0 [])

(* let _ = split_on_char 'c' "ncdiunuidscnicndsncdisnciudnplop";;
   let _ = split_on_char ' ' "  eizbez ";;    *)

let parse_a_string cs =
  let location = cs.where() in
  let b = Buffer.create 10 in
  let rec loop() =
    match cs.take() with
      | None -> Buffer.contents b
      | Some '"' -> Buffer.contents b
      | Some '\\' ->
          begin match cs.take() with
            (* | '"' as c -> Buffer.add_char b c *)
            | Some ('\\' as c) -> Buffer.add_char b c
            | Some 'n' -> Buffer.add_char b '\n'
            | Some 'b' -> Buffer.add_char b '\b'
            | Some 'r' -> Buffer.add_char b '\r'
            | Some 't' -> Buffer.add_char b '\t'
            | Some '\n' -> ()
            | Some (('x'|'X'| '0' .. '2') as c0) ->
                begin
                  match cs.take() with
                    | Some ('A' .. 'F' | 'a' .. 'f' | '0' .. '9' as c1) -> 
                        begin match cs.take() with
                          | Some ('A' .. 'F' | 'a' .. 'f' | '0' .. '9' as c2) ->
                              let s = "123" in
                                s.[0] <- c0; s.[1] <- c1; s.[2] <- c2;
                                Buffer.add_char b (char_of_int(int_of_string s))
                          | Some _ | None -> parse_error ~msg:"Error when parsing a string." location
                        end
                    | Some _ | None -> parse_error ~msg:"Error when parsing a string." location
                end
            | Some c -> Buffer.add_char b c
            | None -> ()
          end;
          loop()
      | Some c ->
          Buffer.add_char b c;
          loop()
  in 
    loop()


(* In case one would want to use the standard OCaml Stream module. *)
let stream_of_charstream (cs:charstream) : char Stream.t =
  Stream.from (fun _ -> cs.take())


let append cs1 cs2 =
  let current_is_cs1 = ref true in
    {
      take  =
        (fun () -> 
           if !current_is_cs1 then
             match cs1.take() with
               | (Some _) as c ->
                   c
               | None ->
                   current_is_cs1 := false; 
                   cs2.take()
           else
             cs2.take()
        );
      push   = (fun c -> if !current_is_cs1 then cs1.push c else cs2.push c);
      insert = (fun c -> if !current_is_cs1 then cs1.insert c else cs2.insert c);
      where  = (fun () -> if !current_is_cs1 then cs1.where() else cs2.where())
    }

let delete_trailing_spaces s =
  if s = "" then
    s
  else
    let l = ref (String.length s) in
      while 
        (match s.[!l - 1] with
          | '\n' | '\t' | ' ' | '\r' -> true
          | _ -> false)
      do
        decr l
      done;
      if !l = String.length s then
        s
      else
        String.sub s 0 !l
          


let string_of_charstream ?(keepcs=false) c =
  let x = c.where() in
  let b = Buffer.create 42 in
  let rec loop () =
    match c.take () with
      | None -> Buffer.contents b
      | Some c -> Buffer.add_char b c; loop()
  in
  let res = loop() in
    match keepcs with
      | true ->
          c.insert (charstream_of_string ~location:x res);
          res
      | false ->
          res
end
module
Mpp_out
= struct
(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

type t = 
    | Buffer of Buffer.t
    | Out_channel of out_channel

let output_buffer o b = match o with
  | Buffer b' -> Buffer.add_buffer b' b
  | Out_channel oc -> Buffer.output_buffer oc b

let flush o = match o with
  | Buffer _ -> ()
  | Out_channel oc -> Pervasives.flush oc

let output_string o s = match o with
  | Buffer b -> Buffer.add_string b s
  | Out_channel oc -> Pervasives.output_string oc s

let output_char o c = match o with
  | Buffer b -> Buffer.add_char b c
  | Out_channel oc -> Pervasives.output_char oc c

let printf o fmt =
  (* This printf function is provided by Benoît Vaugon *)
  let contains_flush fmt =
    let s = string_of_format fmt in
    let n = String.length s in
    let rec f i =
      if i >= n - 1 then false else
        match s.[i], s.[i+1] with
          | '%', '!' -> true
          | '%',   _ -> f (i + 2)
          |   _,   _ -> f (i + 1)
    in f 0
  in
    match o with
      | Buffer b -> Printf.bprintf b fmt
      | Out_channel oc ->
          let b = Buffer.create 16 in
          let k b =
            Pervasives.output_string oc (Buffer.contents b);
            if contains_flush fmt then Pervasives.flush oc
          in
            Printf.kbprintf k b fmt

let output_charstream o cs =
  match o with
    | Buffer buff -> Buffer.add_string buff (Mpp_charstream.string_of_charstream ~keepcs:true cs)
    | Out_channel o -> Pervasives.output_string o (Mpp_charstream.string_of_charstream ~keepcs:true cs)


let cat (out:t) filename =
  if Sys.file_exists filename then
    let i = open_in filename in
      try while true do
        output_char out (input_char i)
      done with End_of_file -> ()
  else
    Printf.eprintf
      "builtin cat error: file <%s> doesn't exist.\n%!"
      filename
end
module
Mpp_stringmap
= struct
(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
include Map.Make(String)
end
module
Mpp_actions
= struct
(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
open Mpp_charstream
module Out = Mpp_out

let space_chars = ref (Mpp_charset.empty)
let blank_chars = ref (Mpp_charset.empty)
let debug = ref false

let ignore_non_existing_commands = ref false

let main_process : (charstream -> Out.t -> unit) ref = ref (fun _ _ -> assert false)

type action = last_condition -> nesting -> action_args -> charstream ->  Out.t -> unit
and action_args = charstream
and nesting = bool
and last_condition = bool option ref

type  action_set = (action * documentation) Mpp_stringmap.t
and documentation = string

let actions :  action_set ref = ref Mpp_stringmap.empty

let ignore_exec_error = ref false


let is_lazy, register_lazy =
  let module SS = Set.Make(String) in
  let lazy_set = ref SS.empty in
    ((fun action_name ->
        SS.mem action_name !lazy_set),
     (fun action_name -> 
        lazy_set := SS.add action_name !lazy_set
     ))

(* *********************************************************** *)
(* **BEGIN LIBRARY ******************************************* *)

(* BEGIN VARIABLES *)
(* variable environment *)
type set = string Mpp_stringmap.t

let environment : set = Mpp_stringmap.empty

module Mpp_conditions = struct
  let elze last_cond nesting (s:charstream) cs out =
    match !last_cond with
      | None ->
          Mpp_charstream.parse_error ~msg:"`else' without a previous matching conditional."
            (cs.Mpp_charstream.where());
          Pervasives.exit 1
      | Some c ->
          last_cond := None;
          if c then
            ()
          else
            begin
              if nesting then
                begin
                  let b1 = Buffer.create 42 in
                    !main_process s (Out.Buffer b1);
                    Out.output_buffer out b1;
                    (* Out.output_char out '\n'; *)
                    let b2 = Buffer.create 42 in
                      !main_process cs (Out.Buffer b2);
                      Out.output_buffer out b2;
                end
              else
                begin
                  Out.output_charstream out s;
                  (* Out.output_char out '\n'; *)
                  Out.output_charstream out cs;
                end
            end
end


module Variable : sig
  val set: charstream -> charstream -> 'ignored -> unit
  val get: charstream -> charstream ->  Out.t -> unit
  val tryget: charstream -> charstream ->  Out.t -> unit
  val unset: charstream -> charstream -> 'ignored -> unit
  val unsetall: 'string -> 'charstream ->  Out.t -> unit
  val ifdef: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
  val ifndef: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
  val elzeifdef: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
  val elze: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
end = struct
  include Mpp_conditions
  include Map.Make(String)
  let suppress_spaces s =
    let b = Buffer.create (String.length s - 1) in
      for i = 0 to String.length s - 1 do
        match s.[i] with
          | ' ' | '\t' | '\n' | '\r' ->
              ()
          | c -> Buffer.add_char b c
      done;
      Buffer.contents b

  let env = ref empty

  let unsetall _s _cs _out = env := empty

  let set s cs _ =
    let variable =
      read_until_one_of ~failsafe:true !blank_chars s
    in
    let value = 
      match string_of_charstream cs with
        | "" -> string_of_charstream s
        | x -> 
            string_of_charstream s ^ (* "\n" ^ *) x
    in
      env := add variable value !env

  let get s cs out =
    let s = suppress_spaces (string_of_charstream s) in
      try
        Out.output_string out (find s !env)
      with Not_found ->
        parse_error
          ~msg:(Printf.sprintf "You tried to get the value of variable %s, which doesn't exist (1)." s) 
          (cs.where());
        Pervasives.exit 1

  let tryget s cs out =
    let s = suppress_spaces (string_of_charstream s) in
      try
        Out.output_string out (find s !env)
      with Not_found ->
        ()

  let unset s cs _ =
    let s = suppress_spaces (string_of_charstream s) in
      try
        env := remove s !env
      with Not_found ->
        parse_error
          ~msg:(Printf.sprintf "You tried to get the value of variable %s, which doesn't exist (2)." s) 
          (cs.where());
        Pervasives.exit 1


  let ifdef last_cond nesting (cs:charstream) bcs out =
    if !debug then Printf.eprintf "ifdef <%s> <%s>\n%!" (string_of_charstream ~keepcs:true cs) (String.escaped (string_of_charstream ~keepcs:true bcs));
    let s:string = read_until_one_of ~failsafe:true ~caller:"ifdef" !blank_chars cs in
      assert (s<>"");
      try
        begin
          ignore(find s !env); (*raises Not_found if not found*)
          last_cond := Some true;
          if nesting then
            begin
              let b1 = Buffer.create 42 in
                !main_process cs (Out.Buffer b1);
                Out.output_buffer out b1;
                (* Out.output_char out '\n'; *)
                let b2 = Buffer.create 42 in
                  !main_process bcs (Out.Buffer b2);
                  Out.output_buffer out b2;
            end
          else
            begin
              Out.output_charstream out cs;
              (* Out.output_char out '\n'; *)
              Out.output_charstream out bcs;
            end
        end
      with Not_found -> 
        last_cond := Some false

  let ifndef last_cond nesting (cs:charstream) bcs out =
    if !debug then Printf.eprintf "ifdef <%s> <%s>\n%!" (string_of_charstream ~keepcs:true cs) (String.escaped (string_of_charstream ~keepcs:true bcs));
    let s:string = read_until_one_of ~failsafe:true ~caller:"ifndef" !blank_chars cs in
      assert(s<>"");
      try
        begin
          ignore(find s !env); (*raises Not_found if not found*)
          last_cond := Some false;
        end
      with Not_found -> 
        last_cond := Some true;
        if nesting then
          begin
            let b1 = Buffer.create 42 in
              !main_process cs (Out.Buffer b1);
              Out.output_buffer out b1;
              (* Out.output_char out '\n'; *)
              let b2 = Buffer.create 42 in
                !main_process bcs (Out.Buffer b2);
                Out.output_buffer out b2;
          end
        else
          begin
            Out.output_charstream out cs;
            (* Out.output_char out '\n'; *)
            Out.output_charstream out bcs;
          end

  let elzeifdef (last_cond:bool option ref) (nesting:bool) (s:charstream) (cs:charstream) (out:Out.t) =
    match !last_cond with
      | Some c ->
          if c then
            ()
          else
            ifdef last_cond nesting s cs out
      | None ->
          parse_error ~msg:"`elseifdef' without a matching previous `if'."
            (cs.where());
          Pervasives.exit 1
end
  (* / END VARIABLES *)


let last_cmd = ref 0

let command arg charstream (out:Out.t) =
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let otmp = open_out tmp in
  let tmp2 = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp2" "plop" in
    output_charstream otmp charstream;
    close_out otmp;
    let ec = Sys.command ("( cat " ^ tmp ^ " | " ^ string_of_charstream arg ^ " ) > " ^ tmp2 ) in
    let () = Out.cat out tmp2 in
      Sys.remove tmp;
      Sys.remove tmp2;
      last_cmd := ec;
      Out.flush out;
      ec

let ifcmd last_cond nesting arg charstream out =
  if !debug then 
    Printf.eprintf "ifcmd <%s> <%s>\n%!"
      (String.escaped (string_of_charstream ~keepcs:true arg)) (String.escaped (string_of_charstream ~keepcs:true charstream));
  if !last_cmd = 0 then
    begin
      last_cond := Some true;
      if nesting then
        begin
          let b1 = Buffer.create 42 in
            !main_process arg (Out.Buffer b1);
            Out.output_buffer out b1;
            (* Out.output_char out '\n'; *)
          let b2 = Buffer.create 42 in
            !main_process charstream (Out.Buffer b2);
            Out.output_buffer out b2;
        end
      else
        begin
          Out.output_charstream out arg;
          (* Out.output_char out '\n'; *)
          Out.output_charstream out charstream;
        end
    end
  else
    last_cond := Some false


let cmd arg charstream (out:Out.t) =
  let file, line, column = charstream.where() in
    match command arg charstream out with
      | 0 -> ()
      | ec ->
          if not (!ignore_exec_error) then
            Pervasives.failwith 
              (Printf.sprintf "Command <%s> ended with error <%d>. Location: %s:%d:%d." 
                 (string_of_charstream ~keepcs:true arg) ec file line column)
          else
            ()

let copy ~trunc _last_cond _nesting filename cs (out:Out.t) =
  let filename = string_of_charstream filename in
  let s = string_of_charstream cs in
  let o =
    if trunc then
      open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] 0o640 filename
    else
      open_out_gen [Open_wronly;Open_creat;Open_binary] 0o640 filename
  in
  let f = Out.Out_channel o in
    Out.output_string f s;
    Out.flush f;
    Pervasives.close_out o;
    Out.output_string out s
      

let builtins : action_set ref =
  let cmd _ _ = cmd in
  let echo _ _ =
    (fun a _cs out -> Out.output_charstream out a) in
  let cat _ _ =
    (fun filename _cs out -> Out.cat out (string_of_charstream filename); Out.flush out)
  in
  let set _ _ = Variable.set in
  let unset _ _ = Variable.unset in
  let unsetall _ _ = Variable.unsetall in
  let get _ _ = Variable.get in
  let tryget _ _ = Variable.tryget in
  let ifcmd = ifcmd in
  let ifdef = Variable.ifdef in
  let ifndef = Variable.ifndef in
(*   let elzeifdef = Variable.elzeifdef in *)
  let elze = Variable.elze in
  let error _ _ s cs _ =
    parse_error 
      ~msg:(Printf.sprintf "your message is <%s>. No matter what, I'm exiting." (string_of_charstream ~keepcs:true s))
      (cs.where());
    Pervasives.exit 1
  in
  let r =
    List.fold_left
      (fun r (k,(e:bool option ref -> bool -> Mpp_charstream.charstream -> Mpp_charstream.charstream -> Out.t -> unit),doc) -> Mpp_stringmap.add k (e,doc) r)
      Mpp_stringmap.empty
      [
        "ignore", (fun _ _ _ _ _ -> ()), "A command that does nothing with its arguments.";
        "ifcmd", ifcmd, "If the last external command returned 0, then inputs the rest.";
        "ifdef", ifdef, "If the argument is a defined variable, then inputs the rest.";
        "tryget", tryget, "Get the value of a variable, and if it doesn't exist, it does nothing.";
        "error", error, "Stops MPP.";
        "ifndef", ifndef, "If the argument is not a defined variable, then inputs the rest, else does nothing.";
        "else", elze, "If the previous test was not satisfied, then outputs its arguments";
        (* "elseifdef", elzeifdef, "If the previous test was not satisfied and the variable exists, then outputs the rest."; *)
        "set", set, "Set the variable to the rest. Related: get, tryget, unset, unsetall.";
        "get", get, "Get the value of a variable, and if it does not exist, MPP stops. Related: set, tryget, unset, unsetall.";
        "unset", unset, "Unset a variable. Related: tryget, get, tryget, unsetall.";
        "unsetall", unsetall, "Unset all variables. Related: tryget, get, tryget, unsetall.";
        "cmd", cmd, "Execute the rest of the line as a shell command. Following lines (if any) are given as input of the shell command.";
        "echo", echo, "Print the rest of the line.";
        "cat", cat, "Print the contents of a file.";
        "copy", copy ~trunc:false, "Copy the block to a file.";
        "tcopy", copy ~trunc:true, "Copy the block to a file, empty it first if it already exists.";
      ]
  in ref (r:action_set)

(* **end library ********************************************* *)
(* *********************************************************** *)

let apply_builtin action_name location =
  try
    match Mpp_stringmap.find action_name !builtins with
      | f, _ -> (f:bool option ref -> bool -> charstream -> charstream -> Out.t -> unit )
  with Not_found ->
    if !ignore_non_existing_commands then
      begin
        (* type  action = (bool option ref -> action_name -> charstream ->  Out.t -> unit) *)
        fun (_last_cond:bool option ref) (_nesting:bool) _action_name _charstream _out -> ()
      end
    else
      begin
        parse_error
          ~msg:(Printf.sprintf "Command <%s> not found!" action_name)
          location;
        Pervasives.exit 1
      end

let exec (nesting:bool) (last_cond:bool option ref) (action_name:string) (arguments:charstream) (charstream:charstream) (out: Out.t) =
  if !debug then
    begin
      Printf.eprintf "Exec: %!";
      (* action_name : thing to do; arguments : arguments on the first
         line; charstream : what follows the first line (if any). *)
      Printf.eprintf "action_name:<%s> arguments:<%s>\n%!"
        action_name (string_of_charstream ~keepcs:true arguments);
    end;
  if action_name.[0] <> '-' then
    begin (* builtins *)
      if nesting then
        begin
          if is_lazy action_name then
            begin (* nesting and lazy: it's delegated! *)
              apply_builtin action_name (charstream.where()) last_cond nesting arguments charstream out
            end
          else
            begin (* nesting but not lazy, so expand now! *)
              let buff1 = Buffer.create 42 in
              let buff2 = Buffer.create 42 in
              let arguments =
                let l = arguments.where() in
                  !main_process arguments (Out.Buffer buff1);
                  charstream_of_string ~location:l (Buffer.contents buff1)
              in
              let charstream =
                let x = charstream.where() in
                  !main_process charstream (Out.Buffer buff2);
                  charstream_of_string ~location:x (Buffer.contents buff2)
              in
                if !debug then Printf.eprintf "W???<%s><%s>\n%!" (String.escaped (string_of_charstream ~keepcs:true arguments)) (String.escaped (string_of_charstream ~keepcs:true charstream));
                apply_builtin action_name (charstream.where()) last_cond nesting arguments charstream out
            end
        end
      else
        begin (* no nesting *)
          apply_builtin action_name (charstream.where()) last_cond nesting arguments charstream out
        end
    end
  else
    begin
      if nesting then
        begin
          (apply_builtin "cmd" (charstream.where())) last_cond nesting
            (charstream_of_string (String.sub action_name 1 (String.length action_name - 1) ^ " " ^ string_of_charstream arguments))
            charstream out
        end
      else
        (apply_builtin "cmd" (charstream.where())) last_cond nesting
          (charstream_of_string (String.sub action_name 1 (String.length action_name - 1) ^ " " ^ string_of_charstream arguments))
          charstream out;
      if !debug then Printf.eprintf "cmd... mpp_actions.ml:>???%!";
    end;
  Out.flush out

let list_builtins out =
  let m =
    4 + Mpp_stringmap.fold
      (fun k _ r -> max (String.length k) r)
      !builtins
      0
  in
  let pad k = 
    k ^ String.make (max 1 (m - String.length k)) ' '
  in
    Mpp_stringmap.iter
      (fun k (_e, doc) ->
         Out.printf out "%s %s\n" (pad k) doc;
         Out.flush out)
      !builtins

let register (name:string) (f: action) (d:documentation) : unit =
  (* type action = last_condition -> nesting -> action_args -> charstream ->  Out.t -> unit *)
  builtins := Mpp_stringmap.add name (f,d) !builtins


let _ =
  register
    "builtins"
    (fun _ _ _ _ out -> list_builtins out)
    "List all available builtins."

let _ =
  List.iter register_lazy [
    "ifdef";
    "ifndef";
    "else";
    "elseifdef";
    "ifcmd";
  ]

let _ =
  register
    "getenv"
    (fun _ _ s cs out ->
       let v = string_of_charstream s in
       try
         Out.output_string out (Sys.getenv v);
         Out.flush out
       with Not_found ->
         parse_error
           ~msg:(Printf.sprintf "You tried to get the value of process-environment variable %s, which doesn't exist." v)
           (cs.where());
        Pervasives.exit 1
    )
    "Get a process-environment variable. Stops if it doesn't find it.";
  register
    "trygetenv"
    (fun _ _ s _ out ->
       try
         Out.output_string out (Sys.getenv(string_of_charstream s));
         Out.flush out
       with Not_found -> ())
    "Try to get a process-environment variable. Continues if it doesn't find it."


let builtins = () (* prevent builtins from being used outside. Eventually, I'll switch to using an mli file. *)


end
module
Ocamltohtml_lexer
= struct
# 6 "ocamltohtml_lexer.mll"
 
  (* header *)

exception Eof

let debug = false

let oc = ref stdout
let ic = ref stdin

let char_count = ref 0

let line_break = "\n"

let print_newline () =
  char_count := 0;
  output_char !oc '\n'

let print_char c =
  output_char !oc c

let print s =
  char_count := !char_count + String.length s;
  output_string !oc s

let print_string = print

let print_endline s =
  print s ;
  print_newline()

let comments = ref 0

type env = | NIL | KWD1 | KWD2 | KWD | OP | STR | MODULE | COMMENTS

let toclose = Stack.create ()
let toopen = Stack.create ()

let open_env e =
  match !comments with 
    | 0 ->
        print_string
          begin
            Stack.push e toclose;
            match e with
              | NIL -> "<span class='nil'>"
              | KWD1 -> "<span class='k'>"
              | KWD2 -> "<span class='o'>"
              | KWD -> "<span class='k'>"
              | STR -> "<span class='s'>"
              | MODULE -> "<span class='m'>"
              | OP -> "<span class='o'>"
              | COMMENTS -> "<span class='c'>"
          end
    | 1 ->
        if e = COMMENTS then 
          (Stack.push e toclose ; print_string "<span class='com2'>")
        else 
          (Stack.push e toclose ; print_string "<span class='ic'>" )
    | _ ->
        Stack.push e toclose ; print_string "<span class='other'>"

and close_env e =
  match !comments with 
    | 0 ->
        begin
          (try ignore(Stack.pop toclose)
            with _ -> if debug then prerr_endline "**************");
          match e with
            | COMMENTS -> print_string "</span><!-- end comment -->"
            | _ -> print_string "</span>" 
        end
    | _ -> 
        (try ignore(Stack.pop toclose)
          with _ -> if debug then prerr_endline "**************");
        match e with
          | COMMENTS ->  print_string "</span>"
          | _ -> print_string "</span>"


let open_envs () = 
  assert(Stack.is_empty toclose);
  Stack.iter (if debug then prerr_endline "---OPEN---"; fun e -> open_env e)  toopen ;
  Stack.clear toopen

let close_envs () = 
  assert(Stack.is_empty toopen);
  Stack.iter
    (if debug then prerr_endline "---CLOSE---";
     fun e -> 
       Stack.push e toopen; close_env e)
    toclose;
  Stack.clear toclose



let newline =
  let x = ref 1 in
    fun () ->
      incr x ;
      close_envs ();
      Printf.fprintf !oc "\n%!";
      open_envs ()

      

let html_escape s =
  let l = String.length s in
  let rec esc i =
    if i = l then ()
    else
      match s.[i] with
        | '&' | '<' | '>' | '\'' | '"' as c ->
            Printf.fprintf !oc "&#%d;" (int_of_char c);
            esc (succ i)
        | c -> 
            print_char c; esc (succ i)
  in esc 0

(* /header *)
# 123 "ocamltohtml_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\235\255\236\255\079\000\194\000\028\001\120\001\243\255\
    \212\001\172\000\250\001\136\000\079\000\032\002\094\002\169\002\
    \244\002\063\003\138\003\213\003\032\004\107\004\182\004\001\005\
    \076\005\151\005\226\005\045\006\120\006\195\006\014\007\047\001\
    \091\007\167\007\251\255\252\255\205\007\243\007\025\008\063\008\
    \255\255\253\255\240\255\078\001\052\002\057\001\089\001\212\001\
    \089\000\160\000\080\002\080\008\169\000\091\000\090\008\125\008\
    \200\008\019\009\094\009\169\009\244\009\102\010\177\010\252\010\
    \071\011\220\000\040\000\044\000\099\011\174\011\249\011\068\012\
    \143\012\218\012\037\013\112\013\187\013\006\014\081\014\156\014\
    \231\014\050\015\125\015\200\015\019\016\094\016\169\016\244\016\
    \102\017\177\017\223\000\071\000\074\000\248\255\252\017\071\018\
    \146\018\221\018\040\019\115\019\190\019\009\020\084\020\159\020\
    \234\020\053\021\128\021\203\021\022\022\097\022\172\022\247\022\
    \066\023\141\023\216\023\035\024\110\024\185\024\004\025\079\025\
    \154\025\229\025\048\026\123\026\198\026\017\027\092\027\167\027\
    \242\027\061\028\136\028\211\028\030\029\105\029\180\029\255\029\
    \074\030\149\030\224\030\043\031\118\031\193\031\012\032\087\032\
    \162\032\237\032\056\033\131\033\206\033\025\034\100\034\175\034\
    \250\034\069\035\144\035\219\035\038\036\113\036\188\036\007\037\
    \082\037\157\037\232\037\051\038\126\038\201\038\020\039\095\039\
    \170\039\245\039\064\040\139\040\214\040\033\041\108\041\183\041\
    \002\042\077\042\152\042\227\042\046\043\121\043\196\043\015\044\
    \090\044\165\044\240\044\059\045\134\045\209\045\028\046\103\046\
    \178\046\253\046\072\047\147\047\222\047\041\048\116\048\191\048\
    \002\000\024\049\027\049\111\008\057\000\245\255\065\010\136\000\
    \050\049\244\255\033\001\047\002\049\002\239\255\013\002\121\049\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\017\000\017\000\013\000\013\000\255\255\
    \012\000\012\000\012\000\020\000\020\000\009\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\005\000\
    \005\000\013\000\255\255\255\255\013\000\020\000\014\000\001\000\
    \255\255\255\255\255\255\005\000\005\000\255\255\005\000\255\255\
    \255\255\255\255\255\255\005\000\005\000\005\000\005\000\018\000\
    \017\000\017\000\017\000\017\000\017\000\007\000\006\000\006\000\
    \006\000\255\255\255\255\255\255\007\000\006\000\006\000\006\000\
    \017\000\017\000\009\000\017\000\009\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\009\000\017\000\017\000\
    \007\000\017\000\255\255\255\255\255\255\255\255\017\000\007\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\007\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\008\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\007\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\007\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \255\255\255\255\255\255\255\255\010\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\011\000\255\255\255\255\018\000\
    ";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\210\000\200\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\210\000\210\000\210\000\000\000\255\255\255\255\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\035\000\034\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \035\000\010\000\011\000\009\000\005\000\005\000\005\000\012\000\
    \037\000\205\000\036\000\005\000\009\000\033\000\005\000\005\000\
    \032\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\005\000\010\000\008\000\013\000\008\000\010\000\
    \005\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\007\000\005\000\007\000\005\000\006\000\
    \205\000\029\000\018\000\017\000\016\000\024\000\015\000\003\000\
    \003\000\027\000\003\000\003\000\030\000\028\000\019\000\026\000\
    \020\000\003\000\025\000\023\000\022\000\003\000\021\000\014\000\
    \003\000\003\000\003\000\007\000\005\000\007\000\005\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\053\000\053\000\053\000\053\000\067\000\003\000\068\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\209\000\201\000\092\000\093\000\055\000\205\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\214\000\214\000\009\000\000\000\009\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \009\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\000\000\214\000\000\000\211\000\065\000\065\000\009\000\
    \090\000\090\000\000\000\009\000\000\000\000\000\000\000\000\000\
    \213\000\000\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\065\000\000\000\000\000\090\000\
    \002\000\004\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\000\000\000\000\000\000\
    \000\000\215\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\005\000\000\000\000\000\
    \005\000\005\000\005\000\209\000\000\000\042\000\005\000\005\000\
    \000\000\005\000\005\000\005\000\000\000\000\000\066\000\255\255\
    \000\000\091\000\000\000\000\000\000\000\000\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\044\000\000\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\000\000\047\000\000\000\000\000\000\000\
    \005\000\000\000\005\000\005\000\044\000\211\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \255\255\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\000\000\047\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\000\000\005\000\005\000\005\000\000\000\
    \000\000\042\000\005\000\005\000\000\000\005\000\005\000\005\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\005\000\005\000\005\000\005\000\005\000\006\000\
    \005\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\000\000\005\000\000\000\005\000\006\000\
    \000\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\000\000\005\000\005\000\005\000\000\000\
    \005\000\005\000\005\000\000\000\000\000\042\000\005\000\005\000\
    \000\000\005\000\005\000\005\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\000\000\214\000\214\000\
    \000\000\000\000\000\000\010\000\000\000\009\000\005\000\005\000\
    \005\000\255\255\000\000\042\000\005\000\005\000\009\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\214\000\000\000\000\000\
    \005\000\000\000\005\000\005\000\005\000\010\000\005\000\005\000\
    \005\000\010\000\005\000\213\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\000\000\000\000\005\000\005\000\005\000\000\000\
    \000\000\042\000\005\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\212\000\005\000\209\000\000\000\000\000\005\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
    \005\000\045\000\000\000\000\000\005\000\000\000\005\000\005\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\000\000\211\000\000\000\211\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\045\000\000\000\000\000\005\000\003\000\005\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\195\000\194\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\184\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \183\000\003\000\003\000\003\000\003\000\003\000\185\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\255\255\
    \000\000\255\255\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\178\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\168\000\003\000\003\000\167\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\164\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\163\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\158\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\153\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\152\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\145\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\074\000\003\000\003\000\
    \146\000\003\000\003\000\003\000\003\000\003\000\003\000\147\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\141\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\140\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \125\000\003\000\126\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\127\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\124\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \074\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\122\000\003\000\074\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\095\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\105\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\081\000\003\000\003\000\003\000\083\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\084\000\
    \003\000\003\000\003\000\003\000\003\000\082\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\077\000\003\000\003\000\003\000\003\000\076\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\059\000\
    \003\000\003\000\003\000\060\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\058\000\003\000\003\000\
    \003\000\057\000\003\000\003\000\003\000\003\000\056\000\003\000\
    \003\000\044\000\000\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\048\000\000\000\000\000\
    \047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\049\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\050\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\048\000\000\000\000\000\
    \047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\000\000\049\000\005\000\005\000\005\000\000\000\000\000\
    \042\000\005\000\005\000\050\000\005\000\005\000\005\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\005\000\005\000\005\000\005\000\013\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
    \000\000\005\000\005\000\005\000\000\000\000\000\041\000\036\000\
    \005\000\000\000\005\000\005\000\005\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\038\000\000\000\000\000\038\000\
    \038\000\038\000\000\000\000\000\000\000\039\000\038\000\000\000\
    \038\000\038\000\038\000\005\000\000\000\005\000\000\000\000\000\
    \000\000\005\000\000\000\005\000\005\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\038\000\000\000\000\000\038\000\038\000\038\000\
    \000\000\000\000\000\000\038\000\038\000\000\000\038\000\038\000\
    \038\000\005\000\000\000\005\000\000\000\000\000\000\000\038\000\
    \000\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \038\000\000\000\000\000\038\000\038\000\038\000\000\000\000\000\
    \040\000\039\000\038\000\000\000\038\000\038\000\038\000\038\000\
    \000\000\038\000\000\000\000\000\000\000\038\000\000\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\000\000\038\000\000\000\038\000\
    \000\000\000\000\000\000\038\000\000\000\038\000\038\000\206\000\
    \206\000\206\000\206\000\206\000\206\000\206\000\206\000\206\000\
    \206\000\000\000\000\000\000\000\000\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\000\000\
    \000\000\000\000\000\000\038\000\055\000\038\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\075\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\074\000\
    \003\000\003\000\003\000\003\000\003\000\074\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \074\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\072\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\073\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \061\000\003\000\003\000\003\000\003\000\003\000\003\000\065\000\
    \065\000\207\000\207\000\207\000\207\000\207\000\207\000\207\000\
    \207\000\207\000\207\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\065\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\062\000\000\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \064\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\000\000\000\000\000\000\000\000\000\000\
    \062\000\000\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\000\000\000\000\000\000\000\000\
    \062\000\000\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\000\000\000\000\
    \000\000\000\000\000\000\063\000\000\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\000\000\
    \000\000\000\000\000\000\062\000\000\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\000\000\000\000\000\000\000\000\000\000\063\000\000\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\000\000\000\000\000\000\000\000\062\000\000\000\
    \063\000\063\000\063\000\063\000\070\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\069\000\000\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \000\000\000\000\000\000\000\000\000\000\069\000\000\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\000\000\000\000\000\000\000\000\069\000\000\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\000\000\000\000\000\000\000\000\000\000\
    \063\000\000\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\000\000\000\000\000\000\000\000\
    \062\000\000\000\063\000\063\000\071\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\000\000\000\000\
    \000\000\000\000\000\000\063\000\000\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\000\000\
    \000\000\000\000\000\000\062\000\000\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\074\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\074\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\074\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\074\000\078\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\061\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\079\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\080\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\074\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\103\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\099\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\096\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\085\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\086\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\087\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\088\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\090\000\
    \090\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\090\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \089\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\094\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\095\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\097\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\098\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\095\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\100\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \101\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\102\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\095\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\104\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\074\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \108\000\003\000\003\000\003\000\003\000\107\000\106\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\115\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \112\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\109\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\110\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \111\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\095\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\113\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\114\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\095\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\116\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\117\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\118\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\119\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\120\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\121\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\095\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\123\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\095\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\095\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\139\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \095\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\129\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\128\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\135\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\130\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \131\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\132\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\133\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \134\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\095\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\136\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \137\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\138\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\095\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \095\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \142\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \095\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\143\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\144\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\095\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\151\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\149\000\003\000\003\000\
    \003\000\095\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\148\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\095\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\150\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\074\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\154\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\095\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\155\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \156\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\157\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\095\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\159\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\160\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\161\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\162\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\095\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\095\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\165\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\166\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\095\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\171\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\169\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \170\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\095\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\172\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\173\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\174\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\175\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \176\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \177\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\074\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\180\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\179\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\181\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\095\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\182\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\095\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\074\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\192\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\186\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\187\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\188\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\190\000\
    \003\000\003\000\003\000\003\000\003\000\189\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\095\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\191\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\095\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\055\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\193\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\055\000\000\000\003\000\003\000\
    \003\000\003\000\150\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\055\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\199\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\055\000\000\000\003\000\003\000\003\000\003\000\
    \197\000\003\000\003\000\003\000\196\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\055\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\198\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\000\000\000\000\000\000\000\000\
    \055\000\000\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\095\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\000\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\055\000\000\000\003\000\003\000\003\000\
    \003\000\095\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\055\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\095\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\200\000\000\000\000\000\000\000\000\000\204\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \203\000\203\000\203\000\208\000\208\000\208\000\208\000\208\000\
    \208\000\208\000\208\000\208\000\208\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\208\000\208\000\208\000\208\000\
    \208\000\208\000\207\000\207\000\207\000\207\000\207\000\207\000\
    \207\000\207\000\207\000\207\000\000\000\000\000\000\000\000\000\
    \202\000\000\000\000\000\207\000\207\000\207\000\207\000\207\000\
    \207\000\000\000\200\000\000\000\208\000\208\000\208\000\208\000\
    \208\000\208\000\214\000\214\000\000\000\000\000\200\000\000\000\
    \000\000\000\000\200\000\000\000\200\000\000\000\000\000\000\000\
    \202\000\000\000\000\000\207\000\207\000\207\000\207\000\207\000\
    \207\000\214\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\213\000\
    \000\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\000\000\000\000\000\000\000\000\000\000\
    \215\000\000\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\000\000\000\000\000\000\000\000\
    \215\000\000\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \204\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\048\000\048\000\053\000\053\000\066\000\003\000\067\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\011\000\012\000\091\000\092\000\003\000\207\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\255\255\004\000\004\000\009\000\255\255\009\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \009\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\255\255\004\000\255\255\011\000\065\000\065\000\009\000\
    \090\000\090\000\255\255\009\000\255\255\255\255\255\255\255\255\
    \004\000\255\255\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\065\000\255\255\255\255\090\000\
    \000\000\004\000\255\255\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\255\255\255\255\255\255\
    \255\255\004\000\255\255\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\005\000\255\255\255\255\
    \005\000\005\000\005\000\210\000\255\255\005\000\005\000\005\000\
    \255\255\005\000\005\000\005\000\255\255\255\255\065\000\012\000\
    \255\255\090\000\255\255\255\255\255\255\255\255\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\031\000\255\255\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\255\255\031\000\255\255\255\255\255\255\
    \005\000\255\255\005\000\005\000\043\000\210\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \011\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\255\255\031\000\255\255\255\255\255\255\
    \005\000\006\000\005\000\255\255\006\000\006\000\006\000\255\255\
    \255\255\006\000\006\000\006\000\255\255\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\255\255\006\000\255\255\006\000\006\000\
    \255\255\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\255\255\006\000\008\000\006\000\255\255\
    \008\000\008\000\008\000\255\255\255\255\008\000\008\000\008\000\
    \255\255\008\000\008\000\008\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\255\255\214\000\214\000\
    \255\255\255\255\255\255\010\000\255\255\010\000\010\000\010\000\
    \010\000\210\000\255\255\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\255\255\255\255\255\255\214\000\255\255\255\255\
    \008\000\255\255\008\000\008\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\214\000\255\255\255\255\255\255\255\255\
    \255\255\013\000\255\255\255\255\013\000\013\000\013\000\255\255\
    \255\255\013\000\013\000\013\000\255\255\013\000\013\000\013\000\
    \008\000\211\000\008\000\212\000\255\255\255\255\010\000\255\255\
    \010\000\010\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\255\255\255\255\255\255\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\010\000\255\255\
    \010\000\044\000\255\255\255\255\013\000\255\255\013\000\013\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\255\255\211\000\255\255\212\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \255\255\044\000\255\255\255\255\013\000\014\000\013\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\255\255\255\255\255\255\255\255\014\000\255\255\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\255\255\255\255\255\255\255\255\255\255\
    \015\000\255\255\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\255\255\255\255\255\255\255\255\
    \015\000\255\255\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\255\255\211\000\
    \255\255\212\000\255\255\016\000\255\255\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\255\255\
    \255\255\255\255\255\255\016\000\255\255\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\255\255\255\255\255\255\255\255\255\255\017\000\255\255\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\255\255\255\255\255\255\255\255\017\000\255\255\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\255\255\255\255\255\255\255\255\
    \255\255\018\000\255\255\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\255\255\255\255\255\255\
    \255\255\018\000\255\255\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\255\255\
    \255\255\255\255\255\255\255\255\019\000\255\255\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \255\255\255\255\255\255\255\255\019\000\255\255\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\255\255\255\255\255\255\255\255\255\255\020\000\
    \255\255\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\255\255\255\255\255\255\255\255\020\000\
    \255\255\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\255\255\255\255\255\255\
    \255\255\255\255\021\000\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\255\255\255\255\
    \255\255\255\255\021\000\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \255\255\255\255\255\255\255\255\255\255\022\000\255\255\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\255\255\255\255\255\255\255\255\022\000\255\255\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\255\255\255\255\255\255\255\255\255\255\
    \023\000\255\255\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\255\255\255\255\255\255\255\255\
    \023\000\255\255\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\255\255\255\255\
    \255\255\255\255\255\255\024\000\255\255\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\255\255\
    \255\255\255\255\255\255\024\000\255\255\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\255\255\255\255\255\255\255\255\255\255\025\000\255\255\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\255\255\255\255\255\255\255\255\025\000\255\255\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\255\255\255\255\255\255\255\255\
    \255\255\026\000\255\255\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\255\255\255\255\255\255\
    \255\255\026\000\255\255\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\255\255\
    \255\255\255\255\255\255\255\255\027\000\255\255\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \255\255\255\255\255\255\255\255\027\000\255\255\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\255\255\255\255\255\255\255\255\255\255\028\000\
    \255\255\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\255\255\255\255\255\255\255\255\028\000\
    \255\255\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\255\255\255\255\255\255\
    \255\255\255\255\029\000\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\255\255\255\255\
    \255\255\255\255\029\000\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \255\255\255\255\255\255\255\255\255\255\030\000\255\255\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\255\255\255\255\255\255\255\255\030\000\255\255\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\032\000\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\032\000\255\255\255\255\
    \032\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\032\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\032\000\255\255\255\255\
    \032\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \033\000\255\255\032\000\033\000\033\000\033\000\255\255\255\255\
    \033\000\033\000\033\000\032\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\036\000\255\255\
    \255\255\036\000\036\000\036\000\255\255\255\255\036\000\036\000\
    \036\000\255\255\036\000\036\000\036\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\033\000\255\255\033\000\033\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\037\000\255\255\255\255\037\000\
    \037\000\037\000\255\255\255\255\255\255\037\000\037\000\255\255\
    \037\000\037\000\037\000\033\000\255\255\033\000\255\255\255\255\
    \255\255\036\000\255\255\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\038\000\255\255\255\255\038\000\038\000\038\000\
    \255\255\255\255\255\255\038\000\038\000\255\255\038\000\038\000\
    \038\000\036\000\255\255\036\000\255\255\255\255\255\255\037\000\
    \255\255\037\000\037\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \039\000\255\255\255\255\039\000\039\000\039\000\255\255\255\255\
    \039\000\039\000\039\000\255\255\039\000\039\000\039\000\037\000\
    \255\255\037\000\255\255\255\255\255\255\038\000\255\255\038\000\
    \038\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\255\255\038\000\255\255\038\000\
    \255\255\255\255\255\255\039\000\255\255\039\000\039\000\203\000\
    \203\000\203\000\203\000\203\000\203\000\203\000\203\000\203\000\
    \203\000\255\255\255\255\255\255\255\255\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\255\255\
    \255\255\255\255\255\255\039\000\055\000\039\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \255\255\255\255\255\255\255\255\055\000\255\255\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\255\255\255\255\255\255\255\255\255\255\056\000\
    \255\255\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\255\255\255\255\255\255\255\255\056\000\
    \255\255\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\056\000\056\000\056\000\056\000\056\000\
    \056\000\056\000\056\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\255\255\255\255\255\255\
    \255\255\255\255\057\000\255\255\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\255\255\255\255\
    \255\255\255\255\057\000\255\255\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \255\255\255\255\255\255\255\255\255\255\058\000\255\255\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\255\255\255\255\255\255\255\255\058\000\255\255\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\255\255\255\255\255\255\255\255\255\255\
    \059\000\255\255\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\255\255\255\255\255\255\255\255\
    \059\000\255\255\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\255\255\255\255\
    \255\255\255\255\255\255\060\000\255\255\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\060\000\255\255\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\061\000\
    \061\000\206\000\206\000\206\000\206\000\206\000\206\000\206\000\
    \206\000\206\000\206\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\061\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \255\255\255\255\255\255\255\255\255\255\061\000\255\255\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\255\255\255\255\255\255\255\255\061\000\255\255\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\255\255\255\255\255\255\255\255\255\255\
    \062\000\255\255\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\255\255\255\255\255\255\255\255\
    \062\000\255\255\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\255\255\255\255\
    \255\255\255\255\255\255\063\000\255\255\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\255\255\
    \255\255\255\255\255\255\063\000\255\255\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\255\255\255\255\255\255\255\255\255\255\064\000\255\255\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\255\255\255\255\255\255\255\255\064\000\255\255\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\068\000\255\255\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \255\255\255\255\255\255\255\255\255\255\069\000\255\255\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\255\255\255\255\255\255\255\255\069\000\255\255\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\255\255\255\255\255\255\255\255\255\255\
    \070\000\255\255\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\255\255\255\255\255\255\255\255\
    \070\000\255\255\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\255\255\255\255\
    \255\255\255\255\255\255\071\000\255\255\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\255\255\
    \255\255\255\255\255\255\071\000\255\255\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\255\255\255\255\255\255\255\255\255\255\072\000\255\255\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\255\255\255\255\255\255\255\255\072\000\255\255\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\255\255\255\255\255\255\255\255\
    \255\255\073\000\255\255\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\255\255\255\255\255\255\
    \255\255\073\000\255\255\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\255\255\
    \255\255\255\255\255\255\255\255\074\000\255\255\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \255\255\255\255\255\255\255\255\074\000\255\255\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\255\255\255\255\255\255\255\255\255\255\075\000\
    \255\255\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\255\255\255\255\255\255\255\255\075\000\
    \255\255\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \075\000\075\000\075\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\255\255\255\255\255\255\
    \255\255\255\255\076\000\255\255\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\255\255\255\255\
    \255\255\255\255\076\000\255\255\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\076\000\076\000\
    \076\000\076\000\076\000\076\000\076\000\076\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \255\255\255\255\255\255\255\255\255\255\077\000\255\255\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\255\255\255\255\255\255\255\255\077\000\255\255\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\255\255\255\255\255\255\255\255\255\255\
    \078\000\255\255\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\255\255\255\255\255\255\255\255\
    \078\000\255\255\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\255\255\255\255\
    \255\255\255\255\255\255\079\000\255\255\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\255\255\
    \255\255\255\255\255\255\079\000\255\255\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\255\255\255\255\255\255\255\255\255\255\080\000\255\255\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\255\255\255\255\255\255\255\255\080\000\255\255\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\255\255\255\255\255\255\255\255\
    \255\255\081\000\255\255\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\255\255\255\255\255\255\
    \255\255\081\000\255\255\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\255\255\
    \255\255\255\255\255\255\255\255\082\000\255\255\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \255\255\255\255\255\255\255\255\082\000\255\255\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\255\255\255\255\255\255\255\255\255\255\083\000\
    \255\255\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\255\255\255\255\255\255\255\255\083\000\
    \255\255\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\083\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\255\255\255\255\255\255\
    \255\255\255\255\084\000\255\255\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\255\255\255\255\
    \255\255\255\255\084\000\255\255\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\084\000\084\000\
    \084\000\084\000\084\000\084\000\084\000\084\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \255\255\255\255\255\255\255\255\255\255\085\000\255\255\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\255\255\255\255\255\255\255\255\085\000\255\255\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\255\255\255\255\255\255\255\255\255\255\
    \086\000\255\255\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\255\255\255\255\255\255\255\255\
    \086\000\255\255\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\255\255\255\255\
    \255\255\255\255\255\255\087\000\255\255\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\255\255\
    \255\255\255\255\255\255\087\000\255\255\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\088\000\
    \088\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\088\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \255\255\255\255\255\255\255\255\255\255\088\000\255\255\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\255\255\255\255\255\255\255\255\088\000\255\255\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\255\255\255\255\255\255\255\255\255\255\
    \089\000\255\255\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\255\255\255\255\255\255\255\255\
    \089\000\255\255\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\255\255\255\255\
    \255\255\255\255\255\255\094\000\255\255\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\255\255\
    \255\255\255\255\255\255\094\000\255\255\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\255\255\255\255\255\255\255\255\255\255\095\000\255\255\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\255\255\255\255\255\255\255\255\095\000\255\255\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\255\255\255\255\255\255\255\255\
    \255\255\096\000\255\255\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\255\255\255\255\255\255\
    \255\255\096\000\255\255\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\255\255\
    \255\255\255\255\255\255\255\255\097\000\255\255\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \255\255\255\255\255\255\255\255\097\000\255\255\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\255\255\255\255\255\255\255\255\255\255\098\000\
    \255\255\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\255\255\255\255\255\255\255\255\098\000\
    \255\255\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\255\255\255\255\255\255\
    \255\255\255\255\099\000\255\255\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\255\255\255\255\
    \255\255\255\255\099\000\255\255\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \255\255\255\255\255\255\255\255\255\255\100\000\255\255\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\255\255\255\255\255\255\255\255\100\000\255\255\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\255\255\255\255\255\255\255\255\255\255\
    \101\000\255\255\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\255\255\255\255\255\255\255\255\
    \101\000\255\255\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\101\000\101\000\101\000\101\000\
    \101\000\101\000\101\000\101\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\255\255\255\255\
    \255\255\255\255\255\255\102\000\255\255\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\255\255\
    \255\255\255\255\255\255\102\000\255\255\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\102\000\
    \102\000\102\000\102\000\102\000\102\000\102\000\102\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\255\255\255\255\255\255\255\255\255\255\103\000\255\255\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\255\255\255\255\255\255\255\255\103\000\255\255\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\103\000\103\000\103\000\103\000\103\000\103\000\
    \103\000\103\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\255\255\255\255\255\255\255\255\
    \255\255\104\000\255\255\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\255\255\255\255\255\255\
    \255\255\104\000\255\255\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\104\000\104\000\104\000\
    \104\000\104\000\104\000\104\000\104\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\255\255\
    \255\255\255\255\255\255\255\255\105\000\255\255\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \255\255\255\255\255\255\255\255\105\000\255\255\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \105\000\105\000\105\000\105\000\105\000\105\000\105\000\105\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\255\255\255\255\255\255\255\255\255\255\106\000\
    \255\255\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\255\255\255\255\255\255\255\255\106\000\
    \255\255\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\106\000\106\000\106\000\106\000\106\000\
    \106\000\106\000\106\000\107\000\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\255\255\255\255\255\255\
    \255\255\255\255\107\000\255\255\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\107\000\255\255\255\255\
    \255\255\255\255\107\000\255\255\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\107\000\107\000\108\000\108\000\
    \108\000\108\000\108\000\108\000\108\000\108\000\108\000\108\000\
    \255\255\255\255\255\255\255\255\255\255\108\000\255\255\108\000\
    \108\000\108\000\108\000\108\000\108\000\108\000\108\000\108\000\
    \108\000\108\000\108\000\108\000\108\000\108\000\108\000\108\000\
    \108\000\108\000\108\000\108\000\108\000\108\000\108\000\108\000\
    \108\000\255\255\255\255\255\255\255\255\108\000\255\255\108\000\
    \108\000\108\000\108\000\108\000\108\000\108\000\108\000\108\000\
    \108\000\108\000\108\000\108\000\108\000\108\000\108\000\108\000\
    \108\000\108\000\108\000\108\000\108\000\108\000\108\000\108\000\
    \108\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\255\255\255\255\255\255\255\255\255\255\
    \109\000\255\255\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\255\255\255\255\255\255\255\255\
    \109\000\255\255\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\110\000\110\000\110\000\110\000\
    \110\000\110\000\110\000\110\000\110\000\110\000\255\255\255\255\
    \255\255\255\255\255\255\110\000\255\255\110\000\110\000\110\000\
    \110\000\110\000\110\000\110\000\110\000\110\000\110\000\110\000\
    \110\000\110\000\110\000\110\000\110\000\110\000\110\000\110\000\
    \110\000\110\000\110\000\110\000\110\000\110\000\110\000\255\255\
    \255\255\255\255\255\255\110\000\255\255\110\000\110\000\110\000\
    \110\000\110\000\110\000\110\000\110\000\110\000\110\000\110\000\
    \110\000\110\000\110\000\110\000\110\000\110\000\110\000\110\000\
    \110\000\110\000\110\000\110\000\110\000\110\000\110\000\111\000\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\255\255\255\255\255\255\255\255\255\255\111\000\255\255\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\111\000\255\255\255\255\255\255\255\255\111\000\255\255\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\111\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\255\255\255\255\255\255\255\255\
    \255\255\112\000\255\255\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\255\255\255\255\255\255\
    \255\255\112\000\255\255\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\113\000\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\113\000\255\255\
    \255\255\255\255\255\255\255\255\113\000\255\255\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\113\000\113\000\
    \255\255\255\255\255\255\255\255\113\000\255\255\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\113\000\113\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\255\255\255\255\255\255\255\255\255\255\114\000\
    \255\255\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\255\255\255\255\255\255\255\255\114\000\
    \255\255\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\115\000\115\000\115\000\115\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\255\255\255\255\255\255\
    \255\255\255\255\115\000\255\255\115\000\115\000\115\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\115\000\115\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\115\000\115\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\115\000\255\255\255\255\
    \255\255\255\255\115\000\255\255\115\000\115\000\115\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\115\000\115\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\115\000\115\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\115\000\116\000\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \255\255\255\255\255\255\255\255\255\255\116\000\255\255\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\255\255\255\255\255\255\255\255\116\000\255\255\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\117\000\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\255\255\255\255\255\255\255\255\255\255\
    \117\000\255\255\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\117\000\255\255\255\255\255\255\255\255\
    \117\000\255\255\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\117\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\255\255\255\255\
    \255\255\255\255\255\255\118\000\255\255\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\255\255\
    \255\255\255\255\255\255\118\000\255\255\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\119\000\
    \119\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\255\255\255\255\255\255\255\255\255\255\119\000\255\255\
    \119\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\119\000\255\255\255\255\255\255\255\255\119\000\255\255\
    \119\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\119\000\120\000\120\000\120\000\120\000\120\000\120\000\
    \120\000\120\000\120\000\120\000\255\255\255\255\255\255\255\255\
    \255\255\120\000\255\255\120\000\120\000\120\000\120\000\120\000\
    \120\000\120\000\120\000\120\000\120\000\120\000\120\000\120\000\
    \120\000\120\000\120\000\120\000\120\000\120\000\120\000\120\000\
    \120\000\120\000\120\000\120\000\120\000\255\255\255\255\255\255\
    \255\255\120\000\255\255\120\000\120\000\120\000\120\000\120\000\
    \120\000\120\000\120\000\120\000\120\000\120\000\120\000\120\000\
    \120\000\120\000\120\000\120\000\120\000\120\000\120\000\120\000\
    \120\000\120\000\120\000\120\000\120\000\121\000\121\000\121\000\
    \121\000\121\000\121\000\121\000\121\000\121\000\121\000\255\255\
    \255\255\255\255\255\255\255\255\121\000\255\255\121\000\121\000\
    \121\000\121\000\121\000\121\000\121\000\121\000\121\000\121\000\
    \121\000\121\000\121\000\121\000\121\000\121\000\121\000\121\000\
    \121\000\121\000\121\000\121\000\121\000\121\000\121\000\121\000\
    \255\255\255\255\255\255\255\255\121\000\255\255\121\000\121\000\
    \121\000\121\000\121\000\121\000\121\000\121\000\121\000\121\000\
    \121\000\121\000\121\000\121\000\121\000\121\000\121\000\121\000\
    \121\000\121\000\121\000\121\000\121\000\121\000\121\000\121\000\
    \122\000\122\000\122\000\122\000\122\000\122\000\122\000\122\000\
    \122\000\122\000\255\255\255\255\255\255\255\255\255\255\122\000\
    \255\255\122\000\122\000\122\000\122\000\122\000\122\000\122\000\
    \122\000\122\000\122\000\122\000\122\000\122\000\122\000\122\000\
    \122\000\122\000\122\000\122\000\122\000\122\000\122\000\122\000\
    \122\000\122\000\122\000\255\255\255\255\255\255\255\255\122\000\
    \255\255\122\000\122\000\122\000\122\000\122\000\122\000\122\000\
    \122\000\122\000\122\000\122\000\122\000\122\000\122\000\122\000\
    \122\000\122\000\122\000\122\000\122\000\122\000\122\000\122\000\
    \122\000\122\000\122\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\255\255\255\255\255\255\
    \255\255\255\255\123\000\255\255\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\255\255\255\255\
    \255\255\255\255\123\000\255\255\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\123\000\123\000\
    \123\000\123\000\123\000\123\000\123\000\123\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \255\255\255\255\255\255\255\255\255\255\124\000\255\255\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\255\255\255\255\255\255\255\255\124\000\255\255\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\124\000\124\000\124\000\124\000\124\000\124\000\124\000\
    \124\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\255\255\255\255\255\255\255\255\255\255\
    \125\000\255\255\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\255\255\255\255\255\255\255\255\
    \125\000\255\255\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\125\000\125\000\125\000\125\000\
    \125\000\125\000\125\000\125\000\126\000\126\000\126\000\126\000\
    \126\000\126\000\126\000\126\000\126\000\126\000\255\255\255\255\
    \255\255\255\255\255\255\126\000\255\255\126\000\126\000\126\000\
    \126\000\126\000\126\000\126\000\126\000\126\000\126\000\126\000\
    \126\000\126\000\126\000\126\000\126\000\126\000\126\000\126\000\
    \126\000\126\000\126\000\126\000\126\000\126\000\126\000\255\255\
    \255\255\255\255\255\255\126\000\255\255\126\000\126\000\126\000\
    \126\000\126\000\126\000\126\000\126\000\126\000\126\000\126\000\
    \126\000\126\000\126\000\126\000\126\000\126\000\126\000\126\000\
    \126\000\126\000\126\000\126\000\126\000\126\000\126\000\127\000\
    \127\000\127\000\127\000\127\000\127\000\127\000\127\000\127\000\
    \127\000\255\255\255\255\255\255\255\255\255\255\127\000\255\255\
    \127\000\127\000\127\000\127\000\127\000\127\000\127\000\127\000\
    \127\000\127\000\127\000\127\000\127\000\127\000\127\000\127\000\
    \127\000\127\000\127\000\127\000\127\000\127\000\127\000\127\000\
    \127\000\127\000\255\255\255\255\255\255\255\255\127\000\255\255\
    \127\000\127\000\127\000\127\000\127\000\127\000\127\000\127\000\
    \127\000\127\000\127\000\127\000\127\000\127\000\127\000\127\000\
    \127\000\127\000\127\000\127\000\127\000\127\000\127\000\127\000\
    \127\000\127\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\255\255\255\255\255\255\255\255\
    \255\255\128\000\255\255\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\255\255\255\255\255\255\
    \255\255\128\000\255\255\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\129\000\129\000\129\000\
    \129\000\129\000\129\000\129\000\129\000\129\000\129\000\255\255\
    \255\255\255\255\255\255\255\255\129\000\255\255\129\000\129\000\
    \129\000\129\000\129\000\129\000\129\000\129\000\129\000\129\000\
    \129\000\129\000\129\000\129\000\129\000\129\000\129\000\129\000\
    \129\000\129\000\129\000\129\000\129\000\129\000\129\000\129\000\
    \255\255\255\255\255\255\255\255\129\000\255\255\129\000\129\000\
    \129\000\129\000\129\000\129\000\129\000\129\000\129\000\129\000\
    \129\000\129\000\129\000\129\000\129\000\129\000\129\000\129\000\
    \129\000\129\000\129\000\129\000\129\000\129\000\129\000\129\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\255\255\255\255\255\255\255\255\255\255\130\000\
    \255\255\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\255\255\255\255\255\255\255\255\130\000\
    \255\255\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\255\255\255\255\255\255\
    \255\255\255\255\131\000\255\255\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\255\255\255\255\
    \255\255\255\255\131\000\255\255\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \255\255\255\255\255\255\255\255\255\255\132\000\255\255\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\255\255\255\255\255\255\255\255\132\000\255\255\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\255\255\255\255\255\255\255\255\255\255\
    \133\000\255\255\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\255\255\255\255\255\255\255\255\
    \133\000\255\255\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\133\000\133\000\133\000\133\000\
    \133\000\133\000\133\000\133\000\134\000\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\134\000\255\255\255\255\
    \255\255\255\255\255\255\134\000\255\255\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\134\000\134\000\255\255\
    \255\255\255\255\255\255\134\000\255\255\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\134\000\134\000\134\000\
    \134\000\134\000\134\000\134\000\134\000\134\000\134\000\135\000\
    \135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
    \135\000\255\255\255\255\255\255\255\255\255\255\135\000\255\255\
    \135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
    \135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
    \135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
    \135\000\135\000\255\255\255\255\255\255\255\255\135\000\255\255\
    \135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
    \135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
    \135\000\135\000\135\000\135\000\135\000\135\000\135\000\135\000\
    \135\000\135\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\255\255\255\255\255\255\255\255\
    \255\255\136\000\255\255\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\255\255\255\255\255\255\
    \255\255\136\000\255\255\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\136\000\136\000\136\000\
    \136\000\136\000\136\000\136\000\136\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\255\255\
    \255\255\255\255\255\255\255\255\137\000\255\255\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \255\255\255\255\255\255\255\255\137\000\255\255\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\255\255\255\255\255\255\255\255\255\255\138\000\
    \255\255\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\255\255\255\255\255\255\255\255\138\000\
    \255\255\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\255\255\255\255\255\255\
    \255\255\255\255\139\000\255\255\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\255\255\255\255\
    \255\255\255\255\139\000\255\255\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \255\255\255\255\255\255\255\255\255\255\140\000\255\255\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\255\255\255\255\255\255\255\255\140\000\255\255\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\140\000\140\000\140\000\140\000\140\000\140\000\140\000\
    \140\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\
    \141\000\141\000\141\000\255\255\255\255\255\255\255\255\255\255\
    \141\000\255\255\141\000\141\000\141\000\141\000\141\000\141\000\
    \141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\
    \141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\
    \141\000\141\000\141\000\141\000\255\255\255\255\255\255\255\255\
    \141\000\255\255\141\000\141\000\141\000\141\000\141\000\141\000\
    \141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\
    \141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\
    \141\000\141\000\141\000\141\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\255\255\255\255\
    \255\255\255\255\255\255\142\000\255\255\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\255\255\
    \255\255\255\255\255\255\142\000\255\255\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\255\255\255\255\255\255\255\255\255\255\143\000\255\255\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\255\255\255\255\255\255\255\255\143\000\255\255\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\255\255\255\255\255\255\255\255\
    \255\255\144\000\255\255\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\255\255\255\255\255\255\
    \255\255\144\000\255\255\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\144\000\144\000\144\000\
    \144\000\144\000\144\000\144\000\144\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\255\255\
    \255\255\255\255\255\255\255\255\145\000\255\255\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \255\255\255\255\255\255\255\255\145\000\255\255\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\255\255\255\255\255\255\255\255\255\255\146\000\
    \255\255\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\255\255\255\255\255\255\255\255\146\000\
    \255\255\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\146\000\
    \146\000\146\000\146\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\255\255\255\255\255\255\
    \255\255\255\255\147\000\255\255\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\255\255\255\255\
    \255\255\255\255\147\000\255\255\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\147\000\147\000\
    \147\000\147\000\147\000\147\000\147\000\147\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \255\255\255\255\255\255\255\255\255\255\148\000\255\255\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\255\255\255\255\255\255\255\255\148\000\255\255\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\148\000\148\000\148\000\
    \148\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\255\255\255\255\255\255\255\255\255\255\
    \149\000\255\255\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\255\255\255\255\255\255\255\255\
    \149\000\255\255\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\149\000\149\000\149\000\149\000\
    \149\000\149\000\149\000\149\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\255\255\255\255\
    \255\255\255\255\255\255\150\000\255\255\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\255\255\
    \255\255\255\255\255\255\150\000\255\255\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\150\000\
    \150\000\150\000\150\000\150\000\150\000\150\000\150\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\255\255\255\255\255\255\255\255\255\255\151\000\255\255\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\255\255\255\255\255\255\255\255\151\000\255\255\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\151\000\151\000\151\000\151\000\151\000\151\000\
    \151\000\151\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\255\255\255\255\255\255\255\255\
    \255\255\152\000\255\255\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\255\255\255\255\255\255\
    \255\255\152\000\255\255\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\152\000\152\000\152\000\
    \152\000\152\000\152\000\152\000\152\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\255\255\
    \255\255\255\255\255\255\255\255\153\000\255\255\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \255\255\255\255\255\255\255\255\153\000\255\255\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\255\255\255\255\255\255\255\255\255\255\154\000\
    \255\255\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\255\255\255\255\255\255\255\255\154\000\
    \255\255\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\255\255\255\255\255\255\
    \255\255\255\255\155\000\255\255\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\255\255\255\255\
    \255\255\255\255\155\000\255\255\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
    \155\000\155\000\155\000\155\000\155\000\155\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \255\255\255\255\255\255\255\255\255\255\156\000\255\255\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\255\255\255\255\255\255\255\255\156\000\255\255\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\156\000\156\000\156\000\156\000\156\000\156\000\156\000\
    \156\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\255\255\255\255\255\255\255\255\255\255\
    \157\000\255\255\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\255\255\255\255\255\255\255\255\
    \157\000\255\255\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\255\255\255\255\
    \255\255\255\255\255\255\158\000\255\255\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\255\255\
    \255\255\255\255\255\255\158\000\255\255\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\158\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\255\255\255\255\255\255\255\255\255\255\159\000\255\255\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\255\255\255\255\255\255\255\255\159\000\255\255\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\159\000\159\000\159\000\159\000\159\000\159\000\
    \159\000\159\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\255\255\255\255\255\255\255\255\
    \255\255\160\000\255\255\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\255\255\255\255\255\255\
    \255\255\160\000\255\255\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\255\255\
    \255\255\255\255\255\255\255\255\161\000\255\255\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \255\255\255\255\255\255\255\255\161\000\255\255\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\255\255\255\255\255\255\255\255\255\255\162\000\
    \255\255\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\255\255\255\255\255\255\255\255\162\000\
    \255\255\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\162\000\162\000\162\000\162\000\162\000\
    \162\000\162\000\162\000\163\000\163\000\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\255\255\255\255\255\255\
    \255\255\255\255\163\000\255\255\163\000\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\163\000\255\255\255\255\
    \255\255\255\255\163\000\255\255\163\000\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\163\000\163\000\163\000\
    \163\000\163\000\163\000\163\000\163\000\163\000\164\000\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \255\255\255\255\255\255\255\255\255\255\164\000\255\255\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \164\000\255\255\255\255\255\255\255\255\164\000\255\255\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \164\000\164\000\164\000\164\000\164\000\164\000\164\000\164\000\
    \164\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\255\255\255\255\255\255\255\255\255\255\
    \165\000\255\255\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\255\255\255\255\255\255\255\255\
    \165\000\255\255\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\165\000\165\000\165\000\165\000\
    \165\000\165\000\165\000\165\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\255\255\255\255\
    \255\255\255\255\255\255\166\000\255\255\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\255\255\
    \255\255\255\255\255\255\166\000\255\255\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\166\000\
    \166\000\166\000\166\000\166\000\166\000\166\000\166\000\167\000\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\255\255\255\255\255\255\255\255\255\255\167\000\255\255\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\167\000\255\255\255\255\255\255\255\255\167\000\255\255\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\167\000\167\000\167\000\167\000\167\000\167\000\167\000\
    \167\000\167\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\255\255\255\255\255\255\255\255\
    \255\255\168\000\255\255\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\255\255\255\255\255\255\
    \255\255\168\000\255\255\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\255\255\
    \255\255\255\255\255\255\255\255\169\000\255\255\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \255\255\255\255\255\255\255\255\169\000\255\255\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\255\255\255\255\255\255\255\255\255\255\170\000\
    \255\255\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\255\255\255\255\255\255\255\255\170\000\
    \255\255\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\255\255\255\255\255\255\
    \255\255\255\255\171\000\255\255\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\255\255\255\255\
    \255\255\255\255\171\000\255\255\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\172\000\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \255\255\255\255\255\255\255\255\255\255\172\000\255\255\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\255\255\255\255\255\255\255\255\172\000\255\255\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\255\255\255\255\255\255\255\255\255\255\
    \173\000\255\255\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\255\255\255\255\255\255\255\255\
    \173\000\255\255\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\255\255\255\255\
    \255\255\255\255\255\255\174\000\255\255\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\255\255\
    \255\255\255\255\255\255\174\000\255\255\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\255\255\255\255\255\255\255\255\255\255\175\000\255\255\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\255\255\255\255\255\255\255\255\175\000\255\255\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\255\255\255\255\255\255\255\255\
    \255\255\176\000\255\255\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\255\255\255\255\255\255\
    \255\255\176\000\255\255\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\255\255\
    \255\255\255\255\255\255\255\255\177\000\255\255\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \255\255\255\255\255\255\255\255\177\000\255\255\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\255\255\255\255\255\255\255\255\255\255\178\000\
    \255\255\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\255\255\255\255\255\255\255\255\178\000\
    \255\255\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\255\255\255\255\255\255\
    \255\255\255\255\179\000\255\255\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\255\255\255\255\
    \255\255\255\255\179\000\255\255\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \255\255\255\255\255\255\255\255\255\255\180\000\255\255\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\255\255\255\255\255\255\255\255\180\000\255\255\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\255\255\255\255\255\255\255\255\255\255\
    \181\000\255\255\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\255\255\255\255\255\255\255\255\
    \181\000\255\255\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\255\255\255\255\
    \255\255\255\255\255\255\182\000\255\255\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\255\255\
    \255\255\255\255\255\255\182\000\255\255\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\255\255\255\255\255\255\255\255\255\255\183\000\255\255\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\255\255\255\255\255\255\255\255\183\000\255\255\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\255\255\255\255\255\255\255\255\
    \255\255\184\000\255\255\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\255\255\255\255\255\255\
    \255\255\184\000\255\255\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\255\255\
    \255\255\255\255\255\255\255\255\185\000\255\255\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \255\255\255\255\255\255\255\255\185\000\255\255\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\255\255\255\255\255\255\255\255\255\255\186\000\
    \255\255\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\255\255\255\255\255\255\255\255\186\000\
    \255\255\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\187\000\187\000\187\000\187\000\187\000\
    \187\000\187\000\187\000\187\000\187\000\255\255\255\255\255\255\
    \255\255\255\255\187\000\255\255\187\000\187\000\187\000\187\000\
    \187\000\187\000\187\000\187\000\187\000\187\000\187\000\187\000\
    \187\000\187\000\187\000\187\000\187\000\187\000\187\000\187\000\
    \187\000\187\000\187\000\187\000\187\000\187\000\255\255\255\255\
    \255\255\255\255\187\000\255\255\187\000\187\000\187\000\187\000\
    \187\000\187\000\187\000\187\000\187\000\187\000\187\000\187\000\
    \187\000\187\000\187\000\187\000\187\000\187\000\187\000\187\000\
    \187\000\187\000\187\000\187\000\187\000\187\000\188\000\188\000\
    \188\000\188\000\188\000\188\000\188\000\188\000\188\000\188\000\
    \255\255\255\255\255\255\255\255\255\255\188\000\255\255\188\000\
    \188\000\188\000\188\000\188\000\188\000\188\000\188\000\188\000\
    \188\000\188\000\188\000\188\000\188\000\188\000\188\000\188\000\
    \188\000\188\000\188\000\188\000\188\000\188\000\188\000\188\000\
    \188\000\255\255\255\255\255\255\255\255\188\000\255\255\188\000\
    \188\000\188\000\188\000\188\000\188\000\188\000\188\000\188\000\
    \188\000\188\000\188\000\188\000\188\000\188\000\188\000\188\000\
    \188\000\188\000\188\000\188\000\188\000\188\000\188\000\188\000\
    \188\000\189\000\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\255\255\255\255\255\255\255\255\255\255\
    \189\000\255\255\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\189\000\255\255\255\255\255\255\255\255\
    \189\000\255\255\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\189\000\189\000\189\000\189\000\189\000\
    \189\000\189\000\189\000\189\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\255\255\255\255\
    \255\255\255\255\255\255\190\000\255\255\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\255\255\
    \255\255\255\255\255\255\190\000\255\255\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\190\000\
    \190\000\190\000\190\000\190\000\190\000\190\000\190\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\255\255\255\255\255\255\255\255\255\255\191\000\255\255\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\255\255\255\255\255\255\255\255\191\000\255\255\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\255\255\255\255\255\255\255\255\
    \255\255\192\000\255\255\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\255\255\255\255\255\255\
    \255\255\192\000\255\255\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\255\255\
    \255\255\255\255\255\255\255\255\193\000\255\255\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \255\255\255\255\255\255\255\255\193\000\255\255\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\193\000\193\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\255\255\255\255\255\255\255\255\255\255\194\000\
    \255\255\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\255\255\255\255\255\255\255\255\194\000\
    \255\255\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\195\000\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\255\255\255\255\255\255\
    \255\255\255\255\195\000\255\255\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\195\000\255\255\255\255\
    \255\255\255\255\195\000\255\255\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\195\000\195\000\195\000\
    \195\000\195\000\195\000\195\000\195\000\195\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \255\255\255\255\255\255\255\255\255\255\196\000\255\255\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\255\255\255\255\255\255\255\255\196\000\255\255\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\196\000\196\000\196\000\196\000\196\000\196\000\196\000\
    \196\000\197\000\197\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\255\255\255\255\255\255\255\255\255\255\
    \197\000\255\255\197\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\255\255\255\255\255\255\255\255\
    \197\000\255\255\197\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\197\000\197\000\197\000\197\000\
    \197\000\197\000\197\000\197\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\255\255\255\255\
    \255\255\255\255\255\255\198\000\255\255\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\198\000\255\255\
    \255\255\255\255\255\255\198\000\255\255\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\198\000\198\000\198\000\198\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\255\255\255\255\255\255\255\255\255\255\199\000\255\255\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\255\255\255\255\255\255\255\255\199\000\255\255\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\199\000\
    \199\000\199\000\201\000\255\255\255\255\255\255\255\255\201\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \201\000\201\000\201\000\202\000\202\000\202\000\202\000\202\000\
    \202\000\202\000\202\000\202\000\202\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\202\000\202\000\202\000\202\000\
    \202\000\202\000\208\000\208\000\208\000\208\000\208\000\208\000\
    \208\000\208\000\208\000\208\000\255\255\255\255\255\255\255\255\
    \201\000\255\255\255\255\208\000\208\000\208\000\208\000\208\000\
    \208\000\255\255\201\000\255\255\202\000\202\000\202\000\202\000\
    \202\000\202\000\215\000\215\000\255\255\255\255\201\000\255\255\
    \255\255\255\255\201\000\255\255\201\000\255\255\255\255\255\255\
    \201\000\255\255\255\255\208\000\208\000\208\000\208\000\208\000\
    \208\000\215\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\215\000\
    \255\255\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\255\255\255\255\255\255\255\255\255\255\
    \215\000\255\255\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\255\255\255\255\255\255\255\255\
    \215\000\255\255\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "\000\000\000\000\000\000\000\000\043\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\002\000\000\000\000\000\003\000\071\000\
    \000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\016\000\000\000\146\000\
    ";
  Lexing.lex_backtrk_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
    \007\000\000\000\000\000\000\000\000\000\007\000\007\000\007\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_default_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_trans_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\004\000\010\000\004\000\
    \000\000\000\000\001\000\000\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\004\000\000\000\
    \000\000\000\000\001\000\000\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\013\000\000\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\000\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\000\000\000\000\
    \000\000\001\000\000\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
    \001\000\001\000\001\000\001\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check_code = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\067\000\070\000\077\000\
    \255\255\255\255\004\000\255\255\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\060\000\255\255\
    \255\255\255\255\004\000\255\255\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\071\000\255\255\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\255\255\255\255\255\255\255\255\
    \255\255\215\000\255\255\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\255\255\255\255\255\255\
    \255\255\215\000\255\255\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\215\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_code = 
   "\255\001\255\255\002\255\255\000\002\255\003\255\255\002\003\255\
    \000\001\255";
}

let rec token lexbuf =
  lexbuf.Lexing.lex_mem <- Array.create 4 (-1) ;   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 132 "ocamltohtml_lexer.mll"
                      lxm
# 3650 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 133 "ocamltohtml_lexer.mll"
      ( incr comments ; 
        open_env COMMENTS;
        print_string  lxm;
        decr comments;
        close_env COMMENTS
      )
# 3659 "ocamltohtml_lexer.ml"

  | 1 ->
let
# 139 "ocamltohtml_lexer.mll"
                   lxm
# 3665 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 140 "ocamltohtml_lexer.mll"
      ( incr comments ; 
        open_env COMMENTS;
        print_string  lxm )
# 3671 "ocamltohtml_lexer.ml"

  | 2 ->
let
# 143 "ocamltohtml_lexer.mll"
                  lxm
# 3677 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 144 "ocamltohtml_lexer.mll"
      ( decr comments ;
        print_string lxm;
        close_env COMMENTS )
# 3683 "ocamltohtml_lexer.ml"

  | 3 ->
let
# 147 "ocamltohtml_lexer.mll"
                  c
# 3689 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 147 "ocamltohtml_lexer.mll"
                    ( html_escape (String.make 1 c))
# 3693 "ocamltohtml_lexer.ml"

  | 4 ->
# 148 "ocamltohtml_lexer.mll"
          (newline() )
# 3698 "ocamltohtml_lexer.ml"

  | 5 ->
let
# 155 "ocamltohtml_lexer.mll"
                                   lxm
# 3704 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 156 "ocamltohtml_lexer.mll"
      (  print_string lxm )
# 3708 "ocamltohtml_lexer.ml"

  | 6 ->
let
# 159 "ocamltohtml_lexer.mll"
           lxm
# 3714 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 160 "ocamltohtml_lexer.mll"
                    name
# 3719 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 161 "ocamltohtml_lexer.mll"
        ( 
          html_escape lxm;
          open_env KWD2; (* plop *)
          html_escape name;
          close_env KWD2;
        )
# 3728 "ocamltohtml_lexer.ml"

  | 7 ->
let
# 177 "ocamltohtml_lexer.mll"
         lxm
# 3734 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 178 "ocamltohtml_lexer.mll"
      (open_env KWD1 ;
       html_escape lxm ;
       close_env KWD1)
# 3740 "ocamltohtml_lexer.ml"

  | 8 ->
let
# 182 "ocamltohtml_lexer.mll"
                        lxm
# 3746 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 183 "ocamltohtml_lexer.mll"
      (open_env KWD2 ; 
       html_escape lxm ; 
       close_env KWD2)
# 3752 "ocamltohtml_lexer.ml"

  | 9 ->
let
# 193 "ocamltohtml_lexer.mll"
         lxm
# 3758 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 193 "ocamltohtml_lexer.mll"
             ( open_env KWD ;
               print_string lxm;
               close_env KWD )
# 3764 "ocamltohtml_lexer.ml"

  | 10 ->
let
# 199 "ocamltohtml_lexer.mll"
                                                                                                                            c
# 3770 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 200 "ocamltohtml_lexer.mll"
      ( if debug then prerr_endline "$$$$$$$$$$$$$ --------- CHAR";
        open_env STR ;
        html_escape c;
        close_env STR ;
      )
# 3778 "ocamltohtml_lexer.ml"

  | 11 ->
let
# 207 "ocamltohtml_lexer.mll"
                                                                      lxm
# 3784 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 208 "ocamltohtml_lexer.mll"
      ( open_env STR ;
        html_escape lxm;
        close_env STR)
# 3790 "ocamltohtml_lexer.ml"

  | 12 ->
let
# 214 "ocamltohtml_lexer.mll"
         lxm
# 3796 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 215 "ocamltohtml_lexer.mll"
      (open_env OP;
       (* print_string "$\\mathtt{";  *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP )
# 3804 "ocamltohtml_lexer.ml"

  | 13 ->
let
# 225 "ocamltohtml_lexer.mll"
            lxm
# 3810 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 226 "ocamltohtml_lexer.mll"
      (open_env OP;
       (* print_string "$\\mathtt{"; *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP )
# 3818 "ocamltohtml_lexer.ml"

  | 14 ->
let
# 236 "ocamltohtml_lexer.mll"
            lxm
# 3824 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 237 "ocamltohtml_lexer.mll"
      (open_env OP;
       (* print_string "$\\mathtt{"; *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP )
# 3832 "ocamltohtml_lexer.ml"

  | 15 ->
let
# 247 "ocamltohtml_lexer.mll"
                lxm
# 3838 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 248 "ocamltohtml_lexer.mll"
      (open_env OP;
       (* print_string "$\\mathtt{"; *)
       html_escape lxm;
       (* print_string "}$"; *)
       close_env OP )
# 3846 "ocamltohtml_lexer.ml"

  | 16 ->
let
# 256 "ocamltohtml_lexer.mll"
                                                   lxm
# 3852 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 257 "ocamltohtml_lexer.mll"
                           spaces
# 3857 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) (lexbuf.Lexing.lex_curr_pos + -1) in
# 258 "ocamltohtml_lexer.mll"
      (open_env MODULE;
       html_escape lxm;
       close_env MODULE;
       html_escape spaces;
       open_env OP;
       html_escape ".";
       close_env OP )
# 3867 "ocamltohtml_lexer.ml"

  | 17 ->
let
# 266 "ocamltohtml_lexer.mll"
                                                      lxm
# 3873 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 267 "ocamltohtml_lexer.mll"
      (print_string lxm)
# 3877 "ocamltohtml_lexer.ml"

  | 18 ->
let
# 269 "ocamltohtml_lexer.mll"
                             lxm
# 3883 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 270 "ocamltohtml_lexer.mll"
      ( html_escape lxm )
# 3887 "ocamltohtml_lexer.ml"

  | 19 ->
# 273 "ocamltohtml_lexer.mll"
        ( close_envs () ; raise Eof )
# 3892 "ocamltohtml_lexer.ml"

  | 20 ->
let
# 274 "ocamltohtml_lexer.mll"
          c
# 3898 "ocamltohtml_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 275 "ocamltohtml_lexer.mll"
      ( 
	print_char c;
	if debug then prerr_endline ("unrecognized character <" ^ Char.escaped c ^ ">") )
# 3904 "ocamltohtml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

end
(* OCaml to HTML ************************************************** *
 * (c) 2007, Philippe Wang **************************************** *
 * Licence: any version of GNU GPL or CeCILL-B, choose one that fits
 * best your needs and legislation. ******************************* *)

let html_of_ocaml filename out =
  let ic = open_in filename in
    Ocamltohtml_lexer.oc := out;
    Printf.fprintf out "<pre class='ocaml'>\n";
    try
      let lexbuf = Lexing.from_channel ic in
        while true do
          ignore (Ocamltohtml_lexer.token lexbuf)
        done
    with Ocamltohtml_lexer.Eof ->
      Printf.fprintf out "</pre>";
      (try close_in ic with _ -> ())

let html_of_ocaml_default_css out =
  Printf.fprintf out "<style type='text/css'>/* <!-- */
.ocaml{background-color:#EEE}

.c {
  color: #408080;
  font-style: italic;
}
/* Comment */
.err {
  border: 1px solid #ff0000;
}
/* Error */
.k {
  color: #008000;
  font-weight: bold;
}
/* Keyword */
.o {
  color: #666666;
}
/* Operator */
.cm {
  color: #408080;
  font-style: italic;
}
/* Comment.Multiline */
.cp {
  color: #bc7a00;
}
/* Comment.Preproc */
.c1 {
  color: #408080;
  font-style: italic;
}
/* Comment.Single */
.cs {
  color: #408080;
  font-style: italic;
}
/* Comment.Special */


.kwd{color: green;}
.kwd1{color: red;}
.kwd2{color: blue;}
.str{color: navy;}
.mname{color: orange;}
.com1{color: violet;}
.com2{color: fuchsia;}
.error{background-color:pink;}
/* --> */</style>\n%!"

let html_of_ocamlc args out =
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let tmp1 = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  match Sys.command (args ^ " > " ^ tmp1 ^ " 2> " ^ tmp) with
    | 2 ->
        let open Mpp_charstream in
        let cs = charstream_of_inchannel tmp (open_in tmp) in
        let _ = read_until '"' cs in
        let _ = charstream_take_n 1 cs in
        let filename = read_until '"' cs in
        let _ = charstream_take_n 8 cs in
        let line = int_of_string (read_until ',' cs) in
        let _ = charstream_take_n 13 cs in
        let colstart = int_of_string (read_until '-' cs) in
        let _ = charstream_take_n 1 cs in
        let colend = int_of_string (read_until ':' cs) in
        let fcs = charstream_of_inchannel filename (open_in filename) in
        let () = 
          for i = 2 to line do
            ignore(read_until '\n' fcs)
          done;
          ignore(fcs.take())
        in
        let s = charstream_take_n (colstart) (fcs) in
        let () = Printf.fprintf out "<pre class='ocaml'>" in
        let () = Ocamltohtml_lexer.oc := out in
        let () = Ocamltohtml_lexer.html_escape s in
        let es = charstream_take_n (colend - colstart + 1) fcs in
        let el = read_until '\n' fcs in
        let lexbuf1 = Lexing.from_string (es) in
        let lexbuf2 = Lexing.from_string (el) in
        let () =
          Printf.fprintf out "<span class='error'>";
          try while true do ignore(Ocamltohtml_lexer.token lexbuf1) done
          with Ocamltohtml_lexer.Eof -> 
            Printf.fprintf out "</span>";
            try while true do ignore(Ocamltohtml_lexer.token lexbuf2) done
            with Ocamltohtml_lexer.Eof -> ()
        in
        let () =
          ignore(cs.take());
          output_charstream out cs;
          Printf.fprintf out "</pre>\n"
        in ()
    | 0 -> ()
    | n -> 
        Printf.fprintf out "<pre class='error'>Command %s returned with code %d, I don't know what to do with it. Here's the output:\n" args n;
        Mpp_out.cat (Mpp_out.Out_channel out) tmp1;
        Printf.fprintf out "</span>"


let _ =
  let input = ref "/dev/stdin" in
  let output = ref "/dev/stdout" in
  let html_box = ref false in
    Arg.parse
      (Arg.align 
          [
            "-h", Arg.Set(html_box), "Output HTML with header and footer.";
            "-i", Arg.Set_string input, "f.ml Treat f.ml as input file (default is /dev/stdin)" ;
            "-o", Arg.Set_string output, "f.html Treat f.html as output file (default is /dev/stdout)";
          ])
      (fun s -> Printf.eprintf "I don't know what to do with <%s>.\n%!" s)
      (Printf.sprintf "Usage: %s [-i f.ml] [-o f.html]" Sys.argv.(0));
    let ic = open_in !input 
    and oc = open_out !output in
      Ocamltohtml_lexer.ic := ic;
      Ocamltohtml_lexer.oc := oc;
      if !html_box then Printf.fprintf oc  "<?xml version='1.0' encoding='utf-8'?>
<html>
<head>
<style type='text/css'>/* <!-- */
.kwd{color: green;}
.kwd1{color: red;}
.kwd2{color: blue;}
.str{color: navy;}
.mname{color: orange;}
.com1{color: violet;}
.com2{color: fuchsia;}
/* --> */</style>
</head>
<body><pre>
%!";
      try
        let lexbuf = Lexing.from_channel ic in
          while true do
            ignore (Ocamltohtml_lexer.token lexbuf)
          done
      with Ocamltohtml_lexer.Eof ->
        if !html_box then Printf.fprintf oc "</pre></body></html>";
        (try close_in ic with _ -> ());
        exit 0
