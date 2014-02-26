let split f s =
  let res = ref [] in
  let b = Buffer.create 42 in
  let rec loop i s =
    if i >= String.length s then
      let bc = Buffer.contents b in
      Buffer.clear b;
      res := bc :: !res;
    else
      match f i s with
      | `Split ->
         let bc = Buffer.contents b in
         Buffer.clear b;
         res := bc :: !res;
         loop (i+1) s
      | `Continue ->
         Buffer.add_char b (s.[i]);
         loop (i+1) s
      | `Split_with (add, new_i, new_s) ->
         Buffer.add_string b add;
         let bc = Buffer.contents b in
         Buffer.clear b;
         res := bc :: !res;
         loop new_i new_s
  in
  loop 0 s;
  List.rev !res
(* val split :
   (int ->
   string -> [< `Continue | `Split | `Split_with of string * int * string ]) ->
   string -> string list *)

let _ = (* split on " " *)
  split (fun i s -> match s.[i] with ' ' -> `Split | _ -> `Continue) "hello foo bar"
(* ["hello"; "foo"; "bar"] *)

let _ = (* split on "foo" *)
  split
    (fun i s ->
       if String.length s > i+3 && String.sub s i 3 = "foo" then
         `Split_with ("", i+3, s)
       else `Continue)
    "hello foo bar"
(* ["hello "; " bar"] *)

let split_on_string sep s =
  if sep = "" then invalid_arg "split_on_string";
  let ls = String.length sep in 
  split
    (fun i s ->
       if String.length s > i+ls && String.sub s i ls = sep then
         `Split_with ("", i+ls, s)
       else `Continue)
    s
(* val split_on_string : string -> string -> string list *)

let _ =
  split_on_string "foo" "hello foo bar baz"
(* ["hello "; " bar baz"] *)

let _ =
  split_on_string "foo" ""
(* [""] *)

let _ =
  split_on_string "" ""
(* Exception: Invalid_argument "split_on_string". *)

let split_on_spaces s =
  split
    (fun i s ->
       match s.[i] with
       | ' ' ->
         let rec loop i =
           if String.length s > i && s.[i] = ' ' then
             loop (i+1)
           else
             `Split_with("", i, s)
         in loop i
       | _ -> `Continue 
    )
    s
(* val split_on_spaces : string -> string list *)

let _ = split_on_spaces "plop                plop          "
(* ["plop"; "plop"; ""] *)


let split_sentences s =
  split
    (fun i s ->
       match s.[i] with
       | '.' | '!' | '?' ->
         let b = Buffer.create 42 in
         let rec loop i =
           if String.length s > i then
             match s.[i] with
             | '.' | '!' | '?' | ' ' ->
               Buffer.add_char b s.[i];
               loop (i+1)
             | _ ->
               `Split_with(Buffer.contents b, i, s)
           else
             `Split_with(Buffer.contents b, i, s)
         in loop i
       | _ -> `Continue)
    s
(* val split_sentences : string -> string list *)

let _ = split_sentences "Bonjour, comment ça va?! Ça va. Merci. Au revoir! Déjà?? Oui!!"
(* ["Bonjour, comment ça va?! "; "Ça va. "; "Merci. "; "Au revoir! "; "Déjà?? "; "Oui!!"; ""] *)
