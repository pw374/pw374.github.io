(** A Lexer for OCaml, capable of source to source *)

type location = {
  sl : int; (* start line   *)
  sc : int; (* start column *)
  el : int; (* end   line   *)
  ec : int; (* end   column *)
}

let error loc m =
  Printf.printf "Characters %d:%d - %d:%d :\nError: %s\n"
    loc.sl loc.sc loc.el loc.ec m;
  exit 1

type t = {
    loc  : location;
    kind : kind;
  }
and kind =
  | Number   of number    | Upper    of string    | Lower    of string
  | String   of string    | Char of char * string | Keyword  of keyword
  | Keyop    of keyop     | Infix    of string    | Prefix   of string
  | Operator of string    | Comment  of string    | Spaces   of string

and number = Int of string | Float of string

and keyword =
  | And        | As       | Assert  | Begin    | Class       | Constraint
  | Do         | Done     | Downto  | Else     | End         | Exception
  | External   | False    | For     | Fun      | Function    | Functor
  | If         | In       | Include | Inherit  | Initializer | Lazy
  | Let        | Match    | Method  | Mod      | Module      | Mutable
  | New        | Object   | Of      | Open     | Private     | Rec
  | Sig        | Struct   | Then    | To       | True        | Try
  | Type       | Val      | Virtual | When     | While       | With
  | Parser

and keyop =
  | EpEq       | Sharp     | Amp        | AmpAmp    | Quote      | OpenP
  | CloseP     | Star      | Plus       | Coma      | Hyphen     | HyphenDot
  | HyphenGt   | Dot       | DotDot     | Colon     | ColonColon | ColonEq
  | ColonGt    | SemiColon | SemiColonSemiColon     | Lt         | LtHyphen
  | Eq         | Gt        | GtCloseSB  | GtCloseCB | Ip         | IpIp
  | OpenSB     | OpenSBLt  | OpenSBGt   | OpenSBVertLine         | CloseSB
  | Underscore | BackQuote | OpenCB     | OpenCBLt  | VertLine   | VertLineCloseSB
  | CloseCB    | Tild      | Lor        | Lsl       | Lsr        | Asr
  | Or         | Lxor      | Land

let keyops =
  [
    "!="   , EpEq               ;  "#"  , Sharp              ;
    "&"    , Amp                ;  "&&" , AmpAmp             ;
    "'"    , Quote              ;  "("  , OpenP              ;
    ")"    , CloseP             ;  "*"  , Star               ;
    "+"    , Plus               ;  ","  , Coma               ;
    "-"    , Hyphen             ;  "-." , HyphenDot          ;
    "->"   , HyphenGt           ;  "."  , Dot                ;
    ".."   , DotDot             ;  ":"  , Colon              ;
    "::"   , ColonColon         ;  ":=" , ColonEq            ;
    ":>"   , ColonGt            ;  ";"  , SemiColon          ;
    ";;"   , SemiColonSemiColon ;  "<"  , Lt                 ;
    "<-"   , LtHyphen           ;  "="  , Eq                 ;
    ">"    , Gt                 ;  ">]" , GtCloseSB          ;
    ">}"   , GtCloseCB          ;  "?"  , Ip                 ;
    "??"   , IpIp               ;  "["  , OpenSB             ;
    "[<"   , OpenSBLt           ;  "[>" , OpenSBGt           ;
    "[|"   , OpenSBVertLine     ;  "]"  , CloseSB            ;
    "_"    , Underscore         ;  "`"  , BackQuote          ;
    "{"    , OpenCB             ;  "{<" , OpenCBLt           ;
    "|"    , VertLine           ;  "|]" , VertLineCloseSB    ;
    "}"    , CloseCB            ;  "~"  , Tild               ;
    "land" , Land               ; "lor" , Lor                ;
    "lsl"  , Lsl                ; "lsr" , Lsr                ;
    "lxor" , Lxor               ; "asr" , Asr                ;
    "or"   , Or                 ;
  ]

let keywords =
  [ "and"       , And        ; "as"           , As             ;
    "assert"    , Assert     ; "lazy"         , Lazy           ;
    "begin"     , Begin      ; "class"        , Class          ;
    "constraint", Constraint ; "do"           , Do             ;
    "done"      , Done       ; "downto"       , Downto         ;
    "else"      , Else       ; "end"          , End            ;
    "exception" , Exception  ; "external"     , External       ;
    "false"     , False      ; "for"          , For            ;
    "fun"       , Fun        ; "function"     , Function       ;
    "functor"   , Functor    ; "if"           , If             ;
    "in"        , In         ; "include"      , Include        ;
    "inherit"   , Inherit    ; "initializer"  , Initializer    ;
    "let"       , Let        ; "match"        , Match          ;
    "method"    , Method     ; "mod"          , Mod            ;
    "module"    , Module     ; "mutable"      , Mutable        ;
    "new"       , New        ; "object"       , Object         ;
    "of"        , Of         ; "open"         , Open           ;
    "private"   , Private    ; "rec"          , Rec            ;
    "sig"       , Sig        ; "struct"       , Struct         ;
    "then"      , Then       ; "to"           , To             ;
    "true"      , True       ; "try"          , Try            ;
    "type"      , Type       ; "val"          , Val            ;
    "virtual"   , Virtual    ; "when"         , When           ;
    "while"     , While      ; "with"         , With           ;
    "parser"    , Parser
  ]

(** (side-effect-style) stream to which we may put back *)
type stream = <
  line   : int;
  column : int;
  getC   : char option;
  putC   : char -> unit;
>

(** stream_of_inchannel from a value of type [Pervasives.in_channel] *)
let stream_of_inchannel inchan : stream =
  object
    val mutable buffer = []
    val mutable line   = 1
    val mutable column = 1
    method line = line
    method column = column
    method getC =
      match buffer with
      | [] ->
        (try match input_char inchan with
         | '\n' as c ->
           line <- line + 1 ;
           column <- 0 ; Some c
         | c -> column <- column + 1 ; Some c
         with End_of_file -> None)
      | c::tl ->
        match c with
        | '\n' -> line <- line+1; column <- 0; buffer <- tl; Some c
        | _ -> column <- column + 1 ; buffer <- tl ; Some c
    method putC c =
      match buffer with
      | [] ->
        (match c with '\n' ->  line <- line - 1
                    | _ -> column <- column - 1);
        buffer <- [c]
      | l -> buffer <- c::l
  end

(** stream_of_string from a value of type [Pervasives.in_channel] *)
let stream_of_string s : stream =
  object
    val mutable buffer = []
    val mutable line   = 1
    val mutable column = 1
    val mutable index = -1
    val length = String.length s
    method line = line
    method column = column
    method getC =
      match buffer with
      | [] ->
        index<-index+1;
        if index < length then
          match s.[index] with
          | '\n' as c ->
            line <- line + 1 ;
            column <- 0 ; Some c
          | c -> column <- column + 1 ; Some c
        else
          None
      | c::tl ->
        match c with
        | '\n' -> line <- line + 1; column <- 0; buffer <- tl; Some c
        | _ -> column <- column + 1 ; buffer <- tl ; Some c
    method putC c =
      if index >= 0 && index <= String.length s && c = s.[index] && buffer = [] then
        index <- index - 1
      else
        match buffer with
        | [] ->
          (match c with
           | '\n' -> line <- line - 1
           | _ -> column <- column - 1);
          buffer <- [c]
        | l -> buffer <- c::l
  end

exception Lexical_error of location

let read_number (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let res = Buffer.create 42 in
  let float = ref false in
  let int = ref false in
  let dot = ref false in
  let rec f () =
    match stream#getC with
    | Some '0' ->
      (match stream#getC with
       | None -> Buffer.add_char res '0'
       | Some ('O' | 'o' | 'x' | 'X' | 'b' | 'B' as c) ->
         if Buffer.length res = 0 || !float || !int then
           error
	     {loc with el = stream#line; ec = stream#column}
	     "Lexical error: you gave a weird number"
         else int := true;
         Buffer.add_char res '0';
         Buffer.add_char res c;
         f()
       | Some('0' .. '9' as c) ->
         Buffer.add_char res '0';
         Buffer.add_char res c;
         f()
       | Some(c) -> stream#putC c ; Buffer.add_char res '0')
    | Some('1' .. '9' as c) ->
      Buffer.add_char res c;
      f ()
    | Some('E' | 'e' as e) ->
      if Buffer.length res = 0 || !float || !int then
        stream#putC e
      else
        (float := true;
         match stream#getC with
         | None -> stream#putC e
         | Some('0' .. '9' as c) -> 
           Buffer.add_char res e;
           Buffer.add_char res c;
           f()
         | Some(c) -> stream#putC c; stream#putC e)
    | Some('.' as c) ->
      if Buffer.length res = 0 || !float || !int then
	error
	  {loc with el = stream#line; ec = stream#column}
	  "Lexical error: you gave a weird number"
      else
        dot := true;
      Buffer.add_char res c;
      f()
    | Some('_') -> Buffer.add_char res '_'; f ()
    | Some(c) -> stream#putC c
    | None -> ()
  in
  let r = f (); Buffer.contents res in
  let n =
    if !float || !dot
    then
      Float
        (try ignore(float_of_string r); r
         with _ ->
           error loc "Lexical error while trying to read a float")
    else
      Int
        (try ignore(int_of_string r); r
         with _ ->
           Printf.printf "2(%d:%d) <%s>\n"
             stream#line
             stream#column r;
           error loc "Lexical error while trying to read an integer")
  in
  { kind = Number n; loc = {loc with el = stream#line; ec = stream#column} }


let read_upper (stream:stream) =
  let loc = { sl = stream#line; sc = stream#column;
              el = stream#line; ec = stream#column; } in
  let res = Buffer.create 42 in
  let rec f () =
    match stream#getC with
    | Some('A' .. 'Z' as c) -> Buffer.add_char res c; f ()
    | Some('a' .. 'z' | '0' .. '9' | '_' as c) ->
      if Buffer.length res = 0 then failwith "bad use of lexer (read_upper)";
      Buffer.add_char res c;
      f ()
    | Some(c) -> stream#putC c
    | None -> ()
  in
  let res = f(); Buffer.contents res in
  { kind = Upper res ; loc = {loc with el = stream#line; ec = stream#column} }

let read_lower (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let string_of_char c = String.make 1 c in
  let rec f res =
    match stream#getC with
    | Some('a' .. 'z' | '_' as c) -> f (res ^ (string_of_char c))
    | Some('A' .. 'Z' | '0' .. '9' as c) ->
      if res = "" then failwith "bad use of lexer (read_lower)";
      f (res ^ (string_of_char c))
    | Some(c) -> stream#putC c ; res
    | None -> res
  in
  let res = f "" in
  try { kind = Keyword (List.assoc res keywords) ;
        loc = {loc with el = stream#line; ec = stream#column}}
  with Not_found ->
    try { kind = Keyop (List.assoc res keyops) ;
          loc = {loc with el = stream#line; ec = stream#column}}
    with Not_found ->
      { kind = Lower res;
        loc = {loc with el = stream#line; ec = stream#column}}

let read_infix (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let string_of_char c = String.make 1 c in
  let rec f res =
    match stream#getC with
    | Some('!' | '?' | '~' as c) ->
      if res = "" then failwith "bad use of lexer (read_infix)";
      f (res ^ (string_of_char c))
    | Some('$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>'
          | '@' | '^' | '|' as c) ->
      f (res ^ (string_of_char c))
    | Some(c) -> stream#putC c ; res
    | None -> res
  in
  let res = f "" in
  try { kind = Keyop (List.assoc res keyops);
        loc = {loc with el = stream#line; ec = stream#column}}
  with Not_found ->
    { kind = Infix res ; loc = {loc with el = stream#line; ec = stream#column}}

let read_prefix (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let string_of_char c = let s = " " in s.[0] <- c ; s  in
  let rec f res =
    match stream#getC with
    | Some('!' | '?' | '~' as c) ->
      f (res ^ (string_of_char c))
    | Some('$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>'
          | '@' | '^' | '|' as c) ->
      if res = "" then failwith "bad use of lexer (read_prefix)";
      f (res ^ (string_of_char c))
    | Some(c) -> stream#putC c ; res
    | None -> res
  in
  let res = f "" in
  try { kind = Keyop (List.assoc res keyops);
        loc = {loc with el = stream#line; ec = stream#column}}
  with Not_found ->
    { kind = Prefix res ;
      loc = {loc with el = stream#line; ec = stream#column}}

let read_char (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let rec int i =
    match stream#getC with
    | Some c ->
      begin match c with
        | '0' -> int (i*10)       | '1' -> int (i*10 + 1)   | '2' -> int (i*10 + 2)
        | '3' -> int (i*10 + 3)   | '4' -> int (i*10 + 4)   | '5' -> int (i*10 + 5)
        | '6' -> int (i*10 + 6)   | '7' -> int (i*10 + 7)   | '8' -> int (i*10 + 8)
        | '9' -> int (i*10 + 9)
        | '\'' ->
          { kind = Char (char_of_int i);
            loc = {loc with el = stream#line; ec = stream#column}}
        | _ ->
          error
	    { loc with el = stream#line; ec = stream#column }
            "Lexical error while trying to read a char"
      end
    | None ->
      error
	{ loc with el = stream#line; ec = stream#column }
        "Lexical error while trying to read a char"      
  in
  match stream#getC with
  | Some('\'') ->
    (match stream#getC with
     | Some('\\') ->
       (match stream#getC with
        | Some('\'') ->
          if stream#getC = Some '\'' then
            { kind = Char '\'' ;
              loc = {loc with el = stream#line; ec = stream#column}}
          else
            error
	      {loc with el = stream#line; ec = stream#column}
	      "Lexical error: Illegal backslash escape in character"
        | Some('n') ->
          if stream#getC = Some '\'' then
            { kind = Char '\n' ;
              loc = {loc with el = stream#line; ec = stream#column}}
          else
            error
	      {loc with el = stream#line; ec = stream#column}
	      "Lexical error: Illegal backslash escape in character"
        | Some('r') ->
          if stream#getC = Some '\'' then
            { kind = Char '\r' ;
              loc = {loc with el = stream#line; ec = stream#column}}
          else
            error
	      {loc with el = stream#line; ec = stream#column}
	      "Lexical error: Illegal backslash escape in character"
        | Some('t') ->
          if stream#getC = Some '\'' then
            { kind = Char '\t' ;
              loc = {loc with el = stream#line; ec = stream#column}}
          else
            error
	      {loc with el = stream#line; ec = stream#column}
	      "Lexical error: Illegal backslash escape in character"
        | Some('0' .. '9') -> int 0
        | Some('\\') ->
          if stream#getC = Some '\'' then
            { kind = Char '\\' ;
              loc = {loc with el = stream#line; ec = stream#column}}
          else
            error
	      {loc with el = stream#line; ec = stream#column}
	      "Lexical error: Illegal backslash escape in character"
        | Some _ | None ->
          error
	    {loc with el = stream#line; ec = stream#column}
	    "Lexical error: Illegal backslash escape in character"
       )
     | Some(c) ->
       if stream#getC = Some '\''
       then
         { kind = Char c;
           loc = {loc with el = stream#line; ec = stream#column}}
       else
         (stream#putC c;
          { kind = Keyop Quote ;
            loc = {loc with el = stream#line; ec = stream#column}})
     | None -> 
       error
	 {loc with el = stream#line; ec = stream#column}
	 "Lexical error: Illegal backslash escape in character"
    )
  | _ -> assert false

let read_string (stream:stream) =
  let res =
    let loc = { sl = stream#line ; sc = stream#column ;
                el = stream#line ; ec = stream#column ; } in
    let string_of_char c = String.make 1 c in
    let rec f res =
      match stream#getC with
      | Some('"') -> res
      | Some('\\') ->
        (match stream#getC with
         | Some('"') -> f (res ^ "\"")
         | Some('n') -> f (res ^ "\n")
         | Some('r') -> f (res ^ "\r")
         | Some('t') -> f (res ^ "\t")
         | Some('\\') -> f (res ^ "\\")
         | _   ->
           error
	     {loc with el = stream#line; ec = stream#column}
	     "Lexical error: Illegal backslash escape in string")
      | Some(c) -> f (res ^ (string_of_char c))
      | None ->
          error
	    {loc with el = stream#line; ec = stream#column}
	    "Lexical error: Incomplete string"
    in
    match stream#getC with
    | Some('"') ->
      { kind = String (f "");
        loc = {loc with el = stream#line; ec = stream#column}}
    | _ -> assert false
  in res

let read_comment (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let string_of_char c = String.make 1 c in
  let rec f res level =
    match stream#getC with
    | None ->
      error
	{loc with el = stream#line; ec = stream#column}
	"Lexical error: Incomplete comment"
    | Some('(') ->
      (match stream#getC with
       | None ->
         error
	   {loc with el = stream#line; ec = stream#column}
	   "Lexical error: Incomplete comment"
       | Some('*') -> f (res ^ "(*") (level+1)
       | Some(c) -> f (res ^ "(" ^(string_of_char c)) level)
    | Some('*') ->
      (match stream#getC with
       | None ->
          error
	    {loc with el = stream#line; ec = stream#column}
	    "Lexical error: Incomplete comment"
       | Some(')') ->
         if level = 1 then res ^ "*)"
         else f (res ^ "*)") (level-1)
       | Some('*') -> stream#putC '*' ; f (res ^ "*") level
       | Some(c) -> f (res ^ "*" ^(string_of_char c)) level)
    | Some(c) -> f (res ^ (string_of_char c)) level
  in
  { kind = Comment ( f "" 0) ;
    loc = {loc with el = stream#line; ec = stream#column}}

let read_op (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let string_of_char c = String.make 1 c in
  let rec f res =
    match stream#getC with
    | Some('!' | '&' | '*' | '+' | '-' | '.' | ':'
          | '<' | '=' | '>' | '?'
          | '[' | ']' | '`' | '{' | '|' | '}'
          | '~' as c) -> f (res ^ (string_of_char c))
    | Some('(') -> if res = "" then "(" else (stream#putC '(' ; res)
    | Some(')') -> if res = "" then ")" else (stream#putC ')' ; res)
    | Some('#') -> if res = "" then "#" else (stream#putC '#' ; res)
    | Some('_') -> if res = "" then "_" else
	error
	  {loc with el = stream#line; ec = stream#column}
	  "Lexical error while reading an operator"
    | Some(',') -> if res = "" then "," else  (stream#putC ',' ; res)
    | Some(';') ->
      if res = "" then
        (match stream#getC with
         | None -> res
         | Some(';') -> ";;"
         | Some(c) -> stream#putC c; res)
      else (stream#putC ';' ; res)
    | Some(c) -> stream#putC c ; res
    | None -> assert(res<>""); res
  in
  let res = f "" in
  try { kind = Keyop (List.assoc res keyops);
        loc = {loc with el = stream#line; ec = stream#column}}
  with Not_found ->
    { kind = Operator res ;
      loc = {loc with el = stream#line; ec = stream#column}}

let read_spaces (stream:stream) =
  let loc = { sl = stream#line ; sc = stream#column ;
              el = stream#line ; ec = stream#column ; } in
  let res = Buffer.create 42 in
  let rec f () =
    match stream#getC with
    | Some(' ' | '\t' | '\n' | '\r' as c) -> Buffer.add_char res c; f ()
    | Some(c) -> stream#putC c; Spaces (Buffer.contents res)
    | None -> Spaces (Buffer.contents res)
  in
  let k = f() in
  { kind = k;
    loc = {loc with el = stream#line; ec = stream#column} }

let read (stream:stream) =
  let rec f res =
    let loc = { sl = stream#line ; sc = stream#column ;
                el = stream#line ; ec = stream#column ; } in
    match stream#getC with
    | Some('0' .. '9' as c) -> stream#putC c ; f ((read_number stream)::res)
    | Some('A' .. 'Z' as c) -> stream#putC c ; f ((read_upper  stream)::res)
    | Some('a' .. 'z' | '_' as c) -> stream#putC c ; f ((read_lower  stream)::res)
    | Some(' ' | '\t' | '\n' | '\r' as c) -> stream#putC c ; f ((read_spaces stream)::res)
    | Some('!' | '?'  | '~' as c) ->
      stream#putC c ; f ((read_prefix stream)::res)
    | Some('=' | '<' | '>' | '@' | '^' | '|' | '&' | '+' | '-' | '*' | '/'
          | '$' | '%' as c) ->
      stream#putC c ; f ((read_infix stream)::res)
    | Some('(') ->
      (match stream#getC with
       | None ->
         error
	   {loc with el = stream#line; ec = stream#column}
           "Lexical error: Unclosed parenthesis"
       | Some('*') ->
	 stream#putC '*'; stream#putC '(';
	 f ((read_comment stream)::res)
       | Some c ->
	 stream#putC c; f ((read_op stream)::res))
    | Some('\'' as c) ->
      stream#putC c; f ((read_char stream)::res)
    | Some('"' as c) ->
      stream#putC c; f ((read_string stream)::res)
    | Some('\\') ->
      error
	{ sl = stream#line; sc = stream#column - 1;
	  el = stream#line; ec = stream#column}
	"Lexical error: unexpected presence of a backslash"
    | Some(c) ->
      stream#putC c ;
      f ((read_op stream)::res)
    | None -> res
  in f []


let l =
  List.rev (read (stream_of_inchannel (open_in "lexer.ml")))


let l =
  List.rev (read (stream_of_inchannel (open_in "test.ml")))

let l =
  List.rev (read (stream_of_string "A 42"))

(*
let _ =
  read (stream_of_inchannel (open_in Sys.argv.(1)))
*)
