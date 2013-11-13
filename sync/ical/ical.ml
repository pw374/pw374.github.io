(* -*- coding:utf-8; -*- *)
(* (c) 2013 Philippe Wang <philippe.wang@cl.cam.ac.uk> *)
(* Licence: ICS *)


open Printf

exception Syntax_error of string
let syntax_error s ln cn = raise (Syntax_error (sprintf "(%d:%d): %s." ln cn s))
let syntax_assert b s ln cn  = if not b then syntax_error s ln cn

type line = {
  name: string;
  value: string;
  name_start: int * int;
  value_start: int * int;
  value_end: int * int;
}

let lex_ical s =
  let name = Buffer.create 42
  and value = Buffer.create 42 
  and name_start = ref (0,0)
  and value_start = ref (0,0)
  in
  let sl = String.length s in
  let rec loop lines i (colon:bool) (nl:bool) (lc:int) (cc:int) =
    if i >= sl then
      { name=Buffer.contents name;
        value=Buffer.contents value;
        name_start = !name_start;
        value_start = !value_start;
        value_end = lc, cc;}
      ::lines
    else
      match s.[i] with
      | '\n' ->
        begin
          syntax_assert (not nl) "unexpected double newline" lc cc;
          if i >= sl-1 then
            { name=Buffer.contents name;
              value=Buffer.contents value;
              name_start = !name_start;
              value_start = !value_start;
              value_end = lc, cc;}
            ::lines
          else if s.[i+1] <> ' ' then
            let nv = {
              name=Buffer.contents name;
              value=Buffer.contents value;
              name_start = !name_start;
              value_start = !value_start;
              value_end = lc, cc;
            }
            in
            Buffer.clear name; Buffer.clear value;
            name_start := (lc+1,0);
            loop
              (nv::lines)
              (i+1) false true (lc+1) 0
          else
            begin
              syntax_assert colon "unexpected end of line" lc cc;
              loop
                lines
                (i+2) colon false (lc+1) 0
            end
        end
      | '\r' -> (* just ignore \r for now *)
        loop lines (i+1) colon nl lc cc
      | ' ' as c ->
        syntax_assert colon "unexpected space before colon" lc cc;
        Buffer.add_char value c;
        loop lines (i+1) colon false lc (cc+1)
      | ':' as c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) true false lc (cc+1)
          end
        else
          begin
            value_start := (lc,cc);
            loop lines (i+1) true false lc (cc+1)
          end
      | c ->
        if colon then
          begin
            Buffer.add_char value c;
            loop lines (i+1) colon false lc (cc+1)
          end
        else
          begin
            Buffer.add_char name c;
            loop lines (i+1) colon false lc (cc+1)
          end
  in
  List.rev (loop [] 0 false true 1 0)
;;

let parse_ical l =
  (* ob = opened blocks *)
  let rec loop (res:'a list) (ob:string option) = function
    | [] ->
      begin match ob with
        | Some e -> syntax_error (sprintf "unclosed block %s" e) (-1) (-1);
        | None -> res, []
      end
    | {name="BEGIN"; value=e}::tl ->
      let block, tl = loop [] (Some e) tl in
      loop ((`Block(e, block))::res) ob tl
    | {name="END"; value=e} as v::tl ->
      begin match ob with
        | Some x when x = e ->
          res, tl
        | Some x ->
          syntax_error (sprintf "unexpected end of block %s, expected end of block %s" x e)
            (fst v.name_start) (snd v.name_start)
        | None ->
          syntax_error (sprintf "unexpected end of block %s" e)
            (fst v.name_start) (snd v.name_start)
      end
    | {name; value}::tl ->
      loop ((`Assoc(name, `Raw value))::res) ob tl
  in loop [] None l

let x =
  lex_ical "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VTODO
DTSTAMP:19980130T134500Z
SEQUENCE:2
UID:uid4@host1.com
ACTION:AUDIO
TRIGGER:19980403T120000
ATTACH;FMTTYPE=audio/basic:http://example.com/pub/audio-
 files/ssbanner.aud
REPEAT:4
DURATION:PT1H
END:VTODO
END:VCALENDAR
";;

let y = parse_ical x;;

let x =
lex_ical "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//ABC Corporation//NONSGML My Product//EN
BEGIN:VJOURNAL
DTSTAMP:19970324T120000Z
UID:uid5@host1.com
ORGANIZER:MAILTO:jsmith@example.com
STATUS:DRAFT
CLASS:PUBLIC
CATEGORIES:Project Report, XYZ, Weekly Meeting
DESCRIPTION:Project xyz Review Meeting Minutes\\n
 Agenda\\n1. Review of project version 1.0 requirements.\\n2.
 Definition
 of project processes.\\n3. Review of project schedule.\\n
 Participants: John Smith, Jane Doe, Jim Dandy\\n-It was
  decided that the requirements need to be signed off by
  product marketing.\\n-Project processes were accepted.\\n
 -Project schedule needs to account for scheduled holidays
  and employee vacation time. Check with HR for specific
  dates.\\n-New schedule will be distributed by Friday.\\n-
 Next weeks meeting is cancelled. No meeting until 3/23.
END:VJOURNAL
END:VCALENDAR
";;

let y = parse_ical x;;
let () = () ;;

