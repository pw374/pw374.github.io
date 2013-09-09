
# 0 "_tmp/2013-09-05-21-31-26-about-omd.main.ml"
# 0 "common.ml"
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

let input_command e =
  flush stdout;
  let tmp = Filename.temp_file ~temp_dir:"./" "tmp" "plop" in
  command (e ^ " > " ^ tmp);
  let r = input tmp in
  Sys.remove tmp;
  r
  
let selfbn = try Sys.getenv "selfbn" with Not_found -> ""
# 0 "_tmp/2013-09-05-21-31-26-about-omd.md.ml"
 let _ = print_string ""
 module Meta : Meta = struct
 let title = "OMD: a Markdown parser in OCaml"
 let id = "pw374.github.io--2013-09-05-22-31-26--29154"
 let xmldate = "2013-09-05T22:31:26+01:00"
 let rssdate = "Thu, 05 Sep 2013 22:31:26 +01:00"
 let date = "2013-09-05 22:31:26+01:00"
 let tags = [ "ocaml"; "markdown"; "software development"; ]
end
include Meta
 let _ = print_string "\n"
 module Post(Unit:Unit) = struct  let _ = print_string "## "
 let _ = !!title  let _ = print_string "\n### Motivations\n\nThere  are  so many  existing  implementations  of  Markdown, why  yet\nanother one?   Well, after asking  the OCaml community  about existing\nways to parse and manipulate  Markdown documents, it seemed that there\nwere no stand-alone complete Markdown parser written in OCaml, meaning\nthat they  were incomplete (i.e., not fully  compatible with Markdown)\nor  interfaces  to  some  Markdown  parsers  implemented  using  other\nlanguages than OCaml.\n\nSince in OCaml  Labs we're working a lot with  Github, and Github uses\nMarkdown a  lot (Github  web pages hosting,  Github issues,  etc.) and\nother  sites are also  using Markdown  as well,  and Markdown  is very\npopular and  easy to  learn, and  flexible in the  sense that  you can\nalways  fall back  to HTML  when you  want to  express  something that\ndoesn't have  a special syntax in  Markdown, blah blah blah,  it was -\nsomehow - time to have  a Markdown parser implemented using OCaml. And\npreferably  OCaml alone,  meaning that  one that  has  OCaml installed\nshould be able  to use it easily without having to  deal with too many\ndependencies. Well, there it is: OMD!\n\n### Issues... were essentially with Markdown itself\n\nEvery computer  scientist knows how to  write a parser  for a language\nthat is as simple as Markdown. Well, no, not every computer scientist,\nsadly, but at least every programmer should. Ok, sadly it isn't really\nthe case either. Anyways.\n\nMarkdown is a rather simple language,  if you look at the specs. Well,\nthat depends on what you actuall call \"specs\", of course. According to\n[Wikipedia](http://en.wikipedia.org/wiki/Markdow),     Markdown    was\ninvented by [John Gruber](http://daringfireball.net), and Aaron Swartz\nhelped  him. Then the  original document  that describes  the Markdown\nlanguage          is          available         online          there:\n<http://daringfireball.net/projects/markdown/syntax>.  And you can see\nthat         searching        for         [\"Markdown+syntax\"        on\nGoogle](https://www.google.com/#q=Markdown+syntax) makes that page the\ntop result (and here again, I'm kind  of helping it be and stay on the\ntop...).\n\nActually, Markdown is not that  simple to parse.  Why? Because so many\nthings are  just left to the  imagination of the  people who implement\nparsers  for  this  language,   because  programmes  can't  decide  by\nthemselves  what's right  to do.   It's  really the  author(s) of  the\nprogramme that  decides what the  programme does. And in  Markdown, so\nmany  things  are ambiguous  and  Gruber's  document doesn't  actually\ndescribe a  grammar at all.  It just tells  you how to write  this and\nthat, but if you write it slightly differently, it doesn't tell you\nwhat the outcome would be. \n***In other words, there are no errors in Markdown.***\nEvery text file is a valid Markdown file, it might just be converted \nto some HTML you wouldn't expect. For instance, you may write a link \nusing this  syntax:\n\n```\n[blah blah blah](the-url-which-can-be-absolute-or-relative)\n```\n\nand it gets converted to \n```\n<a href=\"the-url-which-can-be-absolute-or-relative\">blah blah blah</a>\n```\nBut if you forget the closing parenthesis, then it becomes this instead\n\n```\n<p>[blah blah blah](the-url-which-can-be-absolute-or-relative</p>\n```\nprecisely because nothing is wrong in Markdown.\n\nAnd what if there are parentheses in your URL? What if they are unbalanced?\n\n\n#### Flagrant Ambiguity\n\nThe following is some text that has to mean something in Markdown...\n\n```\n* Alice is falling in love with Bob.\n    * Bob is secretly in love with Alice, but he's seeing Eve.\n  * Eve is in-between, she loves both Alice and Bob, she can't help it.\n```\n\nSo, Pandoc, which a tool that converts a document in language A to a\ndocument in language B, where A and B can be the same or different\nlanguages amongst LaTeX, HTML, Markdown and (many) others, considers\nthat Eve is on the same level as Bob. So its HTML output is\n\n```\n<ul>\n<li>Alice is falling in love with Bob.\n<ul>\n<li>Bob is secretly in love with Alice, but he's seeing Eve.</li>\n</ul></li>\n<li>Eve is in-between, she loves both Alice and Bob, she can't help it.</li>\n</ul>\n```\n\nBut if instead you add Dylan as in\n\n```\n* Alice is falling in love with Bob.\n   * Dylan, a new character, is being conceived here.\n    * Bob is secretly in love with Alice, but he's seeing Eve.\n  * Eve is in-between, she loves both Alice and Bob, she can't help it.\n```\n\nthen *of course* Eve is not on the same level as Bob anymore and goes\nwith Dylan and Alice, Bob is on his own.\n\n```\n<ul>\n<li>Alice is falling in love with Bob.</li>\n<li>Dylan, a new character, is being conceived here.\n<ul>\n<li>Bob is secretly in love with Alice, but he's seeing Eve.</li>\n</ul></li>\n<li>Eve is in-between, she loves both Alice and Bob, she can't help it.</li>\n</ul>\n```\n\nThis doesn't make much sense...\n\n\nAnd Github's embedded Markdown  to HTML converter chooses some similar\nbroken semantics. If one writes bullets on different levels, it shouldn't\nbe meaningless.\n\nAlso, on Github, if you write\n\n```\n* 1\n  * 2\n    * 3\n      * 4\n        * 5\n          * 6\n            * 7\n              * 8\n                * 9\n                  * 10\n```\n\nThen 2 and 3 are on the same level, as for 4 and 5, 6 and 7, and 8 and 9.\n1 and 10 are on their own. And if you extract 2 and 3, meaning\n\n```\n  * 2\n    * 3\n```\nthen 2 and 3 are not on the same level anymore! \nSee for yourself:\n\n- raw version: <https://raw.github.com/pw374/sandbox/master/mad-lists.md>\n- rendered-by-Github version: <https://github.com/pw374/sandbox/blob/master/mad-lists.md>\n\nWith OMD, hopefully, there is a deterministic meaning for each level of indentation. The list where there were Alice, Bob and Eve is converted to this to the least insane semantics I could think of.\n\nThe idea  is that, since Eve  is neither on  the same level as  Bob or\nAlice, Eve should be in a new list (because, obviously, she's the only\none on that  level anyway). So she  is in a new list.  And since she's\nnot on a deeper level than Bob, she shouldn't be on a sub-list of his.\nBut  she is  on a  deeper level  than Alice,  so she  has to  be  on a\nsub-list of hers. So, here is the HTML that OMD produces:\n\n```\n<ul>\n <li>Alice is falling in love with Bob.\n  <ul>\n   <li>Bob is secretly in love with Alice, but he&apos;s seeing Eve.\n   </li>\n  </ul>\n  <ul>\n   <li>Eve is in-between, she loves both Alice and Bob, she can&apos;t help it.\n   </li>\n  </ul>\n </li>\n</ul>\n```\n\nOh,  you might  have  noticed  that OMD  converts  quotes to  `&apos;`\nbecause otherwise I  would need to differentiate when  they have to be\nconverted from when it's optional.\n\n### Implementation\n\nPandoc's documentation says \n\n> In contrast to most existing tools for converting markdown to HTML,\nwhich use regex substitutions, Pandoc has a modular design: it\nconsists of a set of readers, which parse text in a given format and\nproduce a native representation of the document, and a set of writers,\nwhich convert this native representation into a target format.\nThus, adding an input or output format requires only adding a reader\nor writer.\n\nCome on,  most tools are using regular  expressions substitutions?!  I\ncan only imagine the nightmare that  it must be to implement and debug\nsuch an implementation  -- no wait, I can't because  I just don't want\nto imagine such a nightmare.\n\nI used functions and function calls, a lot of them are tail recursive,\nnot  all of  them but  then it  means  I don't  need them  to be  tail\nrecursive, and  those functions  basically take a  list of  tokens and\nreturn a  new list  with possibly fewer  tokens and the  expression to\nwhich the missing ones were converted into.\n\nSo far, in version 0.4 (which is  not released yet at the time I write\nthis), there's a little less than  8k lines of pure OCaml code.  (Note\nthat I didn't write \"pure functional\", I wrote \"pure OCaml\".)\n\nOMD is an  open-source free and libre software  library that any OCaml\ndeveloper can use  (hopefully quite easily since it  doesn't depend on\nanything else  that the standard  OCaml compiler and library).  And of\ncourse, it's also  a tool for any one who write  Markdown and wants it\nto be  converted (quickly)  to HTML.  OMD, so far,  is about  10 times\nfaster than  Pandoc, and  I didn't  even make any  efforts to  make it\nfast.\n\n#### Compatibility\n\nOMD has been developed using OCaml 4.0.1, [Christophe Troestler made\nme make it compatible with OCaml\n3.12.1](https://github.com/pw374/omd/issues/19).  Then I guess it\nmight work with older version of OCaml but it's not certain (mainly\nbecause OCaml's standard library has slightly changed, as I think I\ndon't use any language features that were introduced in 3.12 or 4.0).\n\nBy  the way,  thank  you  Christophe for  your  support, interest  and\ncontributions to OMD :-)\n\n### Future of OMD\n\nOMD  already is  in OPAM.   A  very stable  version of  OMD should  be\nreleased soon.   As a  tool, it takes  Markdown as input  and produces\nHTML as output.   A Markdown backend has been  freshly implemented, so\nyou can output  Markdown as well, which is  quite useful for debugging\nor if you  want to know how many iterations you  need before you reach\nthe fix  point. You  can also  output \"text\", in  the sense  that it's\nbasically the HTML  without the HTML tags, so  it's very non-formatted\ntext.  There  also are  options to output  HTML table of  contents and\nparametrise their depths.\n\n### OMD and OCaml.org\n\nWe    are    at   OCaml    Labs    making    a    new   website    for\n[ocaml.org](http://ocaml.org).   The  design   is  being  provided  by\n[onespacemedia](http://www.onespacemedia.com).    At   the  time   I'm\nwriting  these  lines, I'm  using  the  HTML/CSS/JS  for the  upcoming\nOCaml.org to style my new Github-hosted website, so I can play with it\n*more*.\n\nMost pages will be written in Markdown instead of HTML, so that people\nof the OCaml community may contribute to it in a more convenient way.\n\nAnd of course, that means that OMD will be used for OCaml.org.\n\n\n"
 end  let _ = print_string "\n"
# 0 "ml_to_md.ml"
(* running Post(Something) must generate the main contents of the post *)
include Post(struct end)

let _ =
  printf "<div style='font-size:80%%;'><em>started on %s, (re)generated on %s</em></div>" 
    date 
    (input_command "date --rfc-3339=seconds")
