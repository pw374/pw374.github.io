(* Homework 1 *)

(* # Approximation of π *)

(* From the following statement, write a function that takes a integer
   parameter (n) and returns the corresponding approximated value of
   π. 

   --------------------------------------------------------------
   Let S be a square of (n*n) dots where (n) is an even integer.
   Let C be the largest disc possible in S.
   By counting all dots of the square S that are in C, you can deduce an
   approximated value of pi.
   -------------------------------------------------------------- 

   You need very basic mathematics, which I hope you learnt years ago:
   - how to calculate the area of a disc
   - how to calculate the area of a square
   - how to calculate the distance that separates a dot of coordinates
   (X,Y) from another dot.

   If you don't know, or forgot, you have the internet to help you remember.
   You should see that the largest is the number n, the better the
   approximation is.
*)

(* There are many ways to address this problem. I stated that n was an
   even integer. It wouldn't change a lot of things for n to be odd,
   and in some approaches it won't change anything at all. Being able
   to stick to the specs is a quality but raising concerns also is
   one. Overall none shouldn't be blocking you. It's best if you stuck
   to n being even and managed to solve the problem, but it's better
   if you changed it to an odd integer if n being even was preventing
   you from providing a solution at all. *)

(* Given that
   - the area of a disc is π * r * r where r is the radius the disc;
   - the area of a square is s * s where s is the side of the square;
   if you draw the largest disc in a square, the surface of the
   former is ( (π / 4) ) times the surface of the latter.
   From that, it's easy to determine an approximation of π.
*)

(* ********************************************************************** *)

(* Here follows a possible implementation.

   First of all, let's implement a function that counts an
   approximation of the surface of a disc (without using any
   approximation of π, of course).
*)

(* ***Always keep in mind: divide large problems into smaller ones.*** *)

(** (approxDiscArea n) returns the area of the disc of diameter (n) *)
fun approxDiscArea n =
    let fun loop i j r =
      (* this local function (called "loop") computes a fourth of the
         area of a disc of diameter (n/2) *)
      if i = n then
        (* last step, so return the result *)
        r
      else if j = n then
        (* end of line (or column, here it doesn't matter), so let's go
           to the next one *)
        loop (i+1) 0 r
      else if i*i + j*j > n * n then
        (* it's not in the disc, let's just treat the next dot *)
        loop i (j+1) r
      else
        (* it's in the disc, let's add it to r and treat the next
           dot *)
        loop i (j+1) (r+1)
    in
    (* call the local function with relevant initial values *)
    loop 0 0 0
 end;;
(* val approxDiscArea = fn: int -> int *)

(* Let's use some tests to see if they highlight obvious mistakes *)
val test0 = approxDiscArea 1;;
(* val test0 = 1: int *)
(* 3.14*(1/2)*(1/2)=0.785 *)
(* difference: +27% *)
val test1 = approxDiscArea 10;;
(* val test1 = 88: int *)
(* 3.14*5*5=78.5 *)
(* difference: +12% *)
val test2 = approxDiscArea 100;;
(* val test2 = 7953: int *)
(* 3.14*50*50=7850 *)
(* difference: +1.3% *)
val test3 = approxDiscArea 1000;;
(* val test3 = 786386: int *)
(* 3.14*500*500=785000 *)
(* difference: +0.18% *)
val test4 = approxDiscArea 10000;;
(* val test4 = 78549762: int *)
(* 3.14*5000*5000=78500000 *)
(* difference: +0.06% *)

(* In approxDiscArea, we use a local function that computes a fourth
   of the area of a disc.  That local function uses the fact that the
   area of a fourth of a disc of diameter X is equal to the area of a
   disc of diameter (X/2).  
   (proof: (d/2)*(d/2)*π = 4 * ((d/2)/2)*((d/2)/2)*π)

   Using this property has 2 advantages. First, it's simpler to
   code. Second, since we're using integers (and not reals), it's more
   accurate (because it does less approximations).
*)

(* Let's now provide a function that provides the area of a square. *)
fun squareArea n =
  n * n;;
(* val squareArea = fn: int -> int *)

(* Now that we can compute the area of a disc and the area of a
   square, we can deduce an approximation of π *)
fun approxPi n =
  (* Don't forget to convert the integers to reals, but do it as late
     as possible, because operations on integers are (supposedly)
     faster.  Moreover, it's very likely that you're dealing with
     exact integers, and that reals are very imprecise. *)
  real (4 * approxDiscArea n) / real (squareArea n);;
(* val approxDiscArea = fn: int -> int *)


val x = 2378927389273892738927389273829738927389273892372893.322;;
val y = x + 1.0;;
val e = Real.compare (x, y);; (* this returns EQUAL *)
(* In actual mathematics, x and y would clearly **not** be equal!
   Better implementations of reals do exist, I'm not sure why
   they aren't used by SML. If you're learning Java, you'll see
   that Java integers cannot represent very large integers
   and that at some point, the sum of two positive integers might be negative...
*)

(* Anyways, as some of you noted, there are more efficient ways to
   compute an approximation of π! *)

(* Let's not forget to test our function! *)
val pi1 = approxPi 1;;
(* val pi1 = 4.0: real *)
(* Not that bad, actually! *)
val pi42 = approxPi 42;;
(* val pi42 = 3.224489796: real *)
(* Already a lot better than 4.0! *)
val pi100 = approxPi 100;;
(* val pi100 = 3.1812: real *)
(* Still getting better! *)
val pi1000 = approxPi 1000;;
(* val pi1000 = 3.145544: real *)
(* 3 correct digits! *)
val pi100000 = approxPi 100000;;
(* val pi100000 = 3.141632545: real *)
(* 4 correct digits! *)
val pi500000 = approxPi 500000;;



(* A few additional remarks:
---------------------------------------------------

   Instead of dealing with quarters of squares and discs, we could
   have dealt with full versions, but it's a little more complex: it
   means that in the (if i*i + j*j > n * n) test, we need to compute
   the distance of (i,j) to the centre of the square or disc
   instead.

   Note that in the quarter of disc version, it's also possible to
   stop counting one by one when we have exited the disc, by
   stopping adding all remaining dots on the line, since we know that all
   following coordinates on the same line will not be in the disc.
   And since there're more dots in the disc than out of it, we could
   count only those outside of it...

   Instead of dealing with 2 counters (i.e., i and j previously), we
   could have dealt with only one (e.g., m), and make it go from 0 to
   (n*n). But in this case, each step of the way we need to deduce
   some (i,j) from (m), which is not that convenient...

   ---------------------------------------------------

   Another way of counting dots is to deal with random dots instead!
   By using a good random integer generator, instead of counting a
   precise set of dots, you can count a randomly computed set of
   dots. It works very well if you have a good random generator. The
   advantage is that you can deal with a very large square and disc
   while keeping the amount of dots pretty low: a larger disc means
   a better approximation because the density of dots on the area
   close to the edge of the disc is diminished.
*)


fun approxDiscArea_opt1 n =
    let fun loop i j r =
      if i = n then
        r
      else if j = n then
        loop (i+1) 0 r
      else if i*i + j*j > n * n then
        loop (i+1) 0 r
      else
        loop i (j+1) (r+1)
    in
    loop 0 0 0
 end;;


fun approxDiscArea_opt2 n =
    let fun loop i j r =
      if i = 0-1 then
        r
      else if j = 0-1 then
        loop (i-1) n r
      else if i*i + j*j < n * n then
        loop (i-1) n (r+j+1)
      else
        loop i (j-1) r
    in
    loop n n 0
 end;;

