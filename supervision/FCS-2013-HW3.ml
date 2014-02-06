(* A big integer can be represented using a list of smaller integers.

   The integer (a + b * 10 + c * 100 + d * 1000) can be represented with
   the list [a,b,c,d] where a, b, c and d are digits between 0 and 9
   inclusive.  then [1,3,5,1] means 1531 in base 10.

   The integer (a + b * 16 + c * 16*16 + d * 16*16*16) can be
   represented with the list [a,b,c,d] where a, b, c and d are digits
   between 0 and F inclusive (i.e. one of 0, 1, 2, 3, 4, 5, 6, 7, 8,
   9, A, B, C, D, F).  then [5,F,D,A] means ADF5 in base 16.

   The integer (a + b * 2 + c * 4 + d * 8) can be represented with the
   list [a,b,c,d] where a, b, c and d are digits between 0 and 1
   inclusive.  then [0,1,1,1] means 1110 in base 2.
*)

(* Question 1. Implement a function that takes 2 lists representing 2
   numbers of base 5 and returns its product.  You may use an
   auxiliary function that takes 2 numbers (included in {0, 1, 2, 3,
   4}) and returns its product and its carry.

   For instance, (product [2] [4]) should return [3,1] (not [8] since
   8 is not included in {0, 1, 2, 3, 4}).

   product [a,b] [c,d] should return the sum of [ (a*c) mod 5, (((a *
   c ) / 5) + a*d) mod 5, (((a * c ) / 5) + a*d) / 5 ] [ 0, (b*c) mod
   5, (((b * c ) / 5) + b*d) mod 5, (((b * c ) / 5) + b*d) / 5 ]

   (It's up to you to see if it's relevant to implement a sum
   function.) *)

fun product a b =
    let
        fun p 0 n [] = []
        |   p c n [] = [c]
        |   p c n (e::tl) =
          let
            val nc = (n * e + c) div 5
            val  x = (n * e + c) mod 5
          in
            x::p nc n tl
          end
        fun add c [] [] = [c]
        |   add 0 [] l  = l
        |   add c (e::tl) [] = ((c+e) mod 5)::add ((c+e) div 5) tl []
        |   add c [] l  = add c l []
        |   add c (e1::tl1) (e2::tl2) =
            ((e1+e2+c) mod 5)::add ((e1+e2+c) div 5) tl1 tl2
        fun prod acc z [] x = acc
        |   prod acc z (e::tl) x =
            prod (add 0 acc (z@p 0 e x)) (0::z) tl x
    in
       case
         prod [] [] a b
       of
         [] => [0]
        | x => x
    end;;

product [1,2,3] [2];;
product [3,1] [4,1];;
product [2] [4];;
product [0] [0];;

(* Question 2. What's the complexity in time ? In space ? *)
(*
Complexity for the sub-function [add]: O(max(N, M)) where N and M are the lengths
of the numbers, so it's linear complexity in time and in space (the number
of memory allocations is roughly a factor of the number of other operations).

Complexity for the sub-function [p]: it's a function that multiplies a one-digit
number by a multiple-digit number, the complexity in both space and time is 
linear to the length of the multiple-digit number.

Complexity for the sub-function [prod]: it multiplies 2 multidigit numbers by using
the 2 previously mentioned sub-functions.
[prod] calls
- [prod] N times where N is the number of digits for the first number.
- [add] N times (same number as for [prod])
- [p] N times (same number as for [prod])

Since [prod] calls itself N times, and each time it calls [add] and [p] N times,
that gives a complexity of O(N*N) in time. And in space as well: [prod] itself
doesn't allocate much (note that (0::z) is negligible since it's one operation per
[prod] call, and (z@p 0 e x) is always allocating less than the call to [add]).

*)

(* Question 3. Do you see some possible optimisations?  (you don't
   have to implement them, just give the intuition and the possible
   impact on the complexity, if any)
*)
(*
Many ways to reduce the number of operations: 
- change the algorithm, use a more efficient one, e.g., 
  http://en.wikipedia.org/wiki/Karatsuba_algorithm
  but there are many others, c.f.
  http://en.wikipedia.org/wiki/Multiplication_algorithm
- without changing the algorithm, it's possible to dramatically reduce the 
  number of allocations, for instance by combining some operations made by
  the subfunctions (but then the code might become harder to read).
  It's also possible to convert the list of digits to an array of digits
  and use the mutable property so that all addition results are directy assigned
  to a pre-existing array cell instead of allocating list cells.
*)
