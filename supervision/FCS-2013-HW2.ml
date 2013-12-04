(* A palindrome is a word (here, for the sake of exercising, a list of
   elements) that is the same when it's read in both ways (i.e., from
   left to right or from right to left).

   Example of palindromes:
   A B C B A
   A B C C B A
   1 2 3 3 2 1
   EA EA EA (where EA is considered as a single element or character)

   Since we're using lists to represent arbitrary words, any list with
   a single element is a palindrome, and an empty list is also a
   palindrome.
*)


(* Provide a function named "perms" that takes a list l and returns
   the list of all possible permutations of the elements of l. The type
   of perms should be (''a list -> ''a list list). *)
fun
  perms [] = [[]]
| perms (hd::tl) =
 let
   fun flatten []      = []
   | flatten (e::tl)   = e @ flatten tl
   fun map f []        = []
   | map f (hd::tl)    = (f hd)::(map f tl)
   fun insert e []     = [[e]]
   | insert e (hd::tl) = (e::hd::tl)::(map (fn l => hd::l) (insert e tl))
 in
   flatten (map (fn l => insert hd l) (perms tl))
 end;;

(* 
   Notes:
   - (perms tl) returns the permutations of all elements but the first one
     (yeah, it's the power of recursive functions :)
   - since we have all permutations of tl (and let's call it ptl), 
     we just need to insert hd in every possible position of each list
     of ptl.
     * each list of ptl => use map over ptl
     * insert hd in every possible position => not trivial => delegate
       to another function (which is called insert)
   - map+insert will return a list of lists of lists, and what we
     want is a list of lists, that's why we need to flatten.
   - inserting an element in an empty list means (at least here) 
     to return a single list containing a list with that precise element.
     (we return a single list because there's only ONE way to insert 
     the element)
   - inserting an element every where in a list:
     * the most trivial position is the head (hence (e::hd::tl))
     * all other positions are after the head. Let's just insert e every
       where in tl (using insert, yet again the power of recursion)
       and then let's not forget to add hd in front of all those elements!!
   - we could have used map or List.map which are directly available,
     their types are (('a -> 'b) -> 'a list -> 'b list).
*)

(* In paper 1 of 2013, an implementation was given and students were
   asked to explain it:
fun cons x y = x::y;
fun perms [] = [[]]
  | perms xs =
      let fun perms1 ([],ys) = []
            | perms1 (x::xs,ys) =
                map (cons x) (perms (rev ys @ xs)) @ perms1 (xs,x::ys)
      in perms1 (xs,[]) end;

Personally I prefer my version because it's easier to explain,
and because I don't like the double use of @ and the rev, they
make things more obscure in my opinion.
*)

(* ****************************************************************** *)

(* Provide a function named "pal" that takes a list l and returns true
   if l is a palindrome, and false otherwise.  The type of pal should
   be (''a list -> bool). *)

(* Plenty of possibilities for this one! But the simplest one 
   is to compare the list with its reverse.
*)
fun pal l = l = rev l;;

(* Provide a function named "pp" that takes a list l and returns true
   if elements of "l" can be rearranged to be a palindrome, and false
   otherwise.  The type of pp should be (''a list -> bool). *)

(* We could use perms to implement this one but it would be
   very inefficient in terms of performance.
   One possibility is to count the numbers of occurrences of 
   each element of the word, and if at most one of them is odd,
   then we return true, and false otherwise.
*)

fun pp [] = true
  | pp l =
    let
        fun add e []          = [(e,1)]
        |   add e ((x,i)::tl) = 
          if x = e then
            (x,i+1)::tl
          else
            (x,i)::(add e tl)
        fun at_most_one_odd odd_met [] = true
        | at_most_one_odd true ((_,i)::tl) = 
          if i mod 2 = 0 then 
            at_most_one_odd true tl
          else
            false
        | at_most_one_odd false ((_,i)::tl) = 
          if i mod 2 = 0 then
            at_most_one_odd false tl
          else
            at_most_one_odd true tl
        fun loop occs [] = at_most_one_odd false occs
        |   loop occs (hd::tl) = loop (add hd occs) tl
    in
      loop [] l
 end;
(* The problem with this implementation is that 
   the function add is quite inefficient because in the worst
   case, elements are always added at the end of the word, which means
   a complexity of O(n).
   This could be easily enhanced by using a balanced binary
   tree instead of a list (making the complexity drop to O(log n)).
   If we knew the set of possible elements that can compose a word
   and that set is quite small, we could use a table (possibly
   a hash table) so that add would be of complexity O(1).
*)
          

