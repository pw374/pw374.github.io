(* 
search : ''a -> (''a * 'b) tree -> 'b
insert : (''a,'b) -> (''a * 'b) tree -> (''a * 'b) tree
remove : ''a -> (''a * 'b) tree -> (''a * 'b) tree
*)

datatype ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

fun height Empty = 0
  | height (Node(_,_,_,_,h)) = h

fun create l x d r =
  let
   val hl = height l
   val hr = height r 
  in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))
  end

exception Error

fun bal l x d r =
  let
   val hl = case l of Empty => 0 | Node(_,_,_,_,h) => h
   val hr = case r of Empty => 0 | Node(_,_,_,_,h) => h
  in
  if hl > hr + 2 then (
    case l of
      Empty => raise Error
    | Node(ll, lv, ld, lr, _) =>
      if height ll >= height lr then
        create ll lv ld (create lr x d r)
      else (
        case lr of
          Empty => raise Error
        | Node(lrl, lrv, lrd, lrr, _) =>
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      )
  ) else if hr > hl + 2 then (
    case r of
      Empty => raise Error
    | Node(rl, rv, rd, rr, _) =>
      if height rr >= height rl then
        create (create l x d rl) rv rd rr
      else (
        case rl of
          Empty => raise Error
        | Node(rll, rlv, rld, rlr, _) =>
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      )
  ) else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))
  end


fun insert (x, data) Empty =
    Node(Empty, x, data, Empty, 1)
 |  insert (x, data) (Node(l, v, d, r, h)) =
    if x = v then
      Node(l, x, data, r, h)
    else if x < v then
      bal (insert (x, data) l) v d r
    else
      bal l v d (insert (x, data) r)

exception Not_found
fun search x Empty =
    raise Not_found
 |  search x (Node(l, v, d, r, _)) =
    if x = v then d
    else search x (if x < v then l else r)


fun remove x Empty = Empty
 |  remove x (Node(l, v, d, r, h)) =
let 
 fun min_binding Empty = raise Not_found
   | min_binding (Node(Empty, x, d, r, _)) = (x, d)
   | min_binding (Node(l, x, d, r, _)) = min_binding l
 fun remove_min_binding Empty = raise Error
   | remove_min_binding (Node(Empty, x, d, r, _)) = r
   | remove_min_binding (Node(l, x, d, r, _)) = bal (remove_min_binding l) x d r
 fun merge Empty t = t
   | merge t Empty = t
   | merge t1 t2 =
      let
        val (x, d) = min_binding t2
      in
      bal t1 x d (remove_min_binding t2)
      end
in
  if x = v then
    merge l r
  else if x < v then
    bal (remove x l) v d r
  else
    bal l v d (remove x r)
end
