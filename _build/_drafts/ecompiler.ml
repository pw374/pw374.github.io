(** Extensible map with polymorphic variants with lambda calculus *)
(** (c) Philippe Wang <philippe.wang@cl.cam.ac.uk> *)

(** First let's define a type for lamda terms.
    It's very useful to declare polymorphic variant types
    when we don't want the type inference algorithm to come up
    with gigantic types. *)
type 'a lt = [>
  | `App of 'a lt * 'a lt
  | `Var of string
  | `Lam of string * 'a lt
] as 'a

type 'a action =
  | Wildcard
  | Stop of 'a
  | Continue of 'a

(** This is a map function over the type ['a lt], which has the
    particularity of being extensible. *)
let rec map (f:'a lt -> 'a lt action) = function
  | `App(l, r) as t ->
    begin match f t with
      | Wildcard -> `App(map f l, map f r)
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end
  | `Var _ as t ->
    begin match f t with
      | Wildcard -> t
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end
  | `Lam(x, b) as t ->
    begin match f t with
      | Wildcard -> `Lam(x, map f b)
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end
  | t ->
    begin match f t with
      | Wildcard -> t
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end

(** By using our previously defined function map, it's very
    simple to define a beta-reduction for lambda terms. *)
let lambda_reduce (t:'a lt) =
  map
    (function
      | `App(`Lam(x, b), r) ->
        let substitute variable value lambda_term =
          map
            (function
              | `Var v when v = variable -> Continue value
              | `Lam(x, b) as t when x = variable -> Stop t
              | _ -> Wildcard)
            lambda_term
        in
        Continue(substitute x r b)
      | _ ->
        Wildcard
    ) t

let _ = lambda_reduce (`App(`Lam("x", `Var "x"), `Var "y"))
let _ = lambda_reduce (`App(`Lam("x", `App(`Var "x", `Var "x")), `Var "y"))

(** Extending the previous map to handle [`Num of int] as a member of
    ['a lt] *)
let rec map_n f = function
  | `Num(n:int) as t ->
    begin match f t with
      | Wildcard -> t
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end
  | t ->
    map f t

(** Lambda term that can be applied to a "natural number", it returns
    the successor. *)
let succ : 'a lt =
  `Lam("n",
       `Lam("f",
            `Lam("x", 
                 `App(`Var "f",
                      `App(`App(`Var "n", `Var "f"), `Var "x")))))

(** Lambda term that represents zero. *)
let zero = `Lam("f", `Lam("x", `Var "x"))

(** Converts a positive integer to a lambda term. *)
let lt_of_int : int -> 'a lt = function
  | 0 -> zero
  | n -> `App(succ, `Num(n-1))

(** Beta reduction that handles [`Num of int]. *)
let lambda_reduce_n t =
  map_n
    (function
      | `Num n ->
        Continue(lt_of_int n)
      | _ -> Wildcard
    )
    t

let _ = lambda_reduce_n (`App(`Lam("x", `Var "x"), `Num 42))
let _ = lambda_reduce_n (`App(`Lam("x", `App(`Var "x", `Var "x")), `Num 2))



let rec free_vars ?(env=[]) = function
  | `App(left, right) -> free_vars ~env right @ free_vars ~env left
  | `Var x -> if List.mem x env then [] else [x]
  | `Lam(x, b) -> free_vars ~env:(x :: env) b
  | _ -> []


(** This is a map function over the type ['a lt], which has the
    particularity of being extensible. *)
let rec map (f:'a lt -> 'b) = function
  | `App(l, r) as t ->
    begin match f t with
      | `Dig -> `App(map f l, map f r)
      | ` new_t -> map f new_t
      | Stop new_t -> new_t
    end
  | `Var _ as t ->
    begin match f t with
      | Wildcard -> t
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end
  | `Lam(x, b) as t ->
    begin match f t with
      | Wildcard -> `Lam(x, map f b)
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end
  | t ->
    begin match f t with
      | Wildcard -> t
      | Continue new_t -> map f new_t
      | Stop new_t -> new_t
    end


type ('a, 'b) result =
  | Default
  | Recurse of 'b * 'a
  | Result of 'b

(**)
let rec fold f accu = function
  | `App(l, r) as t ->
    begin match f accu t with
      | Recurse(accu, t) -> fold f accu t
      | Result r -> r
      | Default -> fold f (fold f accu l) r
    end
  | `Var _ as t ->
    begin match f accu t with
      | Recurse(accu, t) -> fold f accu t
      | Result r -> r
      | Default -> accu
    end
  | `Lam(x, b) as t ->
    begin match f accu t with
      | Recurse(accu, t) -> fold f accu t
      | Result r -> r
      | Default -> fold f accu b
    end
  | t ->
    begin match f accu t with
      | Recurse(accu, t) -> fold f accu t
      | Result r -> r
      | Default -> accu
    end


let rec fold_add f accu e =
  fold
    (fun accu -> function
      | `Num _ as t ->
        begin match f accu t with
          | Recurse(accu, t) -> fold f accu t
          | Result r -> r
          | Default -> accu
        end
      | _ -> Default)
    accu
    e

let fold_override f accu e =
  fold
    (fun accu -> function
       | `Lam _ -> Default
       | x -> f accu x)
    e



