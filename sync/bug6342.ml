$ cat mt.ml
module M = struct
  type 'a v = {
    x : int;
    v : 'a;
  } constraint 'a = [> `X ]
end

$ cat mt.mli
module M : sig type 'a v = { x : int; v : 'a; } end

$ ocamlc -c mt.mli

$ ocamlc -c mt.ml
File "mt.ml", line 1:
Error: The implementation mt.ml does not match the interface mt.cmi:
  In module M:
       Modules do not match:
         sig
           type 'a v = 'a M.v = { x : int; v : 'a; } constraint 'a = [> `X ]
         end
       is not included in
         sig type 'a v = { x : int; v : 'a; } end
           In module M:
  Type declarations do not match:
    type 'a v = 'a M.v = { x : int; v : 'a; } constraint 'a = [> `X ]
       is not included in
type 'a v = { x : int; v : 'a; }
       File "mt.ml", line 2, characters 6-68: Actual declaration
       The types for field x are not equal.
