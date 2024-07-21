(*
              License : GPL-3.0-only
              Date : 07/2024
              @author Heyji2
              @version dev
*)

open Bigarray

(**
    The [Iter] module is an extention of the {{: https://ocaml.org/manual/5.2/api/Bigarray.Genarray.html}[BigArray.Genarray]}
    module to complete missing functionalities : iteration, mapping and folding over Genarrays. 
    To make these operations possible, an increment/decrement function is needed to iterate over 
    multi-dimensional indexes of the array. There are many ways to iterate over multi-dimension spaces, 
    but here two versions are provided : [incr] and [incr_last] (with their corresponding [decr] and [decr_last] versions).  

    The module features the following functions : 
    - iter / iteri
    - map / mapi
    - map_inplace / mapi_inplace
    - fold_left / fold_right
 *)




val iter : ('a -> unit) -> ('a, 'b, 'c) Genarray.t -> unit
(**
    [iter f a] applies the function [f] in turn to each element of the array [a]. 
    It is equivalent to [f a.(0); f a.(1); ... ; f a.(length a - 1); ()]
*)

val iteri : (int Array.t -> 'a -> unit) -> ('a, 'b, 'c) Genarray.t -> unit
(**
    Same as [iter] except that the function [f] is applied to the index of the array as first argument, and to the element itself as second argument. 
    [iteri f a] applies the function [f] in turn to each element of the array [a] and the index [i] : [f i a.(i)] for all possibles values of the index i.  

    {b NB}: the index is incremented using the function [Iter.incr]
*)

val map :
  ('a -> 'b) ->
  ('b, 'k2) kind ->
  ('a, 'k1, 'layout) Genarray.t ->
  ('b, 'k2, 'layout) Genarray.t
(**
   [map f k a] applies the map function [f] in turn to each element of the array [a] of type ['a] and outputs another array [b], elements of which have type ['b].
*)

val map_inplace : ('a -> 'a) -> ('a, 'b, 'c) Genarray.t -> unit
(**
    Same as [map] but modifies the array {i inplace} instead of creating a new one.
*)

val mapi :
  (int Array.t -> 'a -> 'b) ->
  ('b, 'k2) kind ->
  ('a, 'k1, 'layout) Genarray.t ->
  ('b, 'k2, 'layout) Genarray.t
(**
    Same as [map] except that the function [f] is applied to the index of the array as the first argument, and to the element itself as second argument.
    [mapi f k a] applies the function [f] in turn to each element of the array [a] and the index [i] : [f i a.(i)] for all possibles values of the index [i]. 
    The output is a new array. 

    {b NB} : the function [incr] is used to iterate over all elements of the array [a]
*)

val mapi_inplace : (int Array.t -> 'a -> 'a) -> ('a, 'b, 'c) Genarray.t -> unit
(**
    A combination of [mapi] and [map_inplace] : [mapi_inplace f a] applies [f] in turn to each index [i] (as first argument) and element of the array [a] (as second argument).

    {b NB}: the function [incr] is used to iterate over all elements of the array [a]
*)

val fold_left :
  ('acc -> 'a -> 'acc) -> 'acc -> ('a, 'b, 'layout) Genarray.t -> 'acc
(**
    [fold_left f init a] computes [f (f (... (f init a.(0)) ... ) a.(n-1))] where [n] is the length of the array [a].

    {b NB} : the function [incr] is used to iterate over all elements of the array [a]
*)

val fold_right :
  ('a -> 'acc -> 'acc) -> ('a, 'b, 'layout) Genarray.t -> 'acc -> 'acc
(**
    [fold_right f a init] computes [f a.(0) (f a.(1) (... (f a.(n-1) init)))] where [n] is the length of the array [a].

    {b NB} : the function [decr] is used to iterate over all elements of the array [a]
*)


val incr : int array -> int array -> bool
(**
    Given an index {m i(i_0,i_1, ..., i_{n-1})} of dimension {m d(d_0, d_1, ..., d_{n-1})} such that 
    {m \forall k \in [0;n-1]} with {m 0 \leqslant i_k < d_k}, the function increments the index {m i} according to the following recursive algorithm: 

    - if {m i_0 < n_0 - 1} then {m i_0 = i_0 + 1}   
    - else {m i_0 = 0} and {m i_1} is incremented by one according the same alogrithm as {m i_0}.  
    
    So for an index {m i} of dimensions {m d(2,2,3)} in the space {m \{0,1\}\times\{0,1\}\times\{0,1,2\}}, applying [incr] to {m i=(0,0,0)} several time will 
    give : 
    - {m i=(0,0,0)}
    - {m i=(1,0,0)}
    - {m i=(0,1,0)}
    - {m i=(1,1,0)}
    - {m i=(0,0,1)}
    - {m i=(1,0,1)}
    - {m i=(0,1,1)}
    - {m i=(1,1,1)}
    - {m i=(0,0,2)}
    - {m i=(1,0,2)}
    - {m i=(0,1,2)}
    - {m i=(1,1,2)}
    @param 1 [index] : an array of size n corresponding to an index of an n dimensions Genarray
    @param 2 [dims] : an array of size n giving all the dimensions of the first argument : 
    - [index.(0)] goes from [0] to [dims.(0)]  
    - [index.(1)] goes from [0] to [dims.(1)]
    @return True if the increment succeded. False otherwise, meaning the index has reached the end of the array.
*)

val incr_last : int array -> int array -> bool
(**
    Same as [incr] but instead of incrementing the first dimension ({m i_0}) of the index {m i(i_0, ..., i_{n-1})}, increments the last dimension first ({m i_{n-1}})
*)

val decr : int array -> int array -> bool
(**
    Same as [incr] but in decrementing instead of incrementing.
*)

val decr_last : int array -> int array -> bool
(**
    Same as [decr] but instead of decrementing the first dimention ({m i_0}) of the index {m i(i_0, ..., i_{n-1})}, decrements the last dimension first ({m i_{n-1}})
*)