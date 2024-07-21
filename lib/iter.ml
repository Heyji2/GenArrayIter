open Bigarray

(**
    Given an index {math i(i_0,i_1, ..., i_n-1)} of dimension {math d(d_0, d_1, ..., d_n-1)} such that 
    {math \forall k \epsilon [0;n-1] 0 <= i_k < d_k}, the function increment the index [i] according to the following recursive algorithm: 

    if {math i_0 < n_0 - 1} then {math i_0 = i_0 + 1} else {math i_0=0} and i_1 is incremented by one according the same alogrithm. 

    @param i an array of size n corresponding to an index of an n dimensions Genarray
    @param dims an array of size n giving all the dimensions of the index i : 
    i.(0) goes from 0 to dims.(0)
    i.(1) goes from 0 to dims.(1)
    etc.
    *)
let incr i dims =
  let l = Array.length i in
  let rec incr_jth k dims j n =
    if k.(j) + 1 < dims.(j) then (
      k.(j) <- k.(j) + 1;
      true)
    else (
      k.(j) <- 0;
      if j + 1 < n then incr_jth k dims (j + 1) n else false)
  in
  incr_jth i dims 0 l

(**
    Increment the index i by one. 
    @param i an array of size n corresponding to an index of an n dimensions Genarray
    @param dims an array of size n giving all the dimensions of the index i : 
    i.(0) goes from 0 to dims.(0)
    i.(1) goes from 0 to dims.(1)
    etc...
*)
let incr_last i dims =
  let l = Array.length i in
  let rec incr_jth i dims j =
    if i.(j) + 1 < dims.(j) then (
      i.(j) <- i.(j) + 1;
      true)
    else (
      i.(j) <- 0;
      if j > 0 then incr_jth i dims (j - 1) else false)
  in
  incr_jth i dims (l - 1)

(**
      Same as [incr] but decrementing instead
*)
let decr i dims =
  let rec decr_jth k dims j =
    if k.(j) > 0 then (
      k.(j) <- k.(j) - 1;
      true)
    else (
      k.(j) <- dims.(j) - 1;
      if j + 1 < Array.length i then decr_jth k dims (j + 1) else false)
  in
  decr_jth i dims 0

let decr_last i dims =
  let l = Array.length i in
  let rec decr_jth k dims j =
    if k.(j) > 0 then (
      k.(j) <- k.(j) - 1;
      true)
    else (
      k.(j) <- dims.(j) - 1;
      if j > 0 then decr_jth k dims (j - 1) else false)
  in
  decr_jth i dims (l - 1)

(**
    `iter f a` applies the function f in turn to all elements of `a`. It is equivalent to : 
    `f a.(0); f a.(1); ... ; f a.(length a - 1);()`.
    @param 1 The function to apply to each of the elements of the array, in place. Its type is 'a -> unit 
    @param 2 The Genarray to which the function f is to be applied : has type ('a, 'c, 'd) Genarray.t 
    *)
let iter f a =
  let n = Genarray.num_dims a in
  let dims = Genarray.dims a in
  let index = Array.make n 0 in
  let rec iter_ith f a i =
    f (Genarray.get a i);
    if incr i dims then iter_ith f a i
  in
  iter_ith f a index

(**
    Same as `iter`, but the function is applied to the index of the elemnt as first argument, and the element itself as second argument.
    @param 1 The function to apply to each of the elements of the array, in place. Its type is int Array.t -> 'a -> unit 
    @param 2 The Genarray to which the function f is to be applied : has type ('a, 'c, 'd) Genarray.t 
    *)
let iteri f a =
  let n = Genarray.num_dims a in
  let dims = Genarray.dims a in
  let index = Array.make n 0 in
  let rec iter_ith f a i =
    f i (Genarray.get a i);
    if incr i dims then iter_ith f a i
  in
  iter_ith f a index

(**
    `map f a` applies the function f in turn to all elements of `a` and builds an array with the results returned by f: 
    `[|a.(0); f a.(1); ... ; f a.(length a - 1)|]`
    @param 1 The function to apply to each of the elements of the array, in place. Its type is 'a -> 'b 
    @param 2 The Genarray to which the function f is to be applied : has type ('a, 'c, 'd) Genarray.t 
    @return The newly created array of type : ('b, 'c, 'd) Genarray.t
    *)
let map f new_kind a =
  let dims = Genarray.dims a in
  let map_a2b i = f (Genarray.get a i) in
  Genarray.init new_kind (Genarray.layout a) dims map_a2b

(**
    Applies the function f to all elements of the Genarray a
    @param 1 The function to apply to each of the elements of the array, in place. Its type is 'a -> 'a 
    @param 2 The Genarray to which the function f is to be applied : has type 'a Genarray.t 
    *)
let map_inplace f a =
  let n = Genarray.num_dims a in
  let dims = Genarray.dims a in
  let index = Array.make n 0 in
  let rec iter_ith f a i =
    Genarray.set a i (f (Genarray.get a i));
    if incr i dims then iter_ith f a i
  in
  iter_ith f a index

(**
    Same as `map` but the function is applied to the index of the element as first argument, and the element itself as second argument.  
    @param 1 The function to apply to each of the elements of the array, in place. Its type is: `int Array.t -> 'a -> 'b`
    @param 2 The Genarray to which the function f is to be applied. Its type is :  `('a, 'c, 'd) Genarray.t`
    @return The newly created array of type : `('b, 'c, 'd) Genarray.t`
    *)
let mapi f new_kind a =
  let dims = Genarray.dims a in
  let map_a2b i = f i (Genarray.get a i) in
  Genarray.init new_kind (Genarray.layout a) dims map_a2b

let mapi_inplace f a =
  let n = Genarray.num_dims a in
  let dims = Genarray.dims a in
  let index = Array.make n 0 in
  let rec iter_ith f a i =
    Genarray.set a i (f i (Genarray.get a i));
    if incr i dims then iter_ith f a i
  in
  iter_ith f a index

let fold_left f init a =
  let n = Genarray.num_dims a in
  let dims = Genarray.dims a in
  let index = Array.make n 0 in
  let rec accumulate f init a =
    let acc = f init (Genarray.get a index) in
    match incr index dims with true -> accumulate f acc a | false -> acc
  in
  accumulate f init a

let fold_right f a init =
  let n = Genarray.num_dims a in
  let dims = Genarray.dims a in
  let index = Array.init n (fun i -> dims.(i) - 1) in
  let rec accumulate f a init =
    let acc = f (Genarray.get a index) init in
    match decr index dims with true -> accumulate f a acc | false -> acc
  in
  accumulate f a init
