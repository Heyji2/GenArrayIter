open Bigarray
open OUnit2
open Stdlib

let array_to_string a =
  let f acc a = Printf.sprintf "%s%d " acc a in
  Array.fold_left f "" a

let test_incr input output dim not_last =
  let incr_ok = GenArrayIter.Iter.incr input dim in
  assert_equal not_last incr_ok ~printer:(fun x ->
      if x then "true" else "false");
  assert_equal input output ~printer:array_to_string

let test_decr index expected_output dim not_last =
  let decr_ok = GenArrayIter.Iter.decr index dim in
  assert_equal not_last decr_ok ~printer:(fun x ->
      if x then "true" else "false");
  assert_equal expected_output index ~printer:array_to_string

let test_incr_last index expected_output dim not_last =
  let incr_ok = GenArrayIter.Iter.incr_last index dim in
  assert_equal not_last incr_ok ~printer:(fun x ->
      if x then "true" else "false");
  assert_equal expected_output index ~printer:array_to_string

let test_decr_last index expected_output dim not_last =
  let decr_ok = GenArrayIter.Iter.decr_last index dim in
  assert_equal not_last decr_ok ~printer:(fun x ->
      if x then "true" else "false");
  assert_equal expected_output index ~printer:array_to_string

let test_mapi_inplace =
  let tab = Genarray.init int32 c_layout [| 4; 2; 3 |] (fun _ -> 0l) in
  let sum_index i _ =
    Int32.of_int (Array.fold_left (fun acc x -> acc + x) 0 i)
  in
  GenArrayIter.Iter.mapi_inplace sum_index tab;
  assert_equal 6l
    (Genarray.get tab [| 3; 1; 2 |])
    ~printer:(Printf.sprintf "%ld")

let test_map_inplace =
  let tab = Genarray.init int32 c_layout [| 4; 2; 3 |] (fun _ -> 0l) in
  GenArrayIter.Iter.map_inplace (fun x -> Int32.add x 2l) tab;
  assert_equal 2l
    (Genarray.get tab [| 1; 1; 1 |])
    ~printer:(Printf.sprintf "%ld")

let test_map =
  let tab = Genarray.init int32 c_layout [| 4; 2; 3 |] (fun _ -> 0l) in
  let mapped_tab = GenArrayIter.Iter.map (fun x -> Int32.add x 2l) int32 tab in
  assert_equal 2l
    (Genarray.get mapped_tab [| 1; 1; 1 |])
    ~printer:(Printf.sprintf "%ld")

let test_mapi =
  let tab = Genarray.init int c_layout [| 4; 2; 3 |] (fun _ -> 7) in
  let sum_index i j = Array.fold_left (fun x acc -> x + acc) j i in
  let mapped_tab = GenArrayIter.Iter.mapi sum_index int tab in
  assert_equal 10
    (Genarray.get mapped_tab [| 1; 1; 1 |])
    ~printer:(Printf.sprintf "%d");
  assert_equal 7
    (Genarray.get mapped_tab [| 0; 0; 0 |])
    ~printer:(Printf.sprintf "%d")

let test_fold_left =
  let tab = Genarray.init int c_layout [| 2; 2; 2 |] (fun _ -> 0) in
  let f i j = i.(0) + (2 * i.(1)) + (4 * i.(2)) in
  let mapped_tab = GenArrayIter.Iter.mapi f int tab in
  (* fills in the map with integers from 0 to 2^3-1*)
  let sum = GenArrayIter.Iter.fold_left (fun x acc -> x + acc) 0 mapped_tab in
  (* sum all intergers from 0 to 7 : 7*8/2 = 28 *)
  assert_equal 28 sum ~printer:(Printf.sprintf "%d")

let test_fold_right =
  let tab = Genarray.init int c_layout [| 2; 2; 2 |] (fun _ -> 0) in
  let f i j = i.(0) + (2 * i.(1)) + (4 * i.(2)) in
  let mapped_tab = GenArrayIter.Iter.mapi f int tab in
  (* fills in the map with integers from 0 to 2^3-1*)
  let sum = GenArrayIter.Iter.fold_right (fun x acc -> x + acc) mapped_tab 0 in
  (* sum all intergers from 0 to 7 : 7*8/2 = 28 *)
  assert_equal 28 sum ~printer:(Printf.sprintf "%d")

let iter_test_suite =
  let dim = [| 4; 2; 3 |] in
  "test suite for iter"
  >::: [
         ( "01 test incr first value" >:: fun _ ->
           test_incr [| 0; 0; 0 |] [| 1; 0; 0 |] dim true );
         ( "02 test incr end of first dim" >:: fun _ ->
           test_incr [| 3; 0; 0 |] [| 0; 1; 0 |] dim true );
         ( "03 test incr last dim" >:: fun _ ->
           test_incr [| 3; 1; 2 |] [| 0; 0; 0 |] dim false );
         ( "04 test incr_last first value" >:: fun _ ->
           test_incr_last [| 0; 0; 0 |] [| 0; 0; 1 |] dim true );
         ( "05 test incr_last end of first dim" >:: fun _ ->
           test_incr_last [| 0; 0; 2 |] [| 0; 1; 0 |] dim true );
         ( "06 test incr_last last dim" >:: fun _ ->
           test_incr_last [| 3; 1; 2 |] [| 0; 0; 0 |] dim false );
         ( "07 test decr first value" >:: fun _ ->
           test_decr [| 3; 1; 2 |] [| 2; 1; 2 |] dim true );
         ( "08 test decr end of first dim" >:: fun _ ->
           test_decr [| 0; 1; 2 |] [| 3; 0; 2 |] dim true );
         ( "09 test decr last dim" >:: fun _ ->
           test_decr [| 0; 0; 0 |] [| 3; 1; 2 |] dim false );
         ( "10 test decr_last first value" >:: fun _ ->
           test_decr_last [| 3; 1; 2 |] [| 3; 1; 1 |] dim true );
         ( "11 test decr_last end of first dim" >:: fun _ ->
           test_decr_last [| 3; 1; 0 |] [| 3; 0; 2 |] dim true );
         ( "12 test decr_last last dim" >:: fun _ ->
           test_decr_last [| 0; 0; 0 |] [| 3; 1; 2 |] dim false );
         ("13 test mapi_inplace" >:: fun _ -> test_mapi_inplace);
         ("14 test map_inplace" >:: fun _ -> test_map_inplace);
         ("15 test map" >:: fun _ -> test_map);
         ("16 test mapi" >:: fun _ -> test_mapi);
         ("17 test fold_left" >:: fun _ -> test_fold_left);
         ("18 test fold_right" >:: fun _ -> test_fold_right);
       ]

let _ = run_test_tt_main iter_test_suite
