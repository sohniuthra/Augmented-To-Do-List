open OUnit2
open Manual

let task1 = init_task "watch lecture" "10/23/20" "10/28/20" 2
let task2 = init_task "fill out OMM" "10/23/20" "10/24/20" 1

(* * [pp_string s] pretty-prints string [s].
   let pp_string s = "\"" ^ s ^ "\""

   (** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
   let pp_list pp_elt lst =
   let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
   in "[" ^ pp_elts lst ^ "]"

   (* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
   let cmp_demo = 
   [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
   ] *)

(********************************************************************
   End helper functions.
 ********************************************************************)

(* let start_room_test name adv expected_output = 
   name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (start_room adv) ~printer:String.escaped) *)

let create_task_test name cat_name task expected_output = 
  name >:: (fun _ -> 
      let expected = create_task cat_name task; access_cat in 
      assert_equal expected_output (expected))

let create_task_tests =
  [
    (* adding two tasks to the same category *)
    create_task_test "adding task1 to General category" "General" task1 [{c_name = "General"; task_list = [{name = "watch lecture"; created_date = "10/23/20"; due_date = "10/28/20"; priority = 2}]}];
    create_task_test "adding task2 to General category" "General" task2 [{c_name = "General"; task_list = [{name = "fill out OMM"; created_date = "10/23/20"; due_date = "10/24/20"; priority = 1}; {name = "watch lecture"; created_date = "10/23/20"; due_date = "10/28/20"; priority = 2}]}];
  ]

let suite =
  "test suite for Manual Mode"  >::: List.flatten [
    create_task_tests;
  ]

let _ = run_test_tt_main suite
