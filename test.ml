open OUnit2
open Manual

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

(* let init_todolist_test name cat_name task_name due_date priority 
    expected_output = 
   name >:: (fun _ -> 
      let expected = create_task cat_name task_name due_date 
          priority; (access_cat ()) in 
      assert_equal expected_output (expected)) *)

let create_task_test name cat_name task_name due_date priority 
    expected_output = 
  name >:: (fun _ -> 
      let expected = create_task cat_name task_name due_date 
          priority; (access_cat ()) in 
      assert_equal expected_output (expected))

let task1 = init_task "watch lecture" "10/28/20" 2
let task2 = init_task "fill out OMM" "10/24/20" 1

let test1_lst = [task1]
let general_lst = [init_todolist "General" test1_lst]

let test2_lst = task2 :: test1_lst
let general2_lst = [init_todolist "General" test2_lst]

let create_task_tests =
  [
    (* adding two tasks to the same category *)
    create_task_test "adding task1 to General category" 
      "General" "watch lecture" "10/28/20" 2 general_lst;

    create_task_test "adding task2 to existing General category" 
      "General" "fill out OMM" "10/24/20" 1 general2_lst;
  ]

let suite =
  "test suite for Manual Mode"  >::: List.flatten [
    create_task_tests;
  ]

let _ = run_test_tt_main suite
