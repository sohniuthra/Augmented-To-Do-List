open OUnit2
open Manual
open Automatic

(**  [pp_string s] pretty-prints string [s]. *)
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
(* let cmp_demo = 
   [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]); *)
(* Uncomment this test to see what happens when a test case fails.
   "duplicates not allowed" >:: (fun _ -> 
    assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
      ["foo"; "foo"] ["foo"]);
*)


(********************************************************************
   End helper functions.
 ********************************************************************)

(* let init_todolist_test name cat_name task_name due_date priority 
    expected_output = 
   name >:: (fun _ -> 
      let expected = create_task cat_name task_name due_date 
          priority; (access_cat ()) in 
      assert_equal expected_output (expected)) *)

(* let create_task_test name cat_name task_name due_date priority 
    expected_output = 
   name >:: (fun _ -> 
      let expected = create_task cat_name task_name due_date 
          priority; (access_cat ()) in 
      assert_equal expected_output (expected)) *)

(* let create_task_test name cat_name task_name due_date priority 
    expected_output = 
   let expected = create_task cat_name task_name due_date 
      priority; to_list cat_name in
   name >:: (fun _ -> 
      assert_equal ~printer: (pp_list pp_string) expected_output expected) *)
let update_cat_test name categories cat_name expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (to_list ~cat:categories cat_name) ~printer: (pp_list pp_string)) 

let update_cat_test_auto name categories cat_name expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (to_list_auto ~cat:categories cat_name) ~printer: (pp_list pp_string)) 

(* all tests using update_cat_test also test to_list *)
let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2

(* adding one task into one category *)
let create_task_tests1 =
  [
    update_cat_test "Adding 1 task to the General category" cat "General"
      ["General"; "watch lecture"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1

(* adding two tasks into one category *)
let create_task_tests2 =
  [
    update_cat_test "Adding 2 tasks to the General category" cat "General"
      ["General"; "fill out OMM"; todays_date (); "10/24/20"; "1"; 
       "watch lecture"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3

(* adding three tasks into one category *)
let create_task_tests3 =
  [
    update_cat_test "Adding 3 tasks to the General category" cat "General"
      ["General"; "do lab"; todays_date (); "10/28/20"; "3"; "fill out OMM"; 
       todays_date (); "10/24/20"; "1"; "watch lecture"; todays_date (); 
       "10/28/20"; "2"]; 
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let task4 = create_task ~cat:cat "General" "wash dishes" "10/27/20" 2
let task5 = create_task ~cat:cat "General" "watch basketball game" "10/28/20" 2


(* adding multiple tasks into two categories *)
let create_task_tests4 =
  [
    update_cat_test "Adding 3 tasks to the School category" cat "School"
      ["School"; "do lab"; todays_date (); "10/28/20"; "3"; "fill out OMM"; 
       todays_date (); "10/24/20"; "1"; "watch lecture"; todays_date (); 
       "10/28/20"; "2"];

    update_cat_test "Also adding 2 tasks to the General category" cat "General" 
      ["General"; "watch basketball game"; todays_date (); "10/28/20"; "2"; 
       "wash dishes"; todays_date (); "10/27/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let sort = sort_list ~cat:cat "General"

(* sorting a list with one task *)
let sort_list_tests1 = [
  update_cat_test "Sorting General by priority with one task" cat "General" 
    ["General"; "watch lecture"; todays_date (); "10/28/20"; "2"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let sort = sort_list ~cat:cat "General"

(* sorting a list with three tasks of each different priorities *)
let sort_list_tests2 = [
  update_cat_test "Sorting General by priority with each task containing 
  different prioirities" cat "General" 
    ["General"; "fill out OMM"; todays_date (); "10/24/20"; "1"; 
     "watch lecture"; todays_date (); "10/28/20"; "2"; "do lab"; todays_date (); 
     "10/28/20"; "3"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 2 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 2 
let sort = sort_list ~cat:cat "General"

(* sorting a list with three tasks, all containing the same priorities *)
let sort_list_tests3 = [
  update_cat_test "Sorting General by priority with some tasks containing the 
  same priorities" cat "General" 
    ["General"; "do lab"; todays_date (); "10/28/20"; "2"; "fill out OMM"; 
     todays_date (); "10/24/20"; "2"; "watch lecture"; todays_date (); 
     "10/28/20"; "2"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 2 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let sort = sort_list ~cat:cat "General"

(* sorting a list with multiple tasks, some containing the same priorities *)
let sort_list_tests4 = [
  update_cat_test "Sorting General by priority with some tasks containing the 
  same priorities" cat "General" 
    ["General"; "fill out OMM"; todays_date (); "10/24/20"; "2"; 
     "watch lecture"; todays_date (); "10/28/20"; "2"; "do lab"; todays_date ();
     "10/28/20"; "3";];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let task4 = create_task ~cat:cat "General" "go to discussion" "10/28/20" 2 
let task5 = create_task ~cat:cat "General" "do reading" "10/28/20" 1 
let sort = sort_list ~cat:cat "General"

(* sorting a list with five tasks, some containing the same priorities*)
let sort_list_tests5 = [
  update_cat_test "Sorting General by priority with some tasks containing the 
  same priorities" cat "General" 
    ["General"; "do reading"; todays_date (); "10/28/20"; "1"; "fill out OMM"; 
     todays_date (); "10/24/20"; "1"; "go to discussion"; todays_date (); 
     "10/28/20"; "2"; "watch lecture"; todays_date (); "10/28/20"; "2";"do lab"; 
     todays_date (); "10/28/20"; "3"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let task4 = create_task ~cat:cat "General" "go to discussion" "10/28/20" 2 
let task5 = create_task ~cat:cat "General" "do reading" "10/28/20" 1 
let comp1 = complete_task ~cat:cat "General" "watch lecture"

(* completing 1 task - watch lecture *)
let complete_tests1 =
  [
    (* watch lecture should not be in General *)
    update_cat_test "General should not contain watch lecture" cat "General" 
      ["General"; "do reading"; todays_date (); "10/28/20"; "1"; 
       "go to discussion"; todays_date (); "10/28/20"; "2"; "do lab"; 
       todays_date (); "10/28/20"; "3"; "fill out OMM"; todays_date (); 
       "10/24/20"; "1"];

    (* watch lecture should be in Completed *)
    update_cat_test "Completed should contain watch lecture" cat "Completed"
      ["Completed"; "watch lecture"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let task4 = create_task ~cat:cat "General" "go to discussion" "10/28/20" 2 
let task5 = create_task ~cat:cat "General" "do reading" "10/28/20" 1 
let comp1 = complete_task ~cat:cat "General" "watch lecture"
let comp2 = complete_task ~cat:cat "General" "go to discussion"

(* completing 2 tasks - watch lecture & go to discussion *)
let complete_tests2 =
  [
    (* watch lecture and go to discussion should not be in General *)
    update_cat_test "General should not contain watch lecture or go to 
    discussion" cat "General" 
      ["General"; "do reading"; todays_date (); "10/28/20"; "1"; "do lab"; 
       todays_date (); "10/28/20"; "3"; "fill out OMM"; todays_date (); 
       "10/24/20"; "1"];

    (* watch lecture & go to discussion should be in Completed *)
    update_cat_test "Completed should contain watch lecture and go to 
    discussion" cat "Completed"
      ["Completed"; "go to discussion"; todays_date (); "10/28/20"; "2";
       "watch lecture"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let task4 = create_task ~cat:cat "General" "wash dishes" "10/27/20" 2
let task5 = create_task ~cat:cat "General" "watch basketball game" "10/28/20" 2

let comp1 = complete_task ~cat:cat "School" "fill out OMM"
let comp2 = complete_task ~cat:cat "General" "watch basketball game"

(* completing 2 tasks from different categories, School & General - fill out 
   OMM & watch basketball game*)
let complete_tests3 =
  [
    (* watch basketball game should not be in General *)
    update_cat_test "General should not contain watch basketball game" cat 
      "General" ["General"; "wash dishes"; todays_date (); "10/27/20"; "2"];

    (* fill out OMM should not be in School *)
    update_cat_test "School should not contain fill out OMM" cat "School" 
      ["School"; "do lab"; todays_date (); "10/28/20"; "3"; "watch lecture"; 
       todays_date (); "10/28/20"; "2"];

    (* fill out OMM & watch basketball game should be in Completed *)
    update_cat_test "Completed should contain fill out OMM and watch basketball
    game" cat "Completed"
      ["Completed"; "watch basketball game"; todays_date (); "10/28/20"; "2"; 
       "fill out OMM"; todays_date (); "10/24/20"; "1"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let task4 = create_task ~cat:cat "General" "go to discussion" "10/28/20" 2 
let task5 = create_task ~cat:cat "General" "do reading" "10/28/20" 1 
let del1 = delete_task ~cat:cat "General" "watch lecture"

(* deleting 1 task in 1 category- watch lecture *)
let delete_tests1 =
  [
    (* watch lecture should not be in General *)
    update_cat_test "General should not contain watch lecture" cat "General" 
      ["General"; "do reading"; todays_date (); "10/28/20"; "1"; 
       "go to discussion"; todays_date (); "10/28/20"; "2"; "do lab"; 
       todays_date (); "10/28/20"; "3"; "fill out OMM"; todays_date (); 
       "10/24/20"; "1"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let task4 = create_task ~cat:cat "General" "go to discussion" "10/28/20" 2 
let task5 = create_task ~cat:cat "General" "do reading" "10/28/20" 1 
let del1 = delete_task ~cat:cat "General" "watch lecture"
let del2 = delete_task ~cat:cat "General" "go to discussion"

(* deleting 2 tasks in 1 categor- watch lecture & go to discussion *)
let delete_tests2 =
  [
    (* watch lecture  go to discussion should not be in General *)
    update_cat_test "General should not contain watch lecture or go to 
    discussion" cat "General" 
      ["General"; "do reading"; todays_date (); "10/28/20"; "1"; "do lab"; 
       todays_date (); "10/28/20"; "3"; "fill out OMM"; todays_date (); 
       "10/24/20"; "1"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let task4 = create_task ~cat:cat "General" "wash dishes" "10/27/20" 2
let task5 = create_task ~cat:cat "General" "watch basketball game" "10/28/20" 2

let del1 = delete_task ~cat:cat "School" "fill out OMM"
let del2 = delete_task ~cat:cat "General" "watch basketball game"


(* deleting 2 tasks from different categories, School & General - fill out OMM 
      & watch basketball game*)
let delete_tests3 = 
  [
    (* watch basketball game should not be in General *)
    update_cat_test "General should not contain watch basketball game" cat 
      "General" ["General"; "wash dishes"; todays_date (); "10/27/20"; "2"];

    (* fill out OMM should not be in School *)
    update_cat_test "School should not contain fill out OMM" cat "School" 
      ["School"; "do lab"; todays_date (); "10/28/20"; "3"; "watch lecture"; 
       todays_date (); "10/28/20"; "2"];
  ]

(* let cat = make_school_auto ~cat:cat () *)
let cat = empty_cat_auto ()
let school = make_school_auto ~cat:cat ()
let change = change_priority ~cat:cat "School Tasks" "Write Essay" 4

let change_priority_tests = 
  [
    update_cat_test_auto "Write Essay priority has changed in category School Tasks" cat "School Tasks"
      ["School Tasks"; "Plan for Pre-Enroll"; todays_date (); "TBD"; "7"; 
       "Watch CS 3110 Lecture Videos"; todays_date (); "TBD"; "6"; 
       "Fill out OMM";todays_date (); "TBD"; "5";
       "Meet with Professor"; todays_date (); "TBD"; "4";
       "Finish Biology Lab"; todays_date (); "TBD"; "3";
       "Write Essay"; todays_date (); "TBD"; "4";
       "Complete Math Problem Set"; todays_date (); "TBD"; "1"]
  ]

let suite =
  "test suite for Manual Mode"  >::: List.flatten [
    create_task_tests1;
    create_task_tests2;
    create_task_tests3;
    create_task_tests4;

    sort_list_tests1;
    sort_list_tests2;
    sort_list_tests3;
    sort_list_tests4;
    sort_list_tests5;

    complete_tests1;
    complete_tests2;
    complete_tests3;

    delete_tests1;    
    delete_tests2;
    delete_tests3;

    change_priority_tests;
  ]

let _ = run_test_tt_main suite
