open OUnit2
open Manual
open Automatic
open Appointments
(* open Appointments *)

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

let update_app_test name appointments expected_output =
  name >:: (fun _ -> 
      assert_equal expected_output
        (to_list_app ~appo:appointments []) ~printer: (pp_list pp_string))

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
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let task4 = create_task ~cat:cat "General" "wash dishes" "10/27/20" 2
let task5 = create_task ~cat:cat "General" "watch basketball game" "10/28/20" 2

let task6 = create_task ~cat:cat "Careers" "Google Interview" "11/15/20" 3
let task7 = create_task ~cat:cat "Careers" "Microsoft Information Session" 
    "11/22/20" 3

(* adding multiple tasks into three categories *)
let create_task_tests4 =
  [
    update_cat_test "Adding 3 tasks to the School category" cat "School"
      ["School"; "do lab"; todays_date (); "10/28/20"; "3"; "fill out OMM"; 
       todays_date (); "10/24/20"; "1"; "watch lecture"; todays_date (); 
       "10/28/20"; "2"];

    update_cat_test "Also adding 2 tasks to the General category" cat "General" 
      ["General"; "watch basketball game"; todays_date (); "10/28/20"; "2"; 
       "wash dishes"; todays_date (); "10/27/20"; "2"];

    update_cat_test "Also adding 2 tasks to the Careers category" cat "Careers" 
      ["Careers"; "Microsoft Information Session"; todays_date (); "11/22/20"; 
       "3"; "Google Interview"; todays_date (); "11/15/20"; "3"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let sort = sort_list ~cat:cat "General" "Priority"

(* sorting a list by priority with one task *)
let sort_list_priority_tests1 = [
  update_cat_test "Sorting General by priority with one task" cat "General" 
    ["General"; "watch lecture"; todays_date (); "10/28/20"; "2"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 1 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let sort = sort_list ~cat:cat "General" "Priority"

(* sorting a list by priority with three tasks of each different priorities *)
let sort_list_priority_tests2 = [
  update_cat_test "Sorting General by priority with each task containing 
   different priorities" cat "General" 
    ["General"; "fill out OMM"; todays_date (); "10/24/20"; "1"; 
     "watch lecture"; todays_date (); "10/28/20"; "2"; "do lab"; todays_date (); 
     "10/28/20"; "3"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 2 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 2 
let sort = sort_list ~cat:cat "General" "Priority"

(* sorting a list by priority with three tasks, all containing the same 
   priorities *)
let sort_list_priority_tests3 = [
  update_cat_test "Sorting General by priority with all tasks containing the 
   same priorities" cat "General" 
    ["General"; "do lab"; todays_date (); "10/28/20"; "2"; "fill out OMM"; 
     todays_date (); "10/24/20"; "2"; "watch lecture"; todays_date (); 
     "10/28/20"; "2"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/20" 2 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let sort = sort_list ~cat:cat "General" "Priority"

(* sorting a list by priority with multiple tasks, some containing the same 
   priorities *)
let sort_list_priority_tests4 = [
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
let sort = sort_list ~cat:cat "General" "Priority"

(* sorting a list by priority with five tasks, some containing the same 
   priorities*)
let sort_list_priority_tests5 = [
  update_cat_test "Sorting General by priority with some tasks containing the 
   same priorities" cat "General" 
    ["General"; "do reading"; todays_date (); "10/28/20"; "1"; "fill out OMM"; 
     todays_date (); "10/24/20"; "1"; "go to discussion"; todays_date (); 
     "10/28/20"; "2"; "watch lecture"; todays_date (); "10/28/20"; "2";"do lab"; 
     todays_date (); "10/28/20"; "3"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let sort = sort_list ~cat:cat "General" "Due Date"

(* sorting a list by due date with one task *)
let sort_list_date_tests1 = [
  update_cat_test "Sorting General by date with one task" cat "General" 
    ["General"; "watch lecture"; todays_date (); "10/28/20"; "2"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/22" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/24/21" 2 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 2 
let sort = sort_list ~cat:cat "General" "Due Date"

(* sorting a list by due date with three tasks, each with the different years *)
let sort_list_date_tests2 = [
  update_cat_test "Sorting General by due date with each task due different 
   years" cat "General" 
    ["General"; "do lab"; todays_date (); "10/28/20"; "2"; "fill out OMM"; 
     todays_date (); "10/24/21"; "2"; "watch lecture"; todays_date (); 
     "10/28/22"; "2"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "11/24/20" 2 
let task3 = create_task ~cat:cat "General" "do lab" "12/28/20" 2 
let sort = sort_list ~cat:cat "General" "Due Date"

(* sorting a list by due date with three tasks, each with the different months 
   and the arbitrary days and same years *)
let sort_list_date_tests3 = [
  update_cat_test "Sorting General by due date with each task due different 
   months of the same year" cat "General" 
    ["General"; "watch lecture"; todays_date ();  "10/28/20"; "2"; 
     "fill out OMM"; todays_date (); "11/24/20"; "2"; "do lab"; todays_date (); 
     "12/28/20"; "2"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "11/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "11/25/20" 2 
let task3 = create_task ~cat:cat "General" "do lab" "11/29/20" 2 
let sort = sort_list ~cat:cat "General" "Due Date"

(* sorting a list by due date with three tasks, each with the different days but 
   the same months and years *)
let sort_list_date_tests4 = [
  update_cat_test "Sorting General by due date with each task due different days 
   of the same month and same years" cat "General" 
    ["General"; "fill out OMM"; todays_date (); "11/25/20"; "2"; 
     "watch lecture"; todays_date ();  "11/28/20"; "2"; "do lab"; 
     todays_date (); "11/29/20"; "2" ];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/29/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/20/20" 3
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 1 
let task4 = create_task ~cat:cat "General" "go to discussion" "10/29/20" 2 
let task5 = create_task ~cat:cat "General" "do reading" "10/20/20" 1
let sort = sort_list ~cat:cat "General" "Priority"

(* sorting a list by due date with five tasks, some containing the same 
   due dates *)
let sort_list_date_tests5 = [
  update_cat_test "Sorting General by due date with some tasks containing the 
   same due dates" cat "General" 
    ["General"; "do reading"; todays_date (); "10/28/20"; "1"; "fill out OMM"; 
     todays_date (); "10/24/20"; "1"; "go to discussion"; todays_date (); 
     "10/28/20"; "2"; "watch lecture"; todays_date (); "10/28/20"; "2";"do lab"; 
     todays_date (); "10/28/20"; "3"];
]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "General" "watch lecture" "10/28/20" 2 
let task2 = create_task ~cat:cat "General" "fill out OMM" "10/28/20" 1 
let task3 = create_task ~cat:cat "General" "do lab" "10/28/20" 3 
let sort = sort_list ~cat:cat "General" "Due Date"

(* sorting a list by due date with three tasks, all containing the same 
   due date *)
let sort_list_date_tests6 = [
  update_cat_test "Sorting General by priority with all tasks containing the 
   same due date" cat "General" 
    ["General"; "do lab"; todays_date (); "10/28/20"; "2"; "fill out OMM"; 
     todays_date (); "10/28/20"; "2"; "watch lecture"; todays_date (); 
     "10/28/20"; "2"];
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

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let ch1 = change_name ~cat:cat "School" "fill out OMM" "Fill Out OMM"

(* changing the name of one task in one category *)
let change_name_tests1 = 
  [
    update_cat_test "fill out OMM in School should now be Fill Out OMM" cat 
      "School" ["School"; "Fill Out OMM"; todays_date (); "10/24/20"; "1";
                "watch lecture"; todays_date (); "10/28/20"; "2"; 
                "do lab"; todays_date (); "10/28/20"; "3"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let ch1 = change_name ~cat:cat "School" "fill out OMM" "Fill Out OMM"
let ch2 = change_name ~cat:cat "School" "do lab" "Do Lab"

(* changing the names of two tasks from the same category *)
let change_name_tests2 = 
  [
    update_cat_test "fill out OMM in School should now be Fill Out OMM and do 
    lab in School should now be Do Lab" cat 
      "School" ["School";  "Do Lab"; todays_date (); "10/28/20"; "3";
                "Fill Out OMM"; todays_date (); "10/24/20"; "1";
                "watch lecture"; todays_date (); "10/28/20"; "2" ];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let task4 = create_task ~cat:cat "General" "wash dishes" "10/27/20" 2
let task5 = create_task ~cat:cat "General" "watch basketball game" "10/28/20" 2

let ch1 = change_name ~cat:cat "School" "fill out OMM" "Fill Out OMM"
let ch2 = change_name ~cat:cat "General" "wash dishes" "Wash Dishes"

(* changing the names of two different tasks from two different categories *)
let change_name_tests3 = 
  [
    update_cat_test "fill out OMM in School should now be named Fill Out OMM" 
      cat 
      "School" ["School"; "Fill Out OMM"; todays_date (); "10/24/20"; "1";
                "watch lecture"; todays_date (); "10/28/20"; "2"; 
                "do lab"; todays_date (); "10/28/20"; "3"];

    update_cat_test "wash dishes in General should now be named Wash Dishes" 
      cat "General" 
      ["General"; "Wash Dishes"; todays_date (); "10/27/20"; "2"; 
       "watch basketball game"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let ch1 = change_priority ~cat:cat "School" "fill out OMM" 2

(* changing the priority of one task in one category *)
let change_priority_tests1 = 
  [
    update_cat_test "fill out OMM in School should now have priority 2" cat 
      "School" ["School"; "fill out OMM"; todays_date (); "10/24/20"; "2";
                "watch lecture"; todays_date (); "10/28/20"; "2"; 
                "do lab"; todays_date (); "10/28/20"; "3"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let ch1 = change_priority ~cat:cat "School" "fill out OMM" 3
let ch2 = change_priority ~cat:cat "School" "do lab" 1

(* changing the priorities of two tasks from the same category *)
let change_priority_tests2 = 
  [
    update_cat_test "fill out OMM in School should now have priority 3 and do 
    lab in School should now have priority 1" cat 
      "School" ["School"; "do lab"; todays_date (); "10/28/20"; "1";
                "fill out OMM"; todays_date (); "10/24/20"; "3";
                "watch lecture"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let task4 = create_task ~cat:cat "General" "wash dishes" "10/27/20" 2
let task5 = create_task ~cat:cat "General" "watch basketball game" "10/28/20" 2

let ch1 = change_priority ~cat:cat "School" "fill out OMM" 2
let ch2 = change_priority ~cat:cat "General" "wash dishes" 1

(* changing the priorities of two different tasks from two 
   different categories *)
let change_priority_tests3 = 
  [
    update_cat_test "fill out OMM in School should now have priority 2" 
      cat 
      "School" ["School"; "fill out OMM"; todays_date (); "10/24/20"; "2";
                "watch lecture"; todays_date (); "10/28/20"; "2"; 
                "do lab"; todays_date (); "10/28/20"; "3"];

    update_cat_test "wash dishes in General should now have priority 1" 
      cat "General" 
      ["General"; "wash dishes"; todays_date (); "10/27/20"; "1"; 
       "watch basketball game"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let ch1 = change_due_date ~cat:cat "School" "fill out OMM" "10/27/20"

(* changing the due date of one task in one category *)
let change_due_date_tests1 = 
  [
    update_cat_test "fill out OMM in School should now be due 10/27/20" cat 
      "School" ["School";  "fill out OMM"; todays_date (); "10/27/20"; "2";
                "watch lecture"; todays_date (); "10/28/20"; "2"; 
                "do lab"; todays_date (); "10/28/20"; "3"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let ch1 = change_due_date ~cat:cat "School" "fill out OMM" "10/27/20"
let ch2 = change_due_date ~cat:cat "School" "do lab" "10/25/20"

(* changing the due dates of two tasks from the same category *)
let change_due_date_tests2 = 
  [
    update_cat_test "fill out OMM in School should now be due 10/27/20" cat 
      "School" ["School";  "do lab"; todays_date (); "10/25/20"; "1";
                "fill out OMM"; todays_date (); "10/27/20"; "3";
                "watch lecture"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat ()
let task1 = create_task ~cat:cat "School" "watch lecture" "10/28/20" 2
let task2 = create_task ~cat:cat "School" "fill out OMM" "10/24/20" 1
let task3 = create_task ~cat:cat "School" "do lab" "10/28/20" 3

let task4 = create_task ~cat:cat "General" "wash dishes" "10/27/20" 2
let task5 = create_task ~cat:cat "General" "watch basketball game" "10/28/20" 2

let ch1 = change_due_date ~cat:cat "School" "fill out OMM" "10/27/20"
let ch2 = change_due_date ~cat:cat "General" "wash dishes" "10/30/20"

(* changing the due dates of two different tasks from two 
   different categories *)
let change_due_date_tests3 = 
  [
    update_cat_test "fill out OMM in School should now be due 10/27/20" 
      cat 
      "School" ["School"; "fill out OMM"; todays_date (); "10/27/20"; "1";
                "watch lecture"; todays_date (); "10/28/20"; "2"; 
                "do lab"; todays_date (); "10/28/20"; "3"];

    update_cat_test "wash dishes in General should now be due 10/30/20" 
      cat "General" 
      ["General"; "wash dishes"; todays_date (); "10/30/20"; "2"; 
       "watch basketball game"; todays_date (); "10/28/20"; "2"];
  ]

let cat = empty_cat_auto ()
let school = make_school_auto ~cat:cat ()
let change = change_priority_auto ~cat:cat "School Tasks" "Write Essay" 4

let change_auto_priority_tests = 
  [
    update_cat_test_auto "Write Essay priority has changed in category School 
    Tasks" cat "School Tasks"
      ["School Tasks"; "Write Essay"; todays_date (); "TBD"; "4";
       "Plan for Pre-Enroll"; todays_date (); "TBD"; "7"; 
       "Watch CS 3110 Lecture Videos"; todays_date (); "TBD"; "6"; 
       "Fill Out OMM";todays_date (); "TBD"; "5";
       "Meet with Professor"; todays_date (); "TBD"; "4";
       "Finish Biology Lab"; todays_date (); "TBD"; "3";
       "Complete Math Problem Set"; todays_date (); "TBD"; "1"]
  ]

let cat = empty_cat_auto ()
let pan = make_pandemic_auto ~cat:cat ()
let change = change_priority_auto ~cat:cat "Pandemic Tasks" "Make Masks" 5
let change2 = change_priority_auto ~cat:cat "Pandemic Tasks"  
    "Buy Disinfectant Spray" 3

(* changi g priority of 2 tasks from the same category*)
let change_auto_priority_tests2 = [
  update_cat_test_auto "the priority of Make Masks in Pandemic Tasks has changed 
   from 4 to 5, and the priority of Buy Disenfectant Spray has changed from 2
   to 3" cat "Pandemic Tasks" 
    ["Pandemic Tasks"; "Buy Disinfectant Spray"; todays_date (); "TBD"; "3";
     "Make Masks"; todays_date (); "TBD"; "5"; 
     "Cancel Flights"; todays_date (); "TBD"; "7"; 
     "Complete Daily Check"; todays_date (); "TBD"; "6"; 
     "Get Tested"; todays_date (); "TBD"; "5"; 
     "Wipe Down Surfaces"; todays_date (); "TBD"; "3"; 
     "Buy Hand Sanitizer"; todays_date ();  "TBD"; "1"]
]

let cat = empty_cat_auto ()
let shopping = make_shopping_auto ~cat:cat ()
let change = change_due_auto ~cat:cat "Shopping Tasks" "Get New iPhone 12 Pro Max" 
    "11/13/2020"

let change_auto_due_tests = [
  update_cat_test_auto "Change due date of Pooja's iPhone 12 Pro Max Purchase 
   to as soon as possible" cat "Shopping Tasks" 
    ["Shopping Tasks"; "Get New iPhone 12 Pro Max"; todays_date (); 
     "11/13/2020"; "7";
     "Get new lightbulbs to replace current ones"; todays_date (); "TBD"; "6";
     "Buy new Nike Sneakers"; todays_date ();"TBD"; "5"; 
     "Buy Essentials for Ithaca Winter"; todays_date (); "TBD"; "4";
     "Find Dress for Formal";  todays_date ();"TBD"; "3";
     "Buy Cake for Melissa's Birthday"; todays_date ();"TBD";  "2";
     "Order Groceries"; todays_date (); "TBD"; "1"]
]

let cat = empty_cat_auto ()
let car = make_car_auto ~cat:cat ()
let change = change_due_auto ~cat:cat "Car Tasks" "Go to Car Wash" "12/20/2020"
let change2 = change_due_auto ~cat:cat "Car Tasks" "Change Radiator Fluid" 
    "11/30/2020"

let change_auto_due_tests2 = [
  update_cat_test_auto "Change due date of 2 tasks in the same category" cat 
    "Car Tasks"
    ["Car Tasks"; "Change Radiator Fluid"; todays_date ();
     "11/30/2020"; "7"; "Go to Car Wash"; 
     todays_date (); "12/20/2020"; "4"; "Change Battery"; todays_date ();
     "TBD"; "6"; "Change Brake Pads"; todays_date (); "TBD"; "5"; 
     "Go for Emissions Test"; todays_date ();
     "TBD"; "3"; "Change Steering Fluid"; todays_date (); "TBD"; "2"; 
     "Change Oil"; todays_date (); "TBD"; "1"]
]

let cat = empty_cat_auto ()
let school = make_school_auto ~cat:cat ()
let chagnge = change_name_auto ~cat:cat "School Tasks" 
    "Watch CS 3110 Lecture Videos" "Watch CS 2800 Lecture Videos"

let change_auto_name_tests = [
  update_cat_test_auto "Change name of a task in School cateogry" cat
    "School Tasks"
    [
      "School Tasks"; 
      "Watch CS 2800 Lecture Videos"; todays_date (); "TBD"; "6"; 
      "Plan for Pre-Enroll"; todays_date (); "TBD"; "7"; 
      "Fill Out OMM";todays_date (); "TBD"; "5";
      "Meet with Professor"; todays_date (); "TBD"; "4";
      "Finish Biology Lab"; todays_date (); "TBD"; "3";
      "Write Essay"; todays_date (); "TBD"; "2";
      "Complete Math Problem Set"; todays_date (); "TBD"; "1"]
]

let cat = empty_cat_auto ()
let house = make_household_auto ~cat:cat ()
let change = change_name_auto ~cat:cat "Household Tasks" "Cook Dinner" "Pack Lunch"
let change2 = change_due_auto ~cat:cat "Household Tasks" "Do the Laundry" 
    "12/16/2020"

let change_auto_name_and_due_tests = [
  update_cat_test_auto "Change name of a task in Household category and change
   due date of another task" cat "Household Tasks"
    [
      "Household Tasks";
      "Do the Laundry"; todays_date (); "12/16/2020"; "7";
      "Pack Lunch"; todays_date (); "TBD"; "4";
      "Feed the Dog"; todays_date (); "TBD"; "6";
      "Mow the Lawn"; todays_date (); "TBD"; "5";
      "Wash the Dishes"; todays_date (); "TBD"; "3";
      "Vacuum the Rugs"; todays_date (); "TBD"; "2";
      "Mop Kitchen Floor"; todays_date (); "TBD"; "1"]
]

let cat = empty_cat_auto ()
let shopping = make_shopping_auto ~cat:cat ()
let change = create_task_auto ~cat:cat "Shopping Tasks" "Buy New Sweater" 
    "12/25/2020" 3

let add_task_auto_tests = [
  update_cat_test_auto "Add task to Shopping category" cat "Shopping Tasks"
    [
      "Shopping Tasks";
      "Buy New Sweater"; todays_date (); "12/25/2020"; "3";
      "Get New iPhone 12 Pro Max"; todays_date (); "TBD"; "7";
      "Get new lightbulbs to replace current ones"; todays_date (); "TBD"; "6";
      "Buy new Nike Sneakers"; todays_date ();"TBD"; "5"; 
      "Buy Essentials for Ithaca Winter"; todays_date (); "TBD"; "4";
      "Find Dress for Formal";  todays_date ();"TBD"; "3";
      "Buy Cake for Melissa's Birthday"; todays_date ();"TBD";  "2";
      "Order Groceries"; todays_date (); "TBD"; "1"]
]

let cat = empty_cat_auto ()
let pandemic = make_pandemic_auto ~cat:cat ()
let change = delete_task_auto ~cat:cat "Pandemic Tasks" "Cancel Flights"

let delete_task_auto_tests = [
  update_cat_test_auto "Delete task from Pandemic category" cat "Pandemic Tasks"
    [
      "Pandemic Tasks";
      "Complete Daily Check"; todays_date (); "TBD"; "6"; 
      "Get Tested"; todays_date (); "TBD"; "5"; 
      "Make Masks"; todays_date (); "TBD"; "4"; 
      "Wipe Down Surfaces"; todays_date (); "TBD"; "3"; 
      "Buy Disinfectant Spray"; todays_date (); "TBD"; "2";
      "Buy Hand Sanitizer"; todays_date ();  "TBD"; "1"]
]

let cat = empty_cat_auto ()
let household = make_household_auto ~cat:cat ()
let change = change_due_auto ~cat:cat "Household Tasks" "Feed the Dog" 
    "12/18/2020"
let reset = reset_household ~cat:cat ()

let reset_one_cat_tests = [
  update_cat_test_auto "Reset household category after a change" cat 
    "Household Tasks"
    [
      "Household Tasks";
      "Do the Laundry"; todays_date (); "TBD"; "7";
      "Feed the Dog"; todays_date (); "TBD"; "6";
      "Mow the Lawn"; todays_date (); "TBD"; "5";
      "Cook Dinner"; todays_date (); "TBD"; "4";
      "Wash the Dishes"; todays_date (); "TBD"; "3";
      "Vacuum the Rugs"; todays_date (); "TBD"; "2";
      "Mop Kitchen Floor"; todays_date (); "TBD"; "1"
    ]
]

let cat = empty_cat_auto ()
let shopping = make_shopping_auto ~cat:cat ()
let change = change_name_auto ~cat:cat "Shopping Tasks" "Order Groceries" 
    "Order Clothes"
let reset = reset_all_cat ~cat:cat ()

let reset_all_cat_tests = [
  update_cat_test_auto "Making sure car category is reset" cat "Car Tasks"
    [
      "Car Tasks";
      "Change Radiator Fluid"; todays_date (); "TBD"; "7"; 
      "Change Battery"; todays_date (); "TBD"; "6";
      "Change Brake Pads"; todays_date (); "TBD"; "5"; 
      "Go to Car Wash"; todays_date (); "TBD"; "4";
      "Go for Emissions Test"; todays_date (); "TBD"; "3";
      "Change Steering Fluid"; todays_date (); "TBD"; "2"; 
      "Change Oil"; todays_date (); "TBD"; "1"
    ];

  update_cat_test_auto "Making sure school category is reset" cat 
    "School Tasks"
    [
      "School Tasks";
      "Plan for Pre-Enroll"; todays_date (); "TBD"; "7"; 
      "Watch CS 3110 Lecture Videos"; todays_date (); "TBD"; "6"; 
      "Fill Out OMM";todays_date (); "TBD"; "5";
      "Meet with Professor"; todays_date (); "TBD"; "4";
      "Finish Biology Lab"; todays_date (); "TBD"; "3";
      "Write Essay"; todays_date (); "TBD"; "2";
      "Complete Math Problem Set"; todays_date (); "TBD"; "1"
    ];

  update_cat_test_auto "Making sure household category is reset" cat
    "Household Tasks"
    [
      "Household Tasks";
      "Do the Laundry"; todays_date (); "TBD"; "7";
      "Feed the Dog"; todays_date (); "TBD"; "6";
      "Mow the Lawn"; todays_date (); "TBD"; "5";
      "Cook Dinner"; todays_date (); "TBD"; "4";
      "Wash the Dishes"; todays_date (); "TBD"; "3";
      "Vacuum the Rugs"; todays_date (); "TBD"; "2";
      "Mop Kitchen Floor"; todays_date (); "TBD"; "1"
    ];

  update_cat_test_auto "Making sure shopping category is reset" cat
    "Shopping Tasks"
    [
      "Shopping Tasks";
      "Get New iPhone 12 Pro Max"; todays_date (); "TBD"; "7";
      "Get new lightbulbs to replace current ones"; todays_date (); "TBD"; "6";
      "Buy new Nike Sneakers"; todays_date ();"TBD"; "5"; 
      "Buy Essentials for Ithaca Winter"; todays_date (); "TBD"; "4";
      "Find Dress for Formal";  todays_date ();"TBD"; "3";
      "Buy Cake for Melissa's Birthday"; todays_date ();"TBD";  "2";
      "Order Groceries"; todays_date (); "TBD"; "1"
    ];

  update_cat_test_auto "Making sure pandemic category is reset" cat
    "Pandemic Tasks"
    [
      "Pandemic Tasks";
      "Cancel Flights"; todays_date (); "TBD"; "7";
      "Complete Daily Check"; todays_date (); "TBD"; "6"; 
      "Get Tested"; todays_date (); "TBD"; "5"; 
      "Make Masks"; todays_date (); "TBD"; "4"; 
      "Wipe Down Surfaces"; todays_date (); "TBD"; "3"; 
      "Buy Disinfectant Spray"; todays_date (); "TBD"; "2";
      "Buy Hand Sanitizer"; todays_date ();  "TBD"; "1"
    ]
]

let apps = empty_appo ()
let access_app_tests = [
  update_app_test "View empty appointments list" apps []
]

let apps = empty_appo ()
let dentist = add_app ~appo:apps "Go to Dentist" "12/16/20" "2:30 PM"
let practiece = add_app ~appo:apps "Go to Lacrosse Practice" "1/25/21" "4 PM"
let chores = add_app ~appo:apps "Do Chores" "6/12/21" "9 AM"
let fat = add_app ~appo:apps "Eat with Fatty and Poo" "12/25/20" "12 PM"
let cs = add_app ~appo:apps "CS 3110 Final Due" "12/20/20" "11:59 PM"


let add_app_tests = [
  update_app_test "Apps are added to appointment list" apps 
    [
      "CS 3110 Final Due"; "12/20/20"; "11:59 PM"; "Location";
      "Notes about Appointment";
      "Eat with Fatty and Poo"; "12/25/20"; "12 PM"; "Location";
      "Notes about Appointment";
      "Do Chores"; "6/12/21"; "9 AM"; "Location";
      "Notes about Appointment";
      "Go to Lacrosse Practice"; "1/25/21"; "4 PM"; "Location"; 
      "Notes about Appointment";
      "Go to Dentist"; "12/16/20"; "2:30 PM"; "Location"; 
      "Notes about Appointment";]
]

let apps = empty_appo ()
let dentist = add_app ~appo:apps "Go to Dentist" "12/16/20" "2:30 PM"
let practiece = add_app ~appo:apps "Go to Lacrosse Practice" "1/25/21" "4 PM"
let chores = add_app ~appo:apps "Do Chores" "6/12/21" "9 AM"
let fat = add_app ~appo:apps "Eat with Fatty and Poo" "12/25/20" "12 PM"
let cs = add_app ~appo:apps "CS 3110 Final Due" "12/20/20" "11:59 PM"
let delete1 = delete_app ~appo:apps "Go to Dentist"
let delete2 = delete_app ~appo:apps "CS 3110 Final Due"
let delete3 = delete_app ~appo:apps "Do Chores"
let semester = add_app ~appo:apps "End of Semester!!!!" "12/20/20" "All Day"
let christmas = add_app ~appo:apps "Christmas Woo" "12/25/20" "All Day"
let new_years = add_app ~appo:apps "Goodbye 2020!" "1/1/21" "All Day"


let delete_app_tests = [
  update_app_test "Apps are deleted from list" apps 
    [
      "Goodbye 2020!"; "1/1/21"; "All Day"; "Location";
      "Notes about Appointment";
      "Christmas Woo"; "12/25/20"; "All Day"; "Location";
      "Notes about Appointment";
      "End of Semester!!!!"; "12/20/20"; "All Day"; "Location";
      "Notes about Appointment";
      "Eat with Fatty and Poo"; "12/25/20"; "12 PM"; "Location";
      "Notes about Appointment";
      "Go to Lacrosse Practice"; "1/25/21"; "4 PM"; "Location"; 
      "Notes about Appointment";
    ]
]

let apps = empty_appo ()
let dentist = add_app ~appo:apps "Go to Dentist" "12/16/20" "2:30 PM"
let practiece = add_app ~appo:apps "Go to Lacrosse Practice" "1/25/21" "4 PM"
let chores = add_app ~appo:apps "Do Chores" "6/12/21" "9 AM"
let fat = add_app ~appo:apps "Eat with Fatty and Poo" "12/25/20" "12 PM"
let cs = add_app ~appo:apps "CS 3110 Final Due" "12/20/20" "11:59 PM"
let delete1 = delete_app ~appo:apps "Go to Dentist"
let delete2 = delete_app ~appo:apps "CS 3110 Final Due"
let delete3 = delete_app ~appo:apps "Do Chores"
let semester = add_app ~appo:apps "End of Semester!!!!" "12/20/20" "All Day"
let christmas = add_app ~appo:apps "Christmas Woo" "12/25/20" "All Day"
let new_years = add_app ~appo:apps "Goodbye 2020!" "1/1/21" "All Day"

let complete_app_tests = [
  update_app_test "Apps are deleted from list" apps 
    [
      "Goodbye 2020!"; "1/1/21"; "All Day"; "Location";
      "Notes about Appointment";
      "Christmas Woo"; "12/25/20"; "All Day"; "Location";
      "Notes about Appointment";
      "End of Semester!!!!"; "12/20/20"; "All Day"; "Location";
      "Notes about Appointment";
      "Eat with Fatty and Poo"; "12/25/20"; "12 PM"; "Location";
      "Notes about Appointment";
      "Go to Lacrosse Practice"; "1/25/21"; "4 PM"; "Location"; 
      "Notes about Appointment";
    ]
]

let apps = empty_appo ()
let meeting = add_app ~appo:apps "Staff Meeting" "4/1/21" "2 pm"
let report = add_app ~appo:apps "Company report due" "2/14/21" "End of work day"
let lunch = add_app ~appo:apps "Lunch with Client" "1/15/21" "12 PM"
let system = add_app ~appo:apps "Learn new system features"
let database = add_app ~appo:apps "Patch Data Base" "TBD" "TBD"
let meeting2 = add_app_info ~appo:apps "Staff Meeting" "meeting to determine 
third financial quarter goals"
let report2 = add_app_info ~appo:apps "Company report due" "report needs
to be 7-10 pages long"
let lunch2 = add_app_info ~appo:apps "Lunch with Client" "client from 
Goldman Sachs"




let add_app_info_tests = [
  update_app_test "Add info to apps" apps 
    [

    ]
]

let suite =
  "test suite for Manual Mode"  >::: List.flatten [
    create_task_tests1;
    create_task_tests2;
    create_task_tests3;
    create_task_tests4;

    sort_list_priority_tests1;
    sort_list_priority_tests2;
    sort_list_priority_tests3;
    sort_list_priority_tests4; 
    sort_list_priority_tests5;

    sort_list_date_tests1;
    sort_list_date_tests2;
    sort_list_date_tests3;
    sort_list_date_tests4;

    complete_tests1;
    complete_tests2;
    complete_tests3;

    delete_tests1;    
    delete_tests2;
    delete_tests3;

    (*change_name_tests1;
      change_name_tests2;
      change_name_tests3;

      change_priority_tests1;
      change_priority_tests2;
      change_priority_tests3;

      change_due_date_tests1;
      change_due_date_tests2;
      change_due_date_tests3; *)


    change_auto_priority_tests;
    change_auto_priority_tests2;

    change_auto_due_tests;
    change_auto_due_tests2;

    change_auto_name_tests;
    change_auto_name_and_due_tests;

    add_task_auto_tests;
    delete_task_auto_tests;

    reset_one_cat_tests;
    reset_all_cat_tests;

    access_app_tests; 
    add_app_tests;
    complete_app_tests;
  ]

let _ = run_test_tt_main suite

