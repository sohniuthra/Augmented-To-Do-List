open Graphics
open Manual
open Printf

let cat = empty_cat ()

(** [draw_str_list slst] takes string list [slst] and draws it with each item 
    on a new line *) 
let rec draw_str_list slst =
  match slst with 
  | [] -> ()
  | h::t -> draw_string h; moveto (10) (current_y () - 15); 
    draw_str_list t

(** [draw_basic ()] is the basic window that should open when the application
    opens *) 
let draw_basic () =
  clear_graph ();
  moveto 10 460;
  set_color blue;
  draw_string "Welcome to your new to-do list!"; 
  moveto 10 440;
  draw_string "Press t to create a new task"; 
  moveto 10 425;
  draw_string "Press c to complete a task"; 
  moveto 10 410;
  draw_string "Press d to delete a task";
  moveto 10 395;
  draw_string "Press v to view a list";
  moveto 10 380;
  draw_string "Press a to make an automatic list";
  set_color black;
  moveto 10 10;
  draw_string "Press q to quit";
  moveto 10 375

(** [string_input str] produces a string from anything that the user types 
    before pressing enter *)
let rec string_input str =
  let e = wait_next_event [Key_pressed] in 
  if e.key <> '\r' 
  then string_input (str ^ (Char.escaped e.key))
  else str

(** [draw_int i] draws int [i] as a string *) 
let draw_int i =
  draw_string (string_of_int i)

let task_input () =
  (*let cat = empty_cat () in*)
  draw_string "Type the name of your category";
  let category = (string_input "") in
  draw_basic ();
  draw_string "Type the name of your task";
  let name = (string_input "") in
  draw_basic ();
  draw_string "Type the due date in an mm/dd/yyyy format";
  let due = (string_input "") in
  draw_basic ();
  draw_string "Type the priority of your task";
  let priority = int_of_string (string_input "") in
  draw_basic ();
  Manual.create_task ~cat:cat category name due priority

(** [complete_task_gui ()] prompts the user to type in the category and name
    of a task they want to complete *) 
let complete_task_gui () =
  (*let cat = empty_cat () in *)
  draw_string "Type the category of the task you want to complete";
  let category = (string_input "") in
  draw_basic ();
  draw_string "Type the name of the task you want to complete";
  let name = (string_input "") in
  Manual.complete_task ~cat:cat category name

(** [delete_task_gui ()] prompts the user to type in the category and name
    of a task they want to delete *) 
let delete_task_gui () =
  (*let cat = empty_cat () in *)
  draw_string "Type the category of the task you want to delete";
  let category = (string_input "") in
  draw_basic ();
  draw_string "Type the name of the task you want to delete";
  let name = (string_input "") in
  Manual.delete_task ~cat:cat category name

let view_category category = 
  (*let cat = empty_cat () in*)
  let lst = Manual.to_list ~cat:cat category in
  draw_str_list lst

let view_all_categories () = failwith "unimplemented"

let draw_list () =
  draw_string "Type the category of the list you want to view. If you want to view all lists, type all";
  let category = (string_input "") in 
  if category = "all" 
  then view_all_categories ()
  else view_category category

let sort_gui () = 
  let e = wait_next_event [Key_pressed] in
  draw_string "To sort by priority, press p. To sort by due date, press d.";
  let priority = if e.key = 'p' 
    then (clear_graph ();
          draw_string "Type the name of the category you want to sort";
          let category = (string_input "") in
          Manual.sort_list category "priority")
    else () in
  let date = if e.key = 'd' 
    then (clear_graph ();
          draw_string "Type the name of the category you want to sort";
          let category = (string_input "") in
          Manual.sort_list category "date")
    else () in
  priority;
  date

let make_auto () =
  draw_string "Type what kind of automatic list you want. The options are car, school, household, shopping, and pandemic.";
  let auto_choice = string_input "" in 
  if auto_choice = "car" then Automatic.make_car_auto ()
  else if auto_choice = "school" then Automatic.make_school_auto ()
  else if auto_choice = "household" then Automatic.make_household_auto ()
  else if auto_choice = "shopping" then Automatic.make_shopping_auto ()
  else if auto_choice = "pandemic" then Automatic.make_pandemic_auto ()
  else draw_string "Input invalid"; draw_basic ()

let rec loop () = 
  let e = wait_next_event [Key_pressed] in

  let new_task = if e.key = 't'
    then task_input ()
    else () in

  let comp_task = if e.key = 'c'
    then complete_task_gui ()
    else () in

  let del_task = if e.key = 'd'
    then delete_task_gui ()
    else () in

  let view = if e.key = 'v'
    then draw_list ()
    else () in

  let sort = if e.key = 's'
    then sort_gui ()
    else () in 

  let auto = if e.key = 'a'
    then make_auto ()
    else () in 

  new_task;
  comp_task;
  del_task;
  view;
  sort;
  auto;

  if e.key <> 'q' then loop () else ()

let open_window = open_graph " 640x480"; set_window_title "To-Do List"

(** [draw_task tsk] takes task [tsk] and draws it *) 
let draw_task (tsk : Manual.task) =
  draw_string "" (*tsk.name*);
  moveto (current_x () + 30) (current_y ()); 
  draw_string "" (*tsk.created_date*);
  moveto (current_x () + 30) (current_y ());
  draw_string "" (*tsk.due_date*);
  moveto (current_x () + 30) (current_y ());
  draw_string "" (*(string_of_int tsk.priority)*)


(** [draw_tsk_list tlst] takes task list [tlst] and draws it with each item on 
    a new line *) 
let rec draw_tsk_list tlst =
  match tlst with 
  | [] -> ()
  | h::t -> draw_task h; moveto (current_x ()) (current_y () - 15); 
    draw_tsk_list t

let () = open_window;
  draw_basic ();
  loop ();
  close_graph ();

