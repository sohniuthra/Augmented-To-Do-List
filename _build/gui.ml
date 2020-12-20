open Graphics
open Manual
open Automatic
open Appointments

let cat = empty_cat ()
let auto_cat = empty_cat_auto ()
let apps = Appointments.empty_appo ()

let viewed_cat = ref ""
let is_todo = ref true 

(** [sep_tasks lst tsklst currtsk] takes list [lst] and separates it into tasks
    in [tsklst] *)
let rec sep_tasks (lst : 'a list) (tsklst : 'a list list) (currtsk : 'a list) = 
  match lst with 
  | [] -> (currtsk :: tsklst)
  | h::t -> if List.length currtsk = 4 
    then sep_tasks t (currtsk :: tsklst) [h] 
    else sep_tasks t tsklst (currtsk @ [h]) 

(** [sep_tasks_w_cat lst tsklst currtsk] takes list [lst] and separates it into 
    tasks in [tsklst]. [lst] is a list of tasks with the category before each 
    task *)
let rec sep_tasks_w_cat (lst : 'a list) (tsklst : 'a list list) currtsk = 
  match lst with 
  | [] -> (currtsk :: tsklst)
  | h::t -> if List.length currtsk = 5 
    then sep_tasks_w_cat t (currtsk :: tsklst) [h] 
    else sep_tasks_w_cat t tsklst (currtsk @ [h]) 

(** [add_cat tll cat acc] adds [cat] to the beginning of every element of 
    [tll] *)
let rec add_cat tll cat acc = 
  match tll with 
  | [] -> acc 
  | h::t -> add_cat t cat (acc @ cat @ h)

(** [make_tll tlst cat] takes the category list [tlst] with category [cat] and
    creates a list list where each element is a task with the category *)
let rec make_tll tlst cat = 
  let not_cat = 
    match tlst with 
    | [] -> []
    | h::t -> t in 
  let separated = sep_tasks not_cat [] [] in 
  let wcat = add_cat separated [cat] [] in 
  sep_tasks_w_cat wcat [] []

(** [draw_task t] draws [t] *)
let rec draw_task t =
  if current_y () > 290 then moveto 10 290; 
  let y = current_y () in  
  match t with 
  | [] -> ()
  | c::t::dd::dc::p::[] -> moveto 10 y;
    draw_string c;
    moveto 150 y;
    draw_string t;
    moveto 300 y;
    draw_string dd;
    moveto 425 y;
    draw_string dc;
    moveto 550 y; 
    draw_string p;
  | _ -> ()

(** [draw_task_list tlst] draws each element of [tlst] on a new line *)
let rec draw_task_list tlst = 
  set_color black;
  match tlst with 
  | [] -> ()
  | h::t -> draw_task h; moveto 10 (current_y () - 15); draw_task_list t

(** [draw_basic ()] is the basic window that opens when the application opens. 
    It is the interface for the to-do list *) 
let draw_basic () =
  clear_graph (); set_color blue; fill_rect 99 459 60 15; moveto 100 460;
  set_color white; draw_string "To-Do List"; set_color red; 
  fill_rect 299 459 72 15; moveto 300 460; set_color black; 
  draw_string "Appointments"; set_color blue; 
  moveto 10 440; draw_string "Press t to create a new task";  
  moveto 10 425; draw_string "Press c to complete a task"; 
  moveto 10 410; draw_string "Press d to delete a task";
  moveto 10 395; draw_string "Press v to view a list";
  moveto 10 380; draw_string "Press a to make an automatic list"; 
  moveto 10 365; draw_string "Press s to sort your to-do list";
  moveto 10 350; draw_string "Press r to reset an automatic to-do list";
  moveto 10 335; 
  draw_string "Click on the priority, name, or due date of a task to change it";
  moveto 520 460; set_color red; draw_string "Press q to quit";
  set_color black; moveto 10 305; draw_string "Category";
  moveto 150 305; draw_string "Task";
  moveto 300 305; draw_string "Date created";
  moveto 425 305; draw_string "Due date";
  moveto 550 305;  draw_string "Priority";
  is_todo := true; moveto 10 320

let view_category category = 
  draw_basic ();
  let lst = Manual.to_list ~cat:cat category in
  let ll = make_tll lst category in 
  draw_task_list ll;
  viewed_cat := category

let draw_appointments () = 
  clear_graph (); set_color red; fill_rect 99 459 60 15; moveto 100 460;
  set_color black; draw_string "To-Do List"; set_color blue; 
  fill_rect 299 459 72 15; moveto 300 460; set_color white;
  draw_string "Appointments - NOT DONE YET"; set_color blue;
  moveto 10 440; draw_string "Press n to create a new appointment"; 
  moveto 10 425; draw_string "Press c to complete an appointment"; 
  moveto 10 410; draw_string "Press d to delete an appointment";
  moveto 10 395;
  draw_string "Click on the information or location field of an appointment to \
               change it";
  moveto 520 460; set_color red; draw_string "Press q to quit";
  set_color black; moveto 10 365; draw_string "Name";
  moveto 150 365; draw_string "Date";
  moveto 225 365; draw_string "Time";
  moveto 300 365; draw_string "Location";
  moveto 425 365; draw_string "Notes";
  is_todo := false;
  moveto 10 380

let rec string_input str =
  let e = wait_next_event [Key_pressed] in 
  if e.key <> '\r' 
  then string_input (str ^ (Char.escaped e.key))
  else str

let task_input () =
  draw_basic (); set_color red; draw_string "Type the name of your category";
  let category = (string_input "") in
  draw_basic (); set_color red; draw_string "Type the name of your task";
  let name = (string_input "") in draw_basic (); set_color red; 
  draw_string "Type the due date in an mm/dd/yyyy format";
  let due = (string_input "") in
  draw_basic (); set_color red; draw_string "Type the priority of your task";
  let priority = int_of_string (string_input "") in draw_basic ();
  if category = "Car Tasks" || category = "School Tasks" || 
     category = "Household Tasks" || category = "Shopping Tasks" || 
     category = "Pandemic Tasks" 
  then (Automatic.create_task_auto ~cat:auto_cat category name due priority;
        let cat_lst_form = Automatic.to_list_auto ~cat:auto_cat category in 
        let cat_lst_lst = make_tll cat_lst_form category in
        draw_task_list cat_lst_lst)
  else (Manual.create_task ~cat:cat category name due priority;
        set_color black;
        let cat_lst_form = Manual.to_list ~cat:cat category in 
        let cat_lst_lst = make_tll cat_lst_form category in
        draw_task_list cat_lst_lst); viewed_cat := category

let complete_task_gui () =
  draw_basic (); set_color red;
  draw_string "Type the category of the task you want to complete";
  let category = (string_input "") in
  draw_basic (); set_color red;
  let cat_lst_form = ref [] in
  draw_string "Type the name of the task you want to completed";
  let name = (string_input "") in
  draw_basic ();
  if category = "Car Tasks" || category = "School Tasks" || 
     category = "Household Tasks" || category = "Shopping Tasks" || 
     category = "Pandemic Tasks"
  then (Automatic.complete_task_auto ~cat:auto_cat category name;
        cat_lst_form := Automatic.to_list_auto ~cat:auto_cat "Completed Tasks";
        let cat_lst_lst = make_tll !cat_lst_form "Completed Tasks" in 
        draw_task_list cat_lst_lst;
        viewed_cat := "Completed Tasks")
  else (Manual.complete_task ~cat:cat category name;
        cat_lst_form := Manual.to_list ~cat:cat "Completed";
        let cat_lst_lst = make_tll !cat_lst_form "Completed" in 
        draw_task_list cat_lst_lst;
        viewed_cat := "Completed")

let delete_task_gui () =
  draw_basic (); set_color red;
  draw_string "Type the category of the task you want to delete";
  let category = (string_input "") in
  draw_basic (); set_color red;
  let cat_lst_form = ref [] in
  draw_string "Type the name of the task you want to delete";
  let name = (string_input "") in
  draw_basic ();
  if category = "Car Tasks" || category = "School Tasks" || 
     category = "Household Tasks" || category = "Shopping Tasks" || 
     category = "Pandemic Tasks"
  then (Automatic.delete_task_auto ~cat:auto_cat category name;
        cat_lst_form := Automatic.to_list_auto ~cat:auto_cat category)
  else (Manual.delete_task ~cat:cat category name;
        cat_lst_form := Manual.to_list ~cat:cat category);
  let cat_lst_lst = make_tll !cat_lst_form category in 
  draw_task_list cat_lst_lst;
  viewed_cat := category

let draw_list () =
  draw_basic ();
  set_color red;
  let cat_lst_form = ref [] in 
  draw_string "Type the category of the list you want to view.";
  let category = (string_input "") in 
  if category = "Car Tasks" || category = "School Tasks" || 
     category = "Household Tasks" || category = "Shopping Tasks" || 
     category = "Pandemic Tasks"
  then (cat_lst_form := Automatic.to_list_auto ~cat:auto_cat category)
  else (cat_lst_form := Manual.to_list ~cat:cat category);
  let cat_lst_lst = make_tll !cat_lst_form category in
  draw_basic ();
  draw_task_list cat_lst_lst;
  viewed_cat := category

(** [priority_sort () is a helper function for [sort_gui ()]] *)
let priority_sort () = 
  draw_basic (); set_color red;
  draw_string "Type the name of the category you want to sort";
  let category = (string_input "") in draw_basic ();
  Manual.sort_list ~cat:cat category "Priority";
  let cat_lst_form = Manual.to_list ~cat:cat category in 
  let cat_lst_lst = make_tll cat_lst_form category in
  draw_task_list cat_lst_lst

(** [date_sort () is a helper function for [sort_gui ()]] *)
let date_sort () =
  draw_basic (); set_color red;
  draw_string "Type the name of the category you want to sort";
  let category = (string_input "") in draw_basic ();
  Manual.sort_list ~cat:cat category "Due Date";
  let cat_lst_form = Manual.to_list ~cat:cat category in 
  let cat_lst_lst = make_tll cat_lst_form category in
  draw_task_list cat_lst_lst

let sort_gui () = 
  moveto 10 320; set_color red;
  draw_string "To sort by priority, press p. To sort by due date, press d.";
  let e = wait_next_event [Key_pressed] in
  match e.key with
  | 'p' -> priority_sort ()
  | 'd' -> date_sort ()
  | _ -> ()

(** [make_auto_car ()] is a helpter function for [make_auto ()] *)
let make_auto_car () = 
  make_car_auto ~cat:auto_cat (); 
  let car_list = to_list_auto ~cat:auto_cat "Car Tasks" in 
  let car_ll = make_tll car_list "Car Tasks" in 
  draw_basic ();
  draw_task_list car_ll;
  viewed_cat := "Car Tasks"

(** [make_auto_school ()] is a helpter function for [make_auto ()] *)
let make_auto_school () =
  make_school_auto ~cat:auto_cat (); 
  let school_list = to_list_auto ~cat:auto_cat "School Tasks" in 
  let school_ll = make_tll school_list "School Tasks" in 
  draw_basic ();
  draw_task_list school_ll;
  viewed_cat := "School Tasks"

(** [make_auto_household ()] is a helpter function for [make_auto ()] *)
let make_auto_household () = 
  make_household_auto ~cat:auto_cat (); 
  let house_list = to_list_auto ~cat:auto_cat "Household Tasks" in 
  let house_ll = make_tll house_list "Household Tasks" in 
  draw_basic ();
  draw_task_list house_ll;
  viewed_cat := "Household Tasks"

(** [make_auto_shopping ()] is a helpter function for [make_auto ()] *)
let make_auto_shopping () =
  make_shopping_auto ~cat:auto_cat (); 
  let shopping_list = to_list_auto ~cat:auto_cat "Shopping Tasks" in 
  let shopping_ll = make_tll shopping_list "Shopping Tasks" in 
  draw_basic ();
  draw_task_list shopping_ll;
  viewed_cat := "Shopping Tasks"

(** [make_auto_pandemic ()] is a helpter function for [make_auto ()] *)
let make_auto_pandemic () =
  make_pandemic_auto ~cat:auto_cat (); 
  let pandemic_list = to_list_auto ~cat:auto_cat "Pandemic Tasks" in 
  let pandemic_ll = make_tll pandemic_list "Pandemic Tasks" in 
  draw_basic ();
  draw_task_list pandemic_ll;
  viewed_cat := "Pandemic Tasks"

(** [make_auto_all ()] is a helpter function for [make_auto ()] *)
let make_auto_all () = 
  make_auto ~cat:auto_cat (); 
  let all_list = to_list_auto ~cat:auto_cat 
      "All Tasks" in 
  let all_ll = make_tll all_list "All Tasks" 
  in 
  draw_basic ();
  draw_task_list all_ll;
  viewed_cat := "All Tasks"

let make_auto () =
  set_color red;
  moveto 10 320;
  draw_string "Type what kind of automatic list you want: car, school, \
               household, shopping, pandemic";
  let auto_choice = string_input "" in 
  match auto_choice with
  | "car" -> make_auto_car ()
  | "school" -> make_auto_school ()
  | "household" -> make_auto_household ()
  | "shopping" -> make_auto_shopping ()
  | "pandemic" -> make_auto_pandemic ()
  | "all" -> make_auto_all ()
  | _ -> draw_basic ()

(** [find_task y] returns the task at location [y] on the screen *)
let find_task y : string list = 
  let cat_lst_form = ref [] in 
  if !viewed_cat = "Car Tasks" || !viewed_cat = "School Tasks" || 
     !viewed_cat = "Household Tasks" || !viewed_cat = "Shopping Tasks" || 
     !viewed_cat = "Pandemic Tasks"
  then (cat_lst_form := Automatic.to_list_auto ~cat:auto_cat !viewed_cat)
  else cat_lst_form := Manual.to_list ~cat:cat !viewed_cat;
  let cat_lst_lst = make_tll !cat_lst_form !viewed_cat in
  let num_tasks = List.length cat_lst_lst in 
  let lower_bound = 305 - 15 * num_tasks in 
  if y < lower_bound then failwith "out of bounds" 
  else let plc = (y - 5) / 15 in 
    let n = 19 - plc in 
    List.nth cat_lst_lst n

let change_dd y = 
  draw_basic (); moveto 10 320; set_color red;
  draw_string "What do you want the new due date to be?";
  let cat_lst_form = ref [] in 
  let new_dd = string_input "" in 
  let task_changing = find_task y in 
  if !viewed_cat = "Car Tasks" || !viewed_cat = "School Tasks" || 
     !viewed_cat = "Household Tasks" || !viewed_cat = "Shopping Tasks" || 
     !viewed_cat = "Pandemic Tasks" 
  then begin
    Automatic.change_due_auto ~cat:auto_cat (List.nth task_changing 0) 
      (List.nth task_changing 1) new_dd;
    cat_lst_form := Automatic.to_list_auto ~cat:auto_cat !viewed_cat
  end
  else (change_due_date ~cat:cat (List.nth task_changing 0) 
          (List.nth task_changing 1) new_dd;
        cat_lst_form := Manual.to_list ~cat:cat !viewed_cat);
  let cat_lst_lst = make_tll !cat_lst_form !viewed_cat in
  draw_basic ();
  draw_task_list cat_lst_lst 

let change_pri y = 
  draw_basic (); moveto 10 320; set_color red;
  draw_string "What do you want the new priority to be?";
  let cat_lst_form = ref [] in 
  let new_pri = string_input "" in 
  let task_changing = find_task y in 
  if !viewed_cat = "Car Tasks" || !viewed_cat = "School Tasks" || 
     !viewed_cat = "Household Tasks" || !viewed_cat = "Shopping Tasks" || 
     !viewed_cat = "Pandemic Tasks" 
  then begin 
    Automatic.change_priority_auto ~cat:auto_cat (List.nth task_changing 0) 
      (List.nth task_changing 1) (int_of_string new_pri);
    cat_lst_form := Automatic.to_list_auto ~cat:auto_cat !viewed_cat
  end
  else (change_priority ~cat:cat (List.nth task_changing 0) 
          (List.nth task_changing 1) (int_of_string new_pri);
        cat_lst_form := Manual.to_list ~cat:cat !viewed_cat);
  let cat_lst_lst = make_tll !cat_lst_form !viewed_cat in
  draw_basic ();
  draw_task_list cat_lst_lst 

let change_name y =
  draw_basic (); moveto 10 320; set_color red;
  draw_string "What do you want the new name to be?";
  let cat_lst_form = ref [] in 
  let new_dd = string_input "" in 
  let task_changing = find_task y in 
  if !viewed_cat = "Car Tasks" || !viewed_cat = "School Tasks" || 
     !viewed_cat = "Household Tasks" || !viewed_cat = "Shopping Tasks" || 
     !viewed_cat = "Pandemic Tasks" 
  then begin 
    Automatic.change_name_auto ~cat:auto_cat (List.nth task_changing 0) 
      (List.nth task_changing 1) new_dd;
    cat_lst_form := Automatic.to_list_auto ~cat:auto_cat !viewed_cat
  end
  else (change_name ~cat:cat (List.nth task_changing 0) 
          (List.nth task_changing 1) new_dd;
        cat_lst_form := Manual.to_list ~cat:cat !viewed_cat);
  let cat_lst_lst = make_tll !cat_lst_form !viewed_cat in
  draw_basic ();
  draw_task_list cat_lst_lst 

(** [reset_car ()] is a helper function for [reset_gui_auto ()] *)
let reset_car () = 
  reset_car ~cat:auto_cat (); 
  let car_list = 
    to_list_auto ~cat:auto_cat "Car Tasks" in 
  let car_ll = make_tll car_list "Car Tasks" in 
  draw_basic (); draw_task_list car_ll;
  viewed_cat := "Car Tasks"

(** [reset_school ()] is a helper function for [reset_gui_auto ()] *)
let reset_school () = 
  reset_school ~cat:auto_cat (); 
  let school_list = to_list_auto ~cat:auto_cat "School Tasks" in 
  let school_ll = make_tll school_list "School Tasks" in 
  draw_basic (); draw_task_list school_ll;
  viewed_cat := "School Tasks"

(** [reset_household ()] is a helper function for [reset_gui_auto ()] *)
let reset_household () = 
  reset_household ~cat:auto_cat (); 
  let house_list = to_list_auto ~cat:auto_cat "Household Tasks" in 
  let house_ll = make_tll house_list "Household Tasks" in 
  draw_basic (); draw_task_list house_ll;
  viewed_cat := "Household Tasks"

(** [reset_shopping ()] is a helper function for [reset_gui_auto ()] *)
let reset_shopping () = 
  reset_shopping ~cat:auto_cat (); 
  let shopping_list = to_list_auto ~cat:auto_cat "Shopping Tasks" in 
  let shopping_ll = make_tll shopping_list "Shopping Tasks" in 
  draw_basic (); draw_task_list shopping_ll;
  viewed_cat := "Shopping Tasks"

(** [reset_pandemic ()] is a helper function for [reset_gui_auto ()] *)
let reset_pandemic () = 
  reset_pandemic ~cat:auto_cat (); 
  let pandemic_list = to_list_auto ~cat:auto_cat "Pandemic Tasks" in 
  let pandemic_ll = make_tll pandemic_list "Pandemic Tasks" in 
  draw_basic (); draw_task_list pandemic_ll;
  viewed_cat := "Pandemic Tasks"

let reset_gui_auto () = 
  draw_basic (); moveto 10 320; set_color red;
  draw_string "What do you want to reset? You may choose car, school, \
               household, shopping, or pandemic";
  let reset = string_input "" in 
  match reset with
  | "car" -> reset_car ()
  | "school" -> reset_school ()
  | "household" -> reset_household ()
  | "shopping" -> reset_shopping ()
  | "pandemic" -> reset_pandemic ()
  | _ -> draw_basic ()

(** [draw_appo a] draws [a] *)
let rec draw_appo a =
  if current_y () > 350 then moveto 10 350; 
  let y = current_y () in  
  match a with 
  | [] -> ()
  | t::d::m::l::n::""::[] -> 
    moveto 10 y;
    draw_string t;
    moveto 150 y;
    draw_string d;
    moveto 225 y;
    draw_string m;
    moveto 300 y;
    draw_string l;
    moveto 425 y; 
    draw_string n;
  | _ -> ()

(** [draw_appo_lst a] draws each appointment in [a] on a new line *)
let rec draw_appo_lst a =
  set_color black;
  match a with 
  | [] -> ()
  | t::d::m::l::n::tail -> draw_appo (t::d::m::l::n::[""]); 
    moveto 10 (current_y () - 15); draw_appo_lst tail
  | _ -> ()

let new_appo () = 
  draw_appointments (); set_color red; 
  draw_string "Type the name of your appointment"; 
  let title = (string_input "") in draw_appointments ();
  set_color red; draw_string "Type the date of your appointment";
  let date = (string_input "") in draw_appointments (); 
  set_color red; draw_string "Type the time of your appointment";
  let time = (string_input "") in draw_appointments (); set_color red; 
  draw_string "Type the location of your appointment. If none, press enter";
  let location = string_input "" in draw_appointments (); set_color red;
  draw_string "Type any notes for your appointment. If none, press enter";
  let notes = string_input "" in
  Appointments.add_app ~appo:apps title date time;
  draw_appointments ();
  if location = "" then () 
  else Appointments.add_location ~appo:apps title location;
  if notes = "" then () 
  else Appointments.add_app_info ~appo:apps title notes;
  draw_appointments ();
  let app_lst = Appointments.to_list_alt ~appo:apps () in 
  draw_appo_lst app_lst

(** [complete_app_gui ()] prompts the user to say what appointment they want
    to complete and draws the updated appointment list *)
let complete_app_gui () = 
  draw_appointments ();
  set_color red;
  draw_string "What appointment do you want to complete?";
  let title = (string_input "") in
  draw_appointments ();
  Appointments.complete_app ~appo:apps title;
  let app_lst = Appointments.to_list_alt ~appo:apps () in 
  draw_appo_lst app_lst

(** [delete_app_gui ()] prompts the user to say what appointment they want
    to delete and draws the updated appointment list *)
let delete_app_gui () = 
  draw_appointments ();
  set_color red;
  draw_string "What appointment do you want to delete?";
  let title = (string_input "") in
  draw_appointments ();
  Appointments.delete_app ~appo:apps title;
  let app_lst = Appointments.to_list_alt ~appo:apps () in 
  draw_appo_lst app_lst

(** [find_app_name y] returns the name of the appointment at location [y] *)
let find_app_name y = 
  let app_lst = Appointments.to_list_alt ~appo:apps () in 
  let num_apps = (List.length app_lst) / 5 in 
  let lower_bound = 365 - 15 * num_apps in 
  if y < lower_bound then failwith "out of bounds"
  else let plc = (y - 5) / 15 in 
    let n = 23 - plc in 
    List.nth app_lst (n * 5)

let info_gui y =
  draw_appointments ();
  set_color red;
  draw_string "What information do you want to add?";
  let info = (string_input "") in
  Appointments.add_app_info ~appo:apps (find_app_name y) info;
  draw_appointments ();
  let app_lst = Appointments.to_list_alt ~appo:apps () in 
  draw_appo_lst app_lst

let loc_gui y =
  draw_appointments ();
  set_color red;
  draw_string "What location do you want to add?";
  let loc = (string_input "") in
  Appointments.add_location ~appo:apps (find_app_name y) loc;
  draw_appointments ();
  let app_lst = Appointments.to_list_alt ~appo:apps () in 
  draw_appo_lst app_lst

(** [loop ()]  runs the GUI. It waits for the user to interact and calls the
    appropriate function based on the user input.
    NOTE: This function is over 20 lines because it needs to handle all the 
    different user inputs. There is no way to shorten it and it is not too
    complicated of a function. *)
let rec loop () = 
  let e = wait_next_event [Key_pressed; Mouse_motion; Button_down] in
  let new_task = if e.key = 't' && !is_todo then task_input () else () in
  let comp_task = if e.key = 'c' && !is_todo then complete_task_gui ()
    else () in
  let del_task = if e.key = 'd' && !is_todo then delete_task_gui () else () in
  let view = if e.key = 'v' && !is_todo then draw_list () else () in
  let sort = if e.key = 's' && !is_todo then sort_gui () else () in 
  let auto = if e.key = 'a' && !is_todo then make_auto () else () in 
  let reset = if e.key = 'r' && !is_todo then reset_gui_auto () else () in 
  let switch_to_appo = if e.mouse_x > 299 && e.mouse_x < 371 && e.mouse_y > 459 
                          && e.mouse_y < 474 && e.button && !is_todo
    then draw_appointments () in 
  let switch_to_todo = if e.mouse_x > 99 && e.mouse_x < 169 && e.mouse_y > 459 
                          && e.mouse_y < 474 && e.button && (not !is_todo)
    then draw_basic () in 
  let click_due = if e.mouse_x > 425 && e.mouse_x < 520 && e.mouse_y < 335 
                     && e.button && !is_todo
    then change_dd (e.mouse_y) in 
  let click_pri = if e.mouse_x > 550 && e.mouse_x < 640 && e.mouse_y < 335 
                     && e.button && !is_todo
    then change_pri (e.mouse_y) in 
  let change_name = if e.mouse_x > 149 && e.mouse_x < 301 && e.mouse_y < 335 
                       && e.button && !is_todo 
    then change_name (e.mouse_y) in
  let new_app = if e.key = 'n' && (not !is_todo) then new_appo () else () in 
  let complete_app = if e.key = 'c' && (not !is_todo) then complete_app_gui ()
    else () in 
  let delete_app = if e.key = 'd' && (not !is_todo) then delete_app_gui ()
    else () in 
  let add_info = if e.mouse_x > 424 && e.mouse_x < 640 && e.mouse_y < 365 
                    && e.button && (not !is_todo) 
    then info_gui (e.mouse_y) in
  let add_loc = if e.mouse_x > 299 && e.mouse_x < 425 && e.mouse_y < 365 
                   && e.button && (not !is_todo) 
    then loc_gui (e.mouse_y) in
  new_task; comp_task; del_task; view; sort; auto; reset; switch_to_appo;
  switch_to_todo; click_due; click_pri; change_name; new_app; complete_app;
  delete_app; add_info; add_loc;
  if e.key <> 'q' then loop () else ()

(** [open_window] opens an empty window *)
let open_window = open_graph " 640x480"; set_window_title "To-Do List"

(** Runs the gui *)
let () = 
  open_window;
  draw_basic ();
  loop ();
  close_graph ();

