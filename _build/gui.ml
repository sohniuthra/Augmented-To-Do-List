open Graphics
open Manual
open Printf

let rec string_input str =
  let e = wait_next_event [Key_pressed] in 
  if e.key <> '0' 
  then string_input (str ^ (Char.escaped e.key))
  else str

(** [draw_int i] draws int [i] as a string *) 
let draw_int i =
  draw_string (string_of_int i)

let task_input () =
  let cat = empty_cat () in
  draw_string "Type the name of your category";
  let category = (string_input "") in
  draw_string "Type the name of your task";
  let name = (string_input "") in
  draw_string "Type the due date in an mm/dd/yyyy format";
  let due = (string_input "") in
  draw_string "Type the priority of your task";
  let priority = int_of_string (string_input "") in
  Manual.create_task ~cat:cat category name due priority


let rec loop () = 
  let e = wait_next_event [Key_pressed] in

  (*let mouse_description = sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y in*)
  (*let key_description = if e.keypressed 
    then sprintf "Key %c was pressed" e.key 
    else "" in*)

  let a_pressed = if e.key = 'a' then sprintf "pressed a" else "" in

  let n_pressed = if e.key = 'n' 
    then (draw_string "Type your task name. Press 0 when you are finished"; 
          string_input "")
    else "" in

  let new_task = if e.key = 't'
    then task_input ()
    else () in

  (*clear_graph ();*)
  (*moveto 0 100; draw_string key_description;*)
  moveto 0 200; draw_string a_pressed;
  moveto 15 100; draw_string n_pressed;
  new_task;
  (*moveto 0 0; draw_string mouse_description;*)

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

(** [draw_str_list slst] takes string list [slst] and draws it with each item 
    on a new line *) 
let rec draw_str_list slst =
  match slst with 
  | [] -> ()
  | h::t -> draw_string h; moveto (10) (current_y () - 15); 
    draw_str_list t

let () = open_window;
  moveto 10 460;
  set_color blue;
  draw_string "Welcome to your new to-do list!"; 
  moveto 10 440;
  draw_string "Press t to create a new task"; (* this doesn't work fully yet *)
  moveto 10 425;
  draw_string "Press c to complete a task"; (* need to implement *)
  moveto 10 410;
  draw_string "Press d to delete a task"; (* need to implement *)
  (*draw_str_list ["a"; "cat"];*)
  set_color black;
  loop ();
  close_graph ();

