open Graphics
open Manual

let rec loop () = loop ()

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
  | h::t -> draw_string h; moveto (current_x ()) (current_y () - 15); 
    draw_str_list t

let () = open_window;
  moveto 10 460;
  set_color blue;
  draw_string "Welcome to your new to-do list!"; 
  moveto 10 440;
  draw_string "Press 'n' to create a new task";
  set_color black;
  loop ()

