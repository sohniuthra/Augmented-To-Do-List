open Graphics
open Manual
open Printf

let rec loop () = 
  let e = wait_next_event [Mouse_motion; Key_pressed] in

  (*let mouse_description = sprintf "Mouse position: %d,%d" e.mouse_x e.mouse_y in*)
  let key_description = if e.keypressed then sprintf "Key %c was pressed" e.key 
    else "" in

  let n_pressed = if e.key = 'n' then sprintf "new task" else "" in

  clear_graph ();
  moveto 0 100; draw_string key_description;
  moveto 0 200; draw_string n_pressed;
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
  draw_string "Press 'n' to create a new task"; (* this doesn't work yet *)
  set_color black;
  moveto 10 420;
  draw_str_list ["a"; "cat"];
  loop ();
  close_graph ();

