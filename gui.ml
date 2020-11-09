open Graphics

let rec loop () = loop ()

let open_window = open_graph " 640x480"; set_window_title "To-Do List"

let () = open_window;
  moveto 10 460;
  set_color blue;
  draw_string "Welcome to your new to-do list"; 
  loop ()

