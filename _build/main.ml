(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to your new to-do list.\n")
(* print_endline "Please enter the name of the game file you want to load.\n"; *)
(* print_string  "> " *)
(* match read_line () with
   | exception End_of_file -> ()
   | file_name -> play_game file_name *)

(* Execute the game engine. *)
let () = main ()