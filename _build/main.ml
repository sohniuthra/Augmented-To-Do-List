(** [main ()] opens the to-do list and gives the user directions for how to do it. *)
let main () =
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to your new to-do list.\n
                  \n\nDirections: To add a task, type create_task followed by \nthe \
                   name of the task (in quotes), \ntoday's date (in quotes), \nthe \
                   due date (in quotes), \nand the priority on a scale from 1-5.\n
                   \n\nTo view your whole to-do list type: \naccess_cat followed by () which will show all current tasks")

(** starts the to-do list *)
let () = main ()







