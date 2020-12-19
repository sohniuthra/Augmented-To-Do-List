(**
   Representation of the GUI. 

   This represents the functions that the GUI will implement.
*) 

(** [draw_basic ()] is the basic window that should open when the application
    opens *) 
val draw_basic : unit -> unit 

(** [string_input str] produces a string from anything that the user types 
    before pressing enter *)
val string_input : string -> string 

(** [view_category category] shows the [category] to-do list *)
val view_category : string -> unit

(** [view_all_categories ()] shows all the to-do lists *) 
val view_all_categories : unit -> unit

(** [task_input ()] prompts the user to type in the information for a new task
    and creates it *) 
val task_input : unit -> unit

(** [complete_task_gui ()] prompts the user to type in the category and name of 
    a task they want to complete *) 
val complete_task_gui : unit -> unit 

(** [delete_task_gui ()] prompts the user to type in the category and name of a 
    task they want to delete *) 
val delete_task_gui : unit -> unit 

(** [draw_list ()] allows the user to choose which category they want to see
    and draws it in the window *)
val draw_list : unit -> unit 

(** [sort_gui ()] allows the user to choose whether to sort by priority or date
    and then sorts the category *)
val sort_gui : unit -> unit

(** [make_auto ()] allows the user to choose what automatic list they would
    like to create and creates it *)
val make_auto : unit -> unit

(** [draw_tsk_list tlst] takes a list of tasks and produces it on the window *)
val draw_tsk_list : string list list -> unit