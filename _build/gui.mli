(**
   Representation of the GUI. 

   This represents the functions that the GUI will implement.
*) 

(** [draw_basic ()] is the basic window for the to-do list *) 
val draw_basic : unit -> unit 

(** [draw_appointments ()] is the basic window for appointments *)
val draw_appointments : unit -> unit

(** [string_input s] produces a string from anything that the user types 
    before pressing enter *)
val string_input : string -> string 

(** [view_category c] shows the to-do list of the category with name [c] *)
val view_category : string -> unit

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

(** [reset_gui_auto ()] resets an automatic list *)
val reset_gui_auto : unit -> unit 

(** [change_dd y] changes the due date of a task at y-coordinate [y] *)
val change_dd : int -> unit 

(** [change_pri y] changes the priority of a task at y-coordinate [y] *)
val change_pri : int -> unit 

(** [change_name y] changes the name of a task at y-coordinate [y] *)
val change_name : int -> unit 

(** [new_appo ()] creates a new appointment in the gui *)
val new_appo : unit -> unit 

(** [complete_app_gui ()] completes an appointment in the gui *)
val complete_app_gui : unit -> unit 

(** [delete_app_gui ()] deletes an appointment in the gui *)
val delete_app_gui : unit -> unit 

(** [info_gui y] changes the information of a appointment at y-coordinate [y] *)
val info_gui : int -> unit 

(** [loc_gui y] changes the location of a appointment at y-coordinate [y] *)
val loc_gui : int -> unit
