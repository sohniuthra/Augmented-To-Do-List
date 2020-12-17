(** 
   Representation of a automatic to-do list.

   This module represents the data stored in an automatic to-do list, 
   where each task is predetermined, including
   data for each task.
*)

(** The abstract type representing one task of the automatic to-do list *)
type task

(** The abstract type representing an automatic to-do list. 
    Tasks should be inserted without user input*)
type t 

(** Raised when an unknown task is encountered. *)
exception UnknownTask of task

(** [todays_date ()] is today's date in string form. *)
val todays_date : unit -> string

(** [init_task name due_date priority] initializes a task with 
    name [name], created date (made automatically with [todays_date()], 
    due date [due_date], and priority [priority] *)
val init_task : string -> string -> int -> task

(** [make_auto ()] is the automatic to-do list. The user does not need to input 
    anything because there are pre-set categories and tasks. *)
val make_auto : ?cat:(t list ref) -> unit -> unit

(** [make_car_auto ()] is the automatic to-do list of just the car category. It 
    includes pre-set tasks. *)
val make_car_auto : ?cat:(t list ref) -> unit -> unit

(** [make_school_auto ()] is the automatic to-do list of just the school category. 
    It includes pre-set tasks. *)
val make_school_auto : ?cat:(t list ref) -> unit -> unit

(** [make_household_auto ()] is the automatic to-do list of just the household
    category. It includes pre-set tasks. *)
val make_household_auto : ?cat:(t list ref) -> unit -> unit

(** [make_shopping_auto ()] is the automatic to-do list of just the shopping
    category. It includes pre-set tasks. *)
val make_shopping_auto : ?cat:(t list ref) -> unit -> unit

(** [make_pandemic_auto ()] is the automatic to-do list of just the pandemic
    category. It includes pre-set tasks. *)
val make_pandemic_auto : ?cat:(t list ref) -> unit -> unit

(** [empty_cat ()] initializes an empty automatic to-do list. *)
val empty_cat_auto : unit -> t list ref

(** [access_cat ()] allows the user to view the entire to-do list. *)
val access_cat : ?cat:(t list ref) -> unit -> t list

(** [delete_task_auto cat_name task_name] allows the user to delete a task with name
    [task_name] out of a category [cat_name] in the automatic list. *)
val delete_task_auto : ?cat:(t list ref) -> string -> string -> unit 

(** [create_task cat_name name due_date priority] updates the to-do
    list with name [cat_name] with the new task [name due_date priority]. *)
val create_task_auto : ?cat:(t list ref) -> string -> string -> string -> int -> 
  unit 

(** [change_priority cat_name task_name new_priority] updates the task with name 
    [task_name] in category with name [cat_name] to have a new priority 
    [new_priority].*)
val change_priority : ?cat:(t list ref) -> string -> string -> int ->  unit

(** [change_due cat_name task_name new_date] updates the task with name 
    [task_name] in category with name [cat_name] to have a new due date 
    [new_date].*)
val change_due : ?cat:(t list ref) -> string -> string -> string ->  unit

(** [change_name cat_name task_name new_name] updates the task with name 
    [task_name] in category with name [cat_name] to have a new name
    [new_name].*)
val change_name : ?cat:(t list ref) -> string -> string -> string ->  unit

(** [delete_cat_auto cat_name] updates the to-do list by deleting the category
    with name [cat_name]. *)
val delete_cat_auto : ?cat:(t list ref) -> string -> unit

(** [reset_car ()] resets the car category of the automatic to-do list to
    its pre-set tasks, due dates, and priorities. *)
val reset_car : ?cat:(t list ref) -> unit -> unit

(** [reset_school ()] resets the school category of the automatic to-do list to
    its pre-set tasks, due dates, and priorities. *)
val reset_school : ?cat:(t list ref) -> unit -> unit

(** [reset_household ()] resets the household category of the automatic to-do 
    list to its pre-set tasks, due dates, and priorities. *)
val reset_household : ?cat:(t list ref) -> unit -> unit

(** [reset_car ()] resets the shopping category of the automatic to-do list to
    its pre-set tasks, due dates, and priorities. *)
val reset_shopping : ?cat:(t list ref) -> unit -> unit

(** [reset_car ()] resets the pandemic category of the automatic to-do list to
    its pre-set tasks, due dates, and priorities. *)
val reset_pandemic : ?cat:(t list ref) -> unit -> unit

(** [reset_all_cat ()] resets every category of the automatic to-do list to
    all pre-set tasks, due dates, and priorities. *)
val reset_all_cat : ?cat:(t list ref) -> unit -> unit

(** [to_list cat_name] is a list containing the same elements and the same 
    category name [cat_name] as the category with name [cat_name].*)
val to_list_auto : ?cat:(t list ref) -> string ->  string list

(** Function for user to be able to complete task and put it in a
    separate complete tasks list that should've already been 
    made also. 
    Invariant: completed list clears if it passes a 
    certain number of elements. *)
(* val complete_task_auto : ?cat:(t list ref) -> string -> string -> unit *)