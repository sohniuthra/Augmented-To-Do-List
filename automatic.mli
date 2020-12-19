(** 
   Representation of an automatic to-do list.

   This module represents the data stored in an automatic to-do list, 
   where each task is predetermined, including data for each task (the user
   does have an option to add additional tasks, though).
*)

(** The abstract type representing one task of the automatic to-do list *)
type task

(** The abstract type representing an automatic to-do list. Tasks are intially
    inserted without user input. *)
type t 

(** Raised when an invalid task is encountered. *)
exception InvalidTask 

(** Raised when a task is not found. *)
exception TaskNotFound of string

(** Raised when a category is not found. *)
exception CategoryNotFound of string

(** [empty_cat ()] initializes an empty automatic to-do list. *)
val empty_cat_auto : unit -> t list ref

(** [access_cat ()] allows the user to view the entire to-do list. *)
val access_cat : ?cat:(t list ref) -> unit -> t list

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

(** [make_school_auto ()] is the automatic to-do list of just the school
    category. It includes pre-set tasks. *)
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

(** [make_completed_auto ()] is the automatic to-do list of just the completed
    tasks category. *)
val make_completed_auto : ?cat:(t list ref) -> unit -> unit

(** [delete_task_auto cat_name task_name] allows the user to delete a task with 
    name [task_name] out of a category [cat_name] in the automatic list. 

    Raises [TaskNotFound task_name] if a task with name [task_name] is not found
    in the to-do list. 
    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
val delete_task_auto : ?cat:(t list ref) -> string -> string -> unit 

(** [create_task cat_name task_name due_date priority] updates the to-do
    list with name [cat_name] with the new task [task_name due_date priority].

    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
val create_task_auto : ?cat:(t list ref) -> string -> string -> string -> int -> 
  unit 

(** [complete_task cat_name task_name] adds the task with name [task_name] in 
    category with name [cat_name] to the completed category, and removes the
    task from the orginal category with name [cat_name].

    Raises [TaskNotFound task_name] if a task with name [task_name] is not found
    in the to-do list. 
    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
val complete_task_auto : ?cat:(t list ref) -> string -> string -> unit

(** [change_priority cat_name task_name new_priority] updates the task with name 
    [task_name] in category with name [cat_name] to have a new priority 
    [new_priority].

    Raises [TaskNotFound task_name] if a task with name [task_name] is not found
    in the to-do list. 
    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
val change_priority_auto : ?cat:(t list ref) -> string -> string -> int ->  unit

(** [change_due cat_name task_name new_date] updates the task with name 
    [task_name] in category with name [cat_name] to have a new due date 
    [new_date].

    Raises [TaskNotFound task_name] if a task with name [task_name] is not found
    in the to-do list. 
    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
val change_due_auto : ?cat:(t list ref) -> string -> string -> string ->  unit

(** [change_name cat_name task_name new_name] updates the task with name 
    [task_name] in category with name [cat_name] to have a new name
    [new_name].

    Raises [TaskNotFound task_name] if a task with name [task_name] is not found
    in the to-do list. 
    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
val change_name_auto : ?cat:(t list ref) -> string -> string -> string ->  unit

(** [delete_cat_auto cat_name] updates the to-do list by deleting the category
    with name [cat_name].

    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
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

(** [reset_shopping ()] resets the shopping category of the automatic to-do list
     to its pre-set tasks, due dates, and priorities. *)
val reset_shopping : ?cat:(t list ref) -> unit -> unit

(** [reset_pandemic ()] resets the pandemic category of the automatic to-do list
     to its pre-set tasks, due dates, and priorities. *)
val reset_pandemic : ?cat:(t list ref) -> unit -> unit

(** [reset_completed ()] resets the completed tasks category of the automatic
    to-do list to an empty task list. *)
val reset_completed : ?cat:(t list ref) -> unit -> unit

(** [reset_all_cat ()] resets every category of the automatic to-do list to
    all pre-set tasks, due dates, and priorities. The completed tasks category
    is reset to an empty task list. *)
val reset_all_cat : ?cat:(t list ref) -> unit -> unit

(** [to_list cat_name] is a list containing the same elements and the same 
    category name [cat_name] as the category with name [cat_name].

    Raises [CategoryNotFound cat_name] if a category with name [cat_name] is not
    found in the to-do list. *)
val to_list_auto : ?cat:(t list ref) -> string ->  string list

(** Function for user to be able to complete task and put it in a
    separate complete tasks list that should've already been 
    made also. 
    Invariant: completed list clears if it passes a 
    certain number of elements. *)
(* val complete_task_auto : ?cat:(t list ref) -> string -> string -> unit *)