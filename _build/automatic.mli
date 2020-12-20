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

(** [init_task n d p] initializes a task with name [n], created date (made 
    automatically with [todays_date()], due date [d], and priority [p] *)
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

(** [delete_task_auto c t] allows the user to delete a task with name [t] out 
    of a category with name [c] in the automatic list. 
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not
    found in the list of categories. *)
val delete_task_auto : ?cat:(t list ref) -> string -> string -> unit 

(** [create_task_auto c t d p] updates the category with name [c] with the new 
    task created by [init_task t d p].
    Raises [CategoryNotFound c] if a category with name [c] is not
    found in the list of categories. *)
val create_task_auto : ?cat:(t list ref) -> string -> string -> string -> int -> 
  unit 

(** [complete_task_auto c t] adds the task with name [t] in category with name 
    [c] to the completed category, and removes the task from the orginal 
    category with name [c].
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound cat_name] if a category with name [c] is not
    found in the list of categories. *)
val complete_task_auto : ?cat:(t list ref) -> string -> string -> unit

(** [change_priority_auto c t p] updates the task with name [t] in category with 
    name [c] to have a new priority [p].
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not
    found in the list of categories. *)
val change_priority_auto : ?cat:(t list ref) -> string -> string -> int ->  unit

(** [change_due_auto c t d] updates the task with name [t] in category with 
    name [c] to have a new due date [d].
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not
    found in the list of categories. *)
val change_due_auto : ?cat:(t list ref) -> string -> string -> string ->  unit

(** [change_name_auto c t n] updates the task with name [t] in category with 
    name [c] to have a new name [n].
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not
    found in the list of categories. *)
val change_name_auto : ?cat:(t list ref) -> string -> string -> string ->  unit

(** [delete_cat_auto c] updates the to-do list by deleting the category
    with name [c].
    Raises [CategoryNotFound c] if a category with name [c] is not
    found in the list of categories. *)
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

(** [to_list_auto c] is a list containing the same elements and the same 
    category name [c] as the category with name [c].
    Raises [CategoryNotFound c] if a category with name [cat_name] is not
    found in the list of categories. *)
val to_list_auto : ?cat:(t list ref) -> string ->  string list