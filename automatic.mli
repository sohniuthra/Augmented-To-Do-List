(** 
   Representation of a automatic to-do list.

   This module represents the data stored in an automatic to-do list, 
   where each task is predetermined, including
   data for each task.

   EVERYTHING NOT LISTED IN THE SIG THE USER DOES NOT NEED
   AND THEREFORE SHOULD ONLY BE SHOWN IN THE STRUCT!!
*)

(** The abstract type representing an automatic to-do list. 
    Tasks should be inserted without user input*)
type t 

(** The abstract type representing one task of the automatic to-do list *)
type task

(** Raised when an unknown task is encountered. *)
exception UnknownTask of task

(** Function for user to instantiate the to-do list with tasks already 
    inserted
    NOTE TO BE DELETED: the type is unit to unit because
    the user should not need to put in any information and
    the make_auto should change the categories variable
    as designed similarly to manual *)
val make_auto : ?cat:(t list ref) -> unit -> unit

val make_car_auto : ?cat:(t list ref) -> unit -> unit

val make_school_auto : ?cat:(t list ref) -> unit -> unit

val make_household_auto : ?cat:(t list ref) -> unit -> unit

val make_shopping_auto : ?cat:(t list ref) -> unit -> unit

val make_pandemic_auto : ?cat:(t list ref) -> unit -> unit

(** [empty_cat ()] initializes an empty category list. *)
val empty_cat_auto : unit -> t list ref


(** Funtion to access the automatic list for user to be able to view *)
val access_cat : ?cat:(t list ref) -> unit -> t list

(** Function for user to be able to complete task and put it in a
    separate complete tasks list that should've already been 
    made also. 
    Invariant: completed list clears if it passes a 
    certain number of elements. *)
val complete_task_auto : ?cat:(t list ref) -> string -> string -> unit

(** Function for user to be able to delete task out of 
    automatic list.  Also used to delete task out of 
    completed list when completed list becomes too large. *)
val delete_task_auto : ?cat:(t list ref) -> string -> string -> unit 

(** [create_task cat_name name due_date priority] updates the to-do
    list with name [cat_name] with the new task [name due_date priority]. 
    If category name [cat_name] does not already exist, a new category with 
    [cat_name] is created with task [name created_date due_date priority] 
    in the task list. *)
val create_task_auto : ?cat:(t list ref) -> string -> string -> string -> int -> 
  unit 

val change_priority : ?cat:(t list ref) -> string -> string -> int ->  unit

val change_due : ?cat:(t list ref) -> string -> string -> string ->  unit

(** [to_list cat_name] is a list containing the same elements and the same 
    category name [cat_name] as the category with name [cat_name].*)
val to_list_auto : ?cat:(t list ref) -> string ->  string list