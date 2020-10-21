(** 
   Representation of a manual to-do list.

   This module represents the data stored in a to-do list, including
   data for each task.
*)


(** The abstract type representing a to-do list. *)
type t 

(** The abstract type representing one task of the to-do list *)
type task

(** Raised when an unknown task is encountered. *)
exception UnknownTask of task

(** Raised when an invalid task is encountered. *)
exception InvalidTask 

(** Raised when a category is not found. *)
exception CategoryNotFound of string

(** [empty_list cat_name] initializes an empty to-do list with category name [cat_name]. *)
val empty_list : string -> t

(** [add_task t task] is the incompleted to-do list [t] with the new task [task]. *)
val add_task : t -> task -> t

(** [add_task_to_category cat_name task ] is the incompleted categorized to-do list [t] that has [cat_name] with the new task [task]. *)
val add_task_to_category : string -> task -> t

(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
val complete_task : t -> task -> t

(** [delete_task t task] is an updated to-do list with [task] removed from [t] . *)
val delete_task : t -> task -> t








