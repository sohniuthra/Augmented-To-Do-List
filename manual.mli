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

(** Initialize an empty to-do list *)
val empty_list : unit

(** [add_task t task] is the incompleted to-do list [t] with the new task [task]. *)
val add_task : t -> task -> t

(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
val complete_task : t -> task -> t

(** [delete_task t task] is an updated to-do list with [task] removed from [t] . *)
val delete_task : t -> task -> t




