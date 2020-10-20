(** 
   Implementation of a manual to-do list.

   This module implements the data stored in a to-do list, including
   data for each task.
*)



(** The abstract type representing one task of the to-do list *)
type task = {
  name : string;
  created_date : string;
  due_date : string;
  priority : int;
  category : 


}

(** The type representing a to-do list.  Type is [task list] *)
type t = {
  name : string;
  task_list : task list;
}

(** Raised when an unknown task is encountered. *)
exception UnknownTask of task

(** Raised when an invalid task is encountered. *)
exception InvalidTask 

(** Initialize an empty to-do list *)
let empty_list
val empty_list : unit

(** [add_task t task] is the incompleted to-do list [t] with the new task [task]. *)
val add_task : t -> task -> t

(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
val complete_task : t -> task -> t

(** [delete_task t task] is an updated to-do list with [task] removed from [t] . *)
val delete_task : t -> task -> t




