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

(** [init_task name created_date due_date priority] initializes a task with 
    name [name], created date [created_date], due date [due_date], and priority 
    [priority] *)
val init_task : string -> string -> int -> task

(** [init_todolist name task_list] initializes a to-do list with name [name] 
    and task list [lst]. *)
val init_todolist : string -> task list -> t

(** [empty_list cat_name] initializes an empty to-do list with category name 
    [cat_name]. *)
val empty_list : string -> t

(** [create_task cat_name name created_date due_date priority] updates the to-do
    list with name [cat_name] with the new task [name created_date due_date
     priority]. If category name [cat_name] does not already exist, a new 
     category with [cat_name] is created with task 
     [name created_date due_date priority] in the task list. *)
val create_task : string -> string -> string -> string -> int -> unit 

val access_cat : unit -> t list



(*
(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
val complete_task : t -> task -> t

(** [delete_task t task] is an updated to-do list with [task] removed from 
[t].*)
val delete_task : t -> task -> t 
*)






