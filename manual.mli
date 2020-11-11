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

(* SPEC??? *)
val access_cat : ?cat:(t list ref) -> unit -> t list

(** [empty_cat ()] initializes an empty category list. *)
val empty_cat : unit -> t list ref

val todays_date : unit -> string

(** [init_task name due_date priority] initializes a task with 
    name [name], created date [created_date], due date [due_date], and priority 
    [priority] *)
val init_task : string -> string -> int -> task

(** [init_todolist name task_list] initializes a to-do list with name [name] 
    and task list [lst]. *)
val init_todolist : string -> task list -> t

(** [empty_list cat_name] initializes an empty to-do list with category name 
    [cat_name]. *)
val empty_list : string -> t

(** [sort_list cat_name] sorts a to-do list with category [name] by priority
    of tasks  *)
val sort_list : ?cat:(t list ref) -> string -> string -> unit

(** [sort_task task cat_name] inserts a task into a sorted list - needed? *)

(** [create_task cat_name name due_date priority] updates the to-do
    list with name [cat_name] with the new task [name due_date priority]. 
    If category name [cat_name] does not already exist, a new category with 
    [cat_name] is created with task [name created_date due_date priority] 
    in the task list. *)
val create_task : ?cat:(t list ref) -> string -> string -> string -> int -> unit 

val empty_cat : unit -> t list ref

(** [complete_task t task] is an updated completed to-do list with name
    [cat_name] with task named [task_name]. *)
val complete_task : ?cat:(t list ref) -> string -> string -> unit

(** [delete_task t task] is an updated to-do list with task with name 
    [task_name ] deleted from category with name [cat_name]. *)
val delete_task : ?cat:(t list ref) -> string -> string -> unit

(** [to_list cat_name] is a list containing the same elements and the same 
    category name [cat_name] as the category with name [cat_name].*)
val to_list : ?cat:(t list ref) -> string ->  string list

(** [to_list cat_name lst] is a list containing the same elements and the same 
    category name [cat_name] as the category with name [cat_name] given 
    a task list [lst] as an additional input.*)
(* val to_list : string ->  task list -> string list *)







