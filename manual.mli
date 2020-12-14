(** 
   Representation of a manual to-do list.

   This module represents the data stored in a to-do list, including
   data for each task.
*)

(** The abstract type representing one task of the to-do list *)
type task

(** The abstract type representing a to-do list. *)
type t 

(** Raised when an invalid task is encountered. *)
exception InvalidTask 

(** Raised when a task is not found. *)
exception TaskNotFound of string

(** Raised when a category is not found. *)
exception CategoryNotFound of string

(** [access_cat ()] allows the user to view the entire to-do list. *)
val access_cat : ?cat:(t list ref) -> unit -> t list

(** [empty_cat ()] initializes an empty category list. *)
val empty_cat : unit -> t list ref

(** [todays_date ()] is today's date in string form. *)
val todays_date : unit -> string

(** [init_task name due_date priority] initializes a task with 
    name [name], created date (made automatically with [todays_date()], 
    due date [due_date], and priority [priority] *)
val init_task : string -> string -> int -> task

(** [init_todolist name task_list] initializes a to-do list with name [name] 
    and task list [lst]. *)
val init_todolist : string -> task list -> t

(** [empty_list cat_name] initializes an empty to-do list with category name 
    [cat_name]. *)
val empty_list : string -> t

(** [sort_task task cat_name] inserts a task into a sorted list - needed? *)

(** [sort_list cat_name] sorts a to-do list with category [name] by [sort_by]
    of tasks. 
    Requires: [sort_by] must either be Priority or Due Date *)
val sort_list : ?cat:(t list ref) -> string -> string -> unit

(** [create_task cat_name name due_date priority] updates the to-do
    list with name [cat_name] with the new task [name due_date priority]. 
    If category name [cat_name] does not already exist, a new category with 
    [cat_name] is created with task [name created_date due_date priority] 
    in the task list. *)
val create_task : ?cat:(t list ref) -> string -> string -> string -> int -> unit 

(** [complete_task t task] is an updated completed to-do list with name
    [cat_name] with completed task named [task_name] added to it. *)
val complete_task : ?cat:(t list ref) -> string -> string -> unit

(** [delete_task t task] is an updated to-do list with task with name 
    [task_name ] deleted from category with name [cat_name]. *)
val delete_task : ?cat:(t list ref) -> string -> string -> unit

(** [to_list cat_name] is a list containing the same elements and the same 
    category name [cat_name] as the category with name [cat_name].*)
val to_list : ?cat:(t list ref) -> string ->  string list

(** [change_due_date cat_name task_name new_date] changes the due date of the 
    task with name [task_name] in the category with name [cat_name] from old 
    due date to [new_date]. *)
val change_due_date : ?cat:(t list ref) -> string -> string ->  string -> unit

(** [change_priority cat_name task_name new_priority] changes the priority of 
    the task with [task_name] in the category with name [cat_name] from 
    old priority to [new_priority]. *)
val change_priority: ?cat:(t list ref) -> string -> string -> int -> unit

(** [todays_tasks] is a task list containing all of the tasks due today *)
val todays_tasks : unit -> task list







