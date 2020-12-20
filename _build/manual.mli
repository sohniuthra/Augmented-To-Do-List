(** 
   Representation of a manual to-do list.

   This module represents the data stored in a to-do list, including
   data for each task.
*)

(** The abstract type representing one task of the manual to-do list *)
type task

(** The abstract type representing a manual to-do list. *)
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

(** [init_task n d p] initializes a task with name [n], created date (made 
    automatically with [todays_date()], due date [d], and priority [p] *)
val init_task : string -> string -> int -> task

(** [init_todolist n tl] initializes a to-do list with name [n] 
    and task list [tl]. *)
val init_todolist : string -> task list -> t

(** [empty_list c] initializes an empty to-do list with category name [c]. *)
val empty_list : string -> t

(** [sort_list c s] sorts a to-do list with category [c] by [s] of tasks. 
    Requires: [sort_by] must either be "Priority" or "Due Date" *)
val sort_list : ?cat:(t list ref) -> string -> string -> unit

(** [create_task c n d p] updates the to-do list with name [c] with the new 
    task created by [init_task n d p]. 
    If the category with name [c] does not already exist, a new category with 
    name [c] is created with task created by init_task [n d p] in the task 
    list. *)
val create_task : ?cat:(t list ref) -> string -> string -> string -> int -> unit 

(** [complete_task c t] is an updated completed to-do list (named "Completed")
    with completed task named [t] from category with name [c] added to it. 
    Task [t] is then removed from category with name [c].
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not found
    in the list of categories. *)
val complete_task : ?cat:(t list ref) -> string -> string -> unit

(** [delete_task c t] is an updated to-do list with task with name [t] deleted 
    from category with name [c].
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not found
    in the list of categories. *)
val delete_task : ?cat:(t list ref) -> string -> string -> unit

(** [to_list c] is a category with name [c] in a list representation (it is a
    list containing the same elements and the same category name [c] as the 
    category with name [c]).
    Raises [CategoryNotFound c] if a category with name [c] is not found
    in the list of categories.*)
val to_list : ?cat:(t list ref) -> string ->  string list

(** [change_name c t n] changes the name of the task with name [t] in the 
    category with name [c] from old name [t] to new name [n].
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not found
    in the list of categories. *)
val change_name : ?cat:(t list ref) -> string -> string ->  string -> unit

(** [change_due_date c t d] changes the due date of the task with name [t] in 
    the category with name [c] from old due date to [d]. 
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not found
    in the list of categories. *)
val change_due_date : ?cat:(t list ref) -> string -> string ->  string -> unit

(** [change_priority c t p] changes the priority of task with name [t] in the 
    category with name [c] from old priority to [p]. 
    Raises [TaskNotFound t] if a task with name [t] is not found in the 
    category with name [c]. 
    Raises [CategoryNotFound c] if a category with name [c] is not found
    in the list of categories. *)
val change_priority: ?cat:(t list ref) -> string -> string -> int -> unit









