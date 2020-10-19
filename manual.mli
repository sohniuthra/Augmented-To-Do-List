(** 
   Representation of a manual to-do list.

   This module represents the data stored in a to-do list, including
   data for each task.
*)


(** The abstract type of values representing a to-do list. *)
type t 

(** The abstract type representing one task of the to-do list *)
type task

(** *)
type exit_name = string

(** Raised when an unknown room is encountered. *)
exception UnknownTask of task

(** Raised when an unknown exit is encountered. *)
exception InvalidTask 

(** Initialize an empty to-do list *)
val empty_list : unit

(** [add_task t task] is the list with the new task [task]. *)
val add_task : t -> task -> t

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val remove_task : t -> task -> t

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val completed_tasks : t -> task -> t


