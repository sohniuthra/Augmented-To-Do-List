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

(** Raised when a category is not found. *)
exception CategoryNotFound of string


let categories = ref []

(** Initialize an empty to-do list *)
let empty_list cat_name = {
  name = cat_name;
  task_list = [];
}


let add_task t task = {
  name = t.name;
  task_list = task :: t.task_list
}


let add_task_to_category cat_name task = 
  try add_task (List.find (fun x -> x.name = cat_name) (!categories)) task
  with Not_found -> begin
      let new_cat = add_task (empty_list cat_name) task in
      ignore (new_cat :: !categories); new_cat
    end

(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
(*val complete_task : t -> task -> t*)


(** [delete_task t task] is an updated to-do list with [task] removed from [t] . *)
(*val delete_task : t -> task -> t

  let new_cat *)


