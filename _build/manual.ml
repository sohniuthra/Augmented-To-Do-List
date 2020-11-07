open Unix
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
  c_name : string;
  task_list : task list; 
}

(** Raised when an unknown task is encountered. *)
exception UnknownTask of task

(** Raised when an invalid task is encountered. *)
exception InvalidTask 

(** Raised when a category is not found. *)
exception CategoryNotFound of string


let categories = ref []

let access_cat () = !categories

let todays_date = 
  let time = Unix.localtime (Unix.time ()) in 
  let (day, month, year) = (time.tm_mday, time.tm_mon, time.tm_year) in
  string_of_int (month + 1) ^ "-" ^ string_of_int(day) ^ "-" ^ 
  string_of_int (1900 + year)

let init_task name due_date priority = {
  name = name;
  created_date = todays_date;
  due_date = due_date;
  (* sorting by priority to be implemented *)
  priority = priority;
}

let init_todolist name lst = {
  c_name = name;
  task_list = lst;
}

(** Initialize an empty to-do list *)
let empty_list cat_name = {
  c_name = cat_name;
  task_list = [];
}

let add_task t task = {
  c_name = t.c_name;
  task_list = task :: t.task_list
}

let add_new_cat new_cat =
  let old_cats = categories in 
  categories := (new_cat :: !old_cats)

let find_category cat_name =
  List.find (fun x -> x.c_name = cat_name) (!categories)

let remove_cat t (lst : t list) = 
  List.filter (fun x -> if x.c_name != t.c_name then true else false) lst

(* let rec sort_list_helper cat_lst = 
   match cat_lst with
   | [] -> acc
   | task :: lst -> if task.priority *)

(* two options for sorting...every time we create a task we can sort list or 
   insert it in a sorted order kinda lke a3..*)
let sort_list cat_name = 
  let cat = find_category cat_name in
  List.rev 
    (List.stable_sort compare (List.map (fun x -> x.priority) cat.task_list))

let create_task cat_name name due_date priority= 
  let task = init_task name due_date priority in
  try 
    let new_list = add_task (find_category cat_name) task in 
    let old_list = find_category cat_name in  
    categories := (new_list :: (remove_cat old_list !categories))
  with Not_found -> begin
      let new_cat = add_task (empty_list cat_name) task in
      add_new_cat new_cat
    end

let remove_task t task = failwith "to be implemented"

(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
let complete_task t task =
  let rem = remove_task t task in
  create_task "Completed" task.name task.due_date task.priority


(** [delete_task t task] is an updated to-do list with [task] removed from [t] 
    . *)
let delete_task t task =
  remove_task t task


