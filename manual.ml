open Unix
(* open Graphics *)
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

let access_cat ?(cat=categories) () = !cat

let empty_cat () = ref []

let todays_date = 
  let time = Unix.localtime (Unix.time ()) in 
  let (day, month, year) = (time.tm_mday, time.tm_mon, time.tm_year) in
  string_of_int (month + 1) ^ "/" ^ string_of_int(day) ^ "/" ^ 
  string_of_int (1900 + year)

let init_task name due_date priority = {
  name = name;
  created_date = todays_date;
  due_date = due_date;
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

let add_new_cat ?(cat=categories) new_cat =
  let old_cats = cat in 
  cat := (new_cat :: !old_cats)

let find_category ?(cat=categories) cat_name =
  List.find (fun x -> x.c_name = cat_name) (!cat)

let remove_cat t (lst : t list) = 
  List.filter (fun x -> if x.c_name != t.c_name then true else false) lst

(* compare function for two tasks based on priority *)
let priority_compare t1 t2 = 
  if t1.priority < t2.priority then -1 else 
  if t1.priority > t2.priority then 1 else 0

let rec sort_list_helper task_lst acc =
  match task_lst with
  | [] -> []
  | task :: [] -> acc @ [task]
  | task1 :: task2 :: lst -> begin if priority_compare task1 task2 = -1
      then acc @ [task2] @ [task1]
      else if priority_compare task1 task2 = 1 
      then acc @ [task1] @ [task2] else sort_list_helper (task2 :: lst) acc
    end

(* two options for sorting...every time we create a task we can sort list or 
   insert it in a sorted order kinda lke a3..
   also which way do we want priority - 3,2,1, or 1,2,3*)
let sort_list ?(cat=categories) cat_name = 
  let category = find_category ~cat:cat cat_name in
  let sorted_lst = List.stable_sort priority_compare category.task_list in
  let sorted_cat = init_todolist cat_name sorted_lst in
  cat := (sorted_cat :: (remove_cat category !cat))

let create_task ?(cat=categories) cat_name name due_date priority = 
  let task = init_task name due_date priority in
  try 
    let new_list = add_task (find_category ~cat:cat cat_name) task in 
    let old_list = find_category ~cat:cat cat_name in  
    cat := (new_list :: (remove_cat old_list !cat))
  with Not_found -> begin
      let new_cat = add_task (empty_list cat_name) task in
      add_new_cat ~cat:cat new_cat
    end

(** [remove_task tsklst task nlst] returns a new task list without [task] *)
let rec remove_task tsklst task nlst = 
  match tsklst with
  | [] -> nlst 
  | h::t -> if h = task then remove_task t task nlst 
    else remove_task t task (h::nlst)

(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
let complete_task t task =
  (*let rem = remove_task t.task_list task [] in
    let created = create_task "Completed" task.name task.due_date task.priority in
    find_category "Completed"*)
  let rem = remove_task t.task_list task [] in
  create_task "Completed" task.name task.due_date task.priority; 
  find_category "Completed"
(* failwith "Unimplemented - have to fix variable error" *)


(** [delete_task t task] is an updated to-do list with [task] removed from 
    [t]. *)
let delete_task t task =
  let remlst = remove_task t.task_list task [] in
  {c_name = t.c_name; task_list = remlst}

let rec to_list_helper cat_list acc =
  match cat_list with
  | [] -> acc
  | {name; created_date; due_date; priority} :: t -> 
    to_list_helper t 
      (acc @ [name; created_date; due_date; string_of_int priority])

let to_list ?(cat=categories) cat_name =
  let cat = find_category ~cat:cat cat_name in  
  [cat_name] @ (to_list_helper cat.task_list [])


