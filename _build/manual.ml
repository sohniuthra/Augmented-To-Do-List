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

let todays_date () = 
  let time = Unix.localtime (Unix.time ()) in 
  let (day, month, year) = (time.tm_mday, time.tm_mon, time.tm_year) in
  string_of_int (month + 1) ^ "/" ^ string_of_int(day) ^ "/" ^ 
  string_of_int (1900 + year)

let init_task name due_date priority = {
  name = name;
  created_date = todays_date ();
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
  let t1p = t1.priority in
  let t2p = t2.priority in
  if t1p < t2p then -1 else 
  if t1p > t2p then 1 else 0

(* helper function to get the month of a properly formatted due date *)
let get_month_due task = 
  let date = task.due_date in 
  int_of_string (String.sub date 0 2)

(* helper function to get the day of a properly formatted due date *)
let get_day_due task = 
  let date = task.due_date in 
  let month_ind = (String.index date '/') + 1 in
  int_of_string (String.sub date month_ind 2 )

(* helper function to get the year of a properly formatted due date *)
let get_year_due task = 
  let date = task.due_date in 
  let year_ind = (String.rindex date '/') + 1 in
  int_of_string (String.sub date year_ind 2)

(* compare function for two tasks based on due date *)
let date_compare t1 t2 = 
  let t1month = get_month_due t1 in 
  let t1day = get_day_due t1 in 
  let t1year = get_year_due t1 in 
  let t2month = get_month_due t2 in
  let t2day = get_day_due t2 in
  let t2year = get_year_due t2 in

  if t1year > t2year then 1 else if t1year < t2year then -1 else
    (if t1month > t2month then 1 else if t1month < t2month then -1 else
       (if t1day > t2day then 1 else if t1day < t2day then -1 else 0))

let sort_by_priority ?(cat=categories) cat_name = 
  let category = find_category ~cat:cat cat_name in
  let sorted_lst = List.stable_sort priority_compare category.task_list in
  let sorted_cat = init_todolist cat_name sorted_lst in
  cat := (sorted_cat :: (remove_cat category !cat))

let sort_by_date ?(cat=categories) cat_name = 
  let category = find_category ~cat:cat cat_name in
  let sorted_lst = List.stable_sort date_compare category.task_list in 
  let sorted_cat = init_todolist cat_name sorted_lst in
  cat := (sorted_cat :: (remove_cat category !cat))

let sort_list ?(cat=categories) cat_name sort_by = 
  if sort_by = "Priority" then sort_by_priority ~cat:cat cat_name
  else if sort_by = "Due Date" then sort_by_date ~cat:cat cat_name
  else failwith "Sorting not properly specified"

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
let rec remove_task_newlst tsklst task nlst = 
  match tsklst with
  | [] -> nlst 
  | h :: t -> if h = task then remove_task_newlst t task nlst 
    else remove_task_newlst t task (nlst @ [h])

let remove t task =
  {c_name = t.c_name; 
   task_list = remove_task_newlst t.task_list task []}

let find_task cat task_name = 
  List.find (fun x -> x.name = task_name) cat.task_list

(** [complete_task t task] is a updated completed to-do list [t] with [task]. *)
let complete_task ?(cat=categories) cat_name task_name =
  let category = find_category ~cat:cat cat_name in
  let task = find_task category task_name in
  try 
    let new_list = remove category task in 
    create_task ~cat:cat "Completed" task.name task.due_date task.priority;
    let old_list = category in 
    cat := (new_list :: (remove_cat old_list !cat))
  with Not_found -> begin
      failwith "task not found" 
    end

let delete_task ?(cat=categories) cat_name task_name =
  let old_cat = find_category ~cat:cat cat_name in 
  let task = find_task old_cat task_name in
  let new_list = remove_task_newlst old_cat.task_list task [] in 
  let new_cat = init_todolist cat_name new_list in
  cat := (new_cat :: (remove_cat old_cat !cat))

let rec to_list_helper cat_list acc =
  match cat_list with
  | [] -> acc
  | {name; created_date; due_date; priority} :: t -> 
    to_list_helper t 
      (acc @ [name; created_date; due_date; string_of_int priority])

let to_list ?(cat=categories) cat_name =
  let cat = find_category ~cat:cat cat_name in  
  [cat_name] @ (to_list_helper cat.task_list [])


