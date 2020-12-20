open Unix

(** 
   Implementation of a manual to-do list.

   This module implements the data stored in a to-do list, including
   data for each task.
*)

type task = {
  name : string;
  created_date : string;
  due_date : string;
  priority : int;
}

type t = {
  c_name : string;
  task_list : task list; 
}

(** Raised when an invalid task is encountered. *)
exception InvalidTask 

(** Raised when an unknown task is encountered. *)
exception TaskNotFound of string

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

let empty_list cat_name = {
  c_name = cat_name;
  task_list = [];
}

let add_task t task = {
  c_name = t.c_name;
  task_list = task :: t.task_list
}

(* helper function *)
let add_new_cat ?(cat=categories) new_cat =
  let old_cats = cat in 
  cat := (new_cat :: !old_cats)

(* helper function *)
let find_category ?(cat=categories) cat_name =
  List.find (fun x -> x.c_name = cat_name) (!cat)

(* helper function: does not actually remove the category from categories, 
   but instead returns a list of cateogories without category [t]*)
let remove_cat t lst = 
  List.filter (fun x -> if x.c_name != t.c_name then true else false) lst

(* helper function: compare function for two tasks based on priority *)
let priority_compare t1 t2 = 
  let t1p = t1.priority in
  let t2p = t2.priority in
  if t1p > t2p then 1 else begin
    if t1p > t2p then -1 else 0
  end

(* helper function: gets the month of a properly formatted due date *)
let get_month_due task = 
  let date = task.due_date in 
  int_of_string (String.sub date 0 2)

(* helper function: gets the day of a properly formatted due date *)
let get_day_due task = 
  let date = task.due_date in 
  let month_ind = (String.index date '/') + 1 in
  int_of_string (String.sub date month_ind 2 )

(* helper function: gets the year of a properly formatted due date *)
let get_year_due task = 
  let date = task.due_date in 
  let year_ind = (String.rindex date '/') + 1 in
  int_of_string (String.sub date year_ind 2)

let day_compare d1 d2 =
  if d1 > d2 then 1 else begin
    if d1 < d2 then -1 else 0
  end 
let month_compare m1 m2 =
  if m1 > m2 then 1 else begin
    if m1 < m2 then -1 else 0
  end 

let year_compare yr1 yr2 =
  if yr1 > yr2 then 1 else begin
    if yr1 < yr2 then -1 else 0
  end 

(* helper function: compare function for two tasks based on due date *)
let date_compare t1 t2 = 
  let t1month = get_month_due t1 in 
  let t1day = get_day_due t1 in 
  let t1year = get_year_due t1 in 
  let t2month = get_month_due t2 in
  let t2day = get_day_due t2 in
  let t2year = get_year_due t2 in

  let yr = year_compare t1year t2year in
  let mnth = month_compare t1month t2month in

  if yr = 1 || yr = -1 then yr else begin
    if mnth = 1 || mnth = -1 then mnth else day_compare t1day t2day
  end

(* helper function: employs sorting by priority *)
let sort_by_priority ?(cat=categories) cat_name = 
  let category = find_category ~cat:cat cat_name in
  let sorted_lst = List.stable_sort priority_compare category.task_list in
  let sorted_cat = init_todolist cat_name sorted_lst in
  cat := sorted_cat :: (remove_cat category !cat)

(* helper function: employs sorting by due date *)
let sort_by_date ?(cat=categories) cat_name = 
  let category = find_category ~cat:cat cat_name in
  let sorted_lst = List.stable_sort date_compare category.task_list in 
  let sorted_cat = init_todolist cat_name sorted_lst in
  cat := sorted_cat :: (remove_cat category !cat)

let sort_list ?(cat=categories) cat_name sort_by = 
  if sort_by = "Priority" then sort_by_priority ~cat:cat cat_name
  else begin 
    if sort_by = "Due Date" then sort_by_date ~cat:cat cat_name
    else failwith "Sorting not properly specified"
  end

let create_task ?(cat=categories) cat_name name due_date priority = 
  let task = init_task name due_date priority in
  try 
    let new_list = add_task (find_category ~cat:cat cat_name) task in 
    let old_list = find_category ~cat:cat cat_name in  
    cat := new_list :: (remove_cat old_list !cat)
  with Not_found -> begin
      let new_cat = add_task (empty_list cat_name) task in
      add_new_cat ~cat:cat new_cat
    end

(* helper function: returns a task with [task_name] in [cat]  *)
let find_task cat task_name = 
  List.find (fun x -> x.name = task_name) cat.task_list

(* helper function for remove *)
let rec remove_task_newlst tsklst task nlst = 
  match tsklst with
  | [] -> nlst 
  | h :: t -> begin if h = task then remove_task_newlst t task nlst 
      else remove_task_newlst t task (nlst @ [h]) end

(* helper function: initializes a new to-do list without [task] *)
let remove t task =
  let new_lst = remove_task_newlst t.task_list task [] in
  init_todolist t.c_name new_lst

let complete_task ?(cat=categories) cat_name task_name =
  try begin
    let category = find_category ~cat:cat cat_name in
    try 
      let task = find_task category task_name in
      let new_list = remove category task in 
      create_task ~cat:cat "Completed" task.name task.due_date task.priority;
      cat := new_list :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let delete_task ?(cat=categories) cat_name task_name =
  try begin
    let old_cat = find_category ~cat:cat cat_name in 
    try
      let task = find_task old_cat task_name in
      let new_cat = remove old_cat task in 
      cat := new_cat :: (remove_cat old_cat !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

(* helper function *)
let rec to_list_helper cat_list acc =
  match cat_list with
  | [] -> acc
  | {name; created_date; due_date; priority} :: t -> 
    to_list_helper t 
      (acc @ [name; created_date; due_date; string_of_int priority])

let to_list ?(cat=categories) cat_name =
  try
    let category = find_category ~cat:cat cat_name in  
    [cat_name] @ (to_list_helper category.task_list [])
  with Not_found -> raise (CategoryNotFound cat_name)

let change_name ?(cat=categories) cat_name task_name new_name =
  try begin
    let category = find_category ~cat:cat cat_name in 
    try
      let old_task = find_task category task_name in
      let due_date = old_task.due_date in
      let priority = old_task.priority in
      let removed_cat = remove category old_task in
      let new_task = init_task new_name due_date priority in
      let new_cat = add_task removed_cat new_task in
      cat := new_cat :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let change_due_date ?(cat=categories) cat_name task_name new_date =
  try begin
    let category = find_category ~cat:cat cat_name in 
    try
      let old_task = find_task category task_name in
      let priority = old_task.priority in
      let removed_cat = remove category old_task in
      let new_task = init_task task_name new_date priority in
      let new_cat = add_task removed_cat new_task in
      cat := new_cat :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let change_priority ?(cat=categories) cat_name task_name new_priority =
  try begin
    let category = find_category ~cat:cat cat_name in 
    try
      let old_task = find_task category task_name in
      let old_date = old_task.due_date in
      let removed_cat = remove category old_task in
      let new_task = init_task task_name old_date new_priority in
      let new_cat = add_task removed_cat new_task in
      cat := new_cat :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

