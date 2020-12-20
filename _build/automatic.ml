(** 
   Implementation of an automatic to-do list.

   This module implements the data stored in an automatic to-do list, 
   where each task is predetermined, including data for each task (the user
   does have an option to add additional tasks, though).
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

let empty_cat_auto () = ref []

let categories = ref []

let access_cat ?(cat=categories) () = !cat

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

(** Initialize an empty category. *)
let empty_list cat_name = {
  c_name = cat_name;
  task_list = [];
}

let add_task t task = {
  c_name = t.c_name;
  task_list = task :: t.task_list
}

let add_new_cat ?(cat=categories) new_cat =
  cat := (new_cat :: !cat)

let find_category ?(cat=categories) cat_name =
  List.find (fun x -> x.c_name = cat_name) (!cat)

let remove_cat t (lst : t list) = 
  List.filter (fun x -> if x.c_name != t.c_name then true else false) lst

let completed_list ?(cat=categories) () =
  add_new_cat ~cat:cat (empty_list "Completed Tasks")

let car_list ?(cat=categories) () = 
  let init_list = empty_list "Car Tasks" in 
  let change_oil = add_task init_list 
      (init_task "Change Oil" "TBD" 1) in 
  let change_steering = add_task change_oil 
      (init_task "Change Steering Fluid" "TBD" 2) in 
  let emissions = add_task change_steering 
      (init_task "Go for Emissions Test" "TBD" 3) in
  let car_wash = add_task emissions (init_task "Go to Car Wash" "TBD" 4) in
  let brakes = add_task car_wash (init_task "Change Brake Pads" "TBD" 5) in
  let battery = add_task brakes (init_task "Change Battery" "TBD" 6) in 
  let radiator = add_task battery 
      (init_task "Change Radiator Fluid" "TBD" 7) in
  add_new_cat ~cat:cat radiator

let school_list ?(cat=categories) () =
  let init_list = empty_list "School Tasks" in 
  let math_pset = add_task init_list 
      (init_task "Complete Math Problem Set" "TBD" 1) in 
  let write_essay = add_task math_pset
      (init_task "Write Essay" "TBD" 2) in 
  let biology_lab = add_task write_essay 
      (init_task "Finish Biology Lab" "TBD" 3) in
  let meet_professor = add_task biology_lab 
      (init_task "Meet with Professor" "TBD" 4) in
  let omm = add_task meet_professor 
      (init_task "Fill Out OMM" "TBD" 5) in
  let lecture = add_task omm 
      (init_task "Watch CS 3110 Lecture Videos" "TBD" 6) in 
  let plan = add_task lecture 
      (init_task "Plan for Pre-Enroll" "TBD" 7) in
  add_new_cat ~cat:cat plan

let household_list ?(cat=categories)() = 
  let init_list = empty_list "Household Tasks" in 
  let mop_floor = add_task init_list 
      (init_task "Mop Kitchen Floor" "TBD" 1) in 
  let vacuum = add_task mop_floor (init_task "Vacuum the Rugs" "TBD" 2) in 
  let dishes = add_task vacuum (init_task "Wash the Dishes" "TBD" 3) in
  let cook = add_task dishes (init_task "Cook Dinner" "TBD" 4) in
  let mow = add_task cook (init_task "Mow the Lawn" "TBD" 5) in
  let dog = add_task mow (init_task "Feed the Dog" "TBD" 6) in 
  let laundry = add_task dog (init_task "Do the Laundry" "TBD" 7) in
  add_new_cat ~cat:cat laundry

let shopping_list ?(cat=categories) () = 
  let init_list = empty_list "Shopping Tasks" in 
  let groceries = add_task init_list 
      (init_task "Order Groceries" "TBD" 1) in 
  let cake = add_task groceries 
      (init_task "Buy Cake for Melissa's Birthday" "TBD" 2) in 
  let dress = add_task cake 
      (init_task "Find Dress for Formal" "TBD" 3) in
  let winter = add_task dress 
      (init_task "Buy Essentials for Ithaca Winter" "TBD" 4) in
  let shoes = add_task winter 
      (init_task "Buy new Nike Sneakers" "TBD" 5) in
  let lightbulbs = add_task shoes 
      (init_task "Get new lightbulbs to replace current ones" "TBD" 6) in 
  let phone = add_task lightbulbs 
      (init_task "Get New iPhone 12 Pro Max" "TBD" 7) in
  add_new_cat ~cat:cat phone

let pandemic_list ?(cat=categories) () = 
  let init_list = empty_list "Pandemic Tasks" in 
  let sanitizer = add_task init_list (init_task "Buy Hand Sanitizer" "TBD" 1) 
  in 
  let spray = add_task sanitizer (init_task "Buy Disinfectant Spray" "TBD" 2) 
  in 
  let wipe = add_task spray (init_task "Wipe Down Surfaces" "TBD" 3) in
  let masks = add_task wipe (init_task "Make Masks" "TBD" 4) in
  let tests = add_task masks (init_task "Get Tested" "TBD" 5) in
  let daily_check = add_task tests (init_task "Complete Daily Check" "TBD" 6) 
  in 
  let flight = add_task daily_check (init_task "Cancel Flights" "TBD" 7) in
  add_new_cat ~cat:cat flight

let make_auto ?(cat=categories) () =
  car_list ~cat:cat (); 
  school_list ~cat:cat (); 
  household_list ~cat:cat (); 
  shopping_list ~cat:cat (); 
  pandemic_list ~cat:cat ();
  completed_list ~cat:cat ()

let make_car_auto ?(cat=categories) () = 
  car_list ~cat:cat () 

let make_school_auto ?(cat=categories) () =
  school_list ~cat:cat ()

let make_household_auto ?(cat=categories) () =
  household_list ~cat:cat ()

let make_shopping_auto ?(cat=categories) () =
  shopping_list ~cat:cat ()

let make_pandemic_auto ?(cat=categories) () =
  pandemic_list ~cat:cat ()

let make_completed_auto ?(cat=categories) () =
  completed_list ~cat:cat ()

let del_task t task =
  let new_task_list = List.filter (fun x -> x.name <> task.name) t.task_list in
  {c_name = t.c_name; task_list = new_task_list}

let find_task cat task_name =
  List.find (fun x -> x.name = task_name) cat.task_list

let delete_task_auto ?(cat=categories) cat_name task_name =
  try begin
    let old_cat = find_category ~cat:cat cat_name in 
    try 
      let task = find_task old_cat task_name in
      let new_cat = del_task old_cat task in 
      cat := new_cat :: (remove_cat old_cat !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let create_task_auto ?(cat=categories) cat_name task_name due_date priority= 
  let task = init_task task_name due_date priority in
  try 
    let old_list = find_category ~cat:cat cat_name in  
    let new_list = add_task (old_list) task in
    cat := new_list :: (remove_cat old_list !cat)
  with Not_found -> raise (CategoryNotFound cat_name)

let complete_task_auto ?(cat=categories) cat_name task_name = 
  try begin
    let category = find_category ~cat:cat cat_name in
    try 
      let task = find_task category task_name in
      let new_list = del_task category task in 
      create_task_auto ~cat:cat "Completed Tasks" task.name task.due_date 
        task.priority;
      cat := new_list :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let change_priority_auto ?(cat=categories) cat_name task_name new_priority =
  try begin
    let category = find_category ~cat:cat cat_name in
    try
      let old_task =  find_task category task_name in
      let date = old_task.due_date in 
      let new_t = del_task category old_task in
      let new_task = init_task task_name date new_priority in
      let new_cat = add_task new_t new_task in
      cat := new_cat :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let change_due_auto ?(cat=categories) cat_name task_name new_date =
  try begin
    let category = find_category ~cat:cat cat_name in
    try
      let old_task =  find_task category task_name in
      let priority = old_task.priority in 
      let new_t = del_task category old_task in
      let new_task = init_task task_name new_date priority in
      let new_cat = add_task new_t new_task in
      cat := new_cat :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let change_name_auto ?(cat=categories) cat_name task_name new_name =
  try begin
    let category = find_category ~cat:cat cat_name in
    try
      let old_task =  find_task category task_name in
      let priority = old_task.priority in 
      let date = old_task.due_date in
      let new_t = del_task category old_task in
      let new_task = init_task new_name date priority in
      let new_cat = add_task new_t new_task in
      cat := new_cat :: (remove_cat category !cat)
    with Not_found -> raise (TaskNotFound task_name)
  end
  with Not_found -> raise (CategoryNotFound cat_name)

let delete_cat_auto ?(cat=categories) cat_name =
  try
    cat := List.filter (fun x -> x.c_name <> cat_name) !cat
  with Not_found -> raise (CategoryNotFound cat_name)

let reset_car ?(cat=categories) () = 
  delete_cat_auto ~cat:cat "Car Tasks"; 
  make_car_auto ~cat:cat ()

let reset_school ?(cat=categories) () = 
  delete_cat_auto ~cat:cat "School Tasks"; 
  make_school_auto ~cat:cat ()

let reset_household ?(cat=categories) () = 
  delete_cat_auto ~cat:cat "Household Tasks";
  make_household_auto ~cat:cat ()

let reset_shopping ?(cat=categories) () = 
  delete_cat_auto ~cat:cat "Shopping Tasks";
  make_shopping_auto ~cat:cat ()

let reset_pandemic ?(cat=categories) () = 
  delete_cat_auto ~cat:cat "Pandemic Tasks"; 
  make_pandemic_auto ~cat:cat ()

let reset_completed ?(cat=categories) () =
  delete_cat_auto ~cat:cat "Completed Tasks"; 
  make_completed_auto ~cat:cat ()

let reset_all_cat ?(cat=categories) () = 
  delete_cat_auto ~cat:cat "Car Tasks"; 
  delete_cat_auto ~cat:cat "School Tasks"; 
  delete_cat_auto ~cat:cat "Household Tasks"; 
  delete_cat_auto ~cat:cat "Shopping Tasks"; 
  delete_cat_auto ~cat:cat "Pandemic Tasks"; 
  delete_cat_auto ~cat:cat "Completed Tasks"; 
  make_auto ~cat:cat ()

let rec to_list_helper cat_list acc =
  match cat_list with
  | [] -> acc
  | {name; created_date; due_date; priority} :: t -> 
    to_list_helper t 
      (acc @ [name; created_date; due_date; string_of_int priority])

let to_list_auto ?(cat=categories) cat_name =
  try
    let cat = find_category ~cat:cat cat_name in  
    [cat_name] @ (to_list_helper cat.task_list [])
  with Not_found -> raise (CategoryNotFound cat_name)