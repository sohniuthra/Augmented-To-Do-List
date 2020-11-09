(** 
   Representation of a automatic to-do list.

   This module represents the data stored in an automatic to-do list, 
   where each task is predetermined, including
   data for each task.

   EVERYTHING NOT LISTED IN THE SIG THE USER DOES NOT NEED
   AND THEREFORE SHOULD ONLY BE SHOWN IN THE STRUCT!!
*)

module type Automatic = sig

  (** The abstract type representing an automatic to-do list. 
      Tasks should be inserted without user input*)
  type t 

  (** The abstract type representing one task of the automatic to-do list *)
  type task

  (** Raised when an unknown task is encountered. *)
  exception UnknownTask of task

  (** Function for user to instantiate the to-do list with tasks already 
      inserted
      NOTE TO BE DELETED: the type is unit to unit because
      the user should not need to put in any information and
      the make_auto should change the categories variable
      as designed similarly to manual *)
  val make_auto : unit -> unit


  (** Funtion to access the automatic list for user to be able to view *)
  val access_cat : unit -> t list

  (** Function for user to be able to complete task and put it in a
      separate complete tasks list that should've already been 
      made also. 
      Invariant: completed list clears if it passes a 
      certain number of elements. *)
  val complete_task : t -> task -> unit

  (** Function for user to be able to delete task out of 
      automatic list.  Also used to delete task out of 
      completed list when completed list becomes too large. *)
  val delete_task : t -> task -> t 

  (** [create_task cat_name name due_date priority] updates the to-do
      list with name [cat_name] with the new task [name due_date priority]. 
      If category name [cat_name] does not already exist, a new category with 
      [cat_name] is created with task [name created_date due_date priority] 
      in the task list. *)
  val create_task : string -> string -> string -> int -> unit 

  val change_priority : string -> string -> int ->  unit

  val change_due : string -> string -> string ->  unit

end
(** Module representation of a to-do list with  *)

module CarList : Automatic = struct

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

  let categories = ref []

  exception UnknownTask of task

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

  let car_list () = 
    let init_list = empty_list "Car Tasks" in 
    let change_oil = add_task init_list (init_task "Change Oil" "TBD" 1) in 
    let change_steering = add_task change_oil (init_task "Change Steering Fluid" "TBD" 2) in 
    let emissions = add_task change_steering (init_task "Go for Emissions Test" "TBD" 3) in
    let car_wash = add_task emissions (init_task "Go to Car Wash" "TBD" 4) in
    let brakes = add_task car_wash (init_task "Change Brake Pads" "TBD" 5) in
    let battery = add_task brakes (init_task "Change battery" "TBD" 6) in 
    let radiator = add_task battery (init_task "Change radiator fluid" "TBD" 7) in
    add_new_cat radiator; add_new_cat (empty_list "Completed Tasks")

  let school_list () =
    let init_list = empty_list "School Tasks" in 
    let math_pset = add_task init_list (init_task "Complete Math Problem Set" "TBD" 1) in 
    let write_essay = add_task math_pset(init_task "Write Essay" "TBD" 2) in 
    let biology_lab = add_task write_essay (init_task "Finish Biology Lab" "TBD" 3) in
    let meet_professor = add_task biology_lab (init_task "Meet with Professor" "TBD" 4) in
    let omm = add_task meet_professor (init_task "Fill out OMM" "TBD" 5) in
    let lecture = add_task omm (init_task "Watch CS 3110 Lecture Videos" "TBD" 6) in 
    let plan = add_task lecture (init_task "Plan for Pre-Enroll" "TBD" 7) in
    add_new_cat plan

  let household_list () = 
    let init_list = empty_list "Household Tasks" in 
    let mop_floor = add_task init_list (init_task "Mop Kitchen Floor" "TBD" 1) in 
    let vacuum = add_task mop_floor (init_task "Vacuum the Rugs" "TBD" 2) in 
    let dishes = add_task vacuum (init_task "Wash the Dishes" "TBD" 3) in
    let cook = add_task dishes (init_task "Cook Dinner" "TBD" 4) in
    let mow = add_task cook (init_task "Mow the Lawn" "TBD" 5) in
    let dog = add_task mow (init_task "Feed the Dog" "TBD" 6) in 
    let laundry = add_task dog (init_task "Do the Laundry" "TBD" 7) in
    add_new_cat laundry

  let shopping_list () = 
    let init_list = empty_list "Shopping Tasks" in 
    let groceries = add_task init_list (init_task "Order Groceries" "TBD" 1) in 
    let cake = add_task groceries (init_task "Buy Cake for Melissa's Birthday" "TBD" 2) in 
    let dress = add_task cake (init_task "Find Dress for Formal" "TBD" 3) in
    let winter = add_task dress (init_task "Buy Essentials for Ithaca Winter" "TBD" 4) in
    let shoes = add_task winter (init_task "Buy new Nike Sneakers" "TBD" 5) in
    let lightbulbs = add_task shoes (init_task "Get new lightbulbs to replace current ones" "TBD" 6) in 
    let phone = add_task lightbulbs (init_task "Get New iPhone 12 Pro Max" "TBD" 7) in
    add_new_cat phone


  let pandemic_list () = 
    let init_list = empty_list "Pandemic Tasks" in 
    let sanitizer = add_task init_list (init_task "Buy Hand Sanitizer" "TBD" 1) in 
    let spray = add_task sanitizer (init_task "Buy Disinfectant Spray" "TBD" 2) in 
    let wipe = add_task spray (init_task "Wipe down surfaces" "TBD" 3) in
    let masks = add_task wipe (init_task "Make Masks" "TBD" 4) in
    let tests = add_task masks (init_task "Get tested" "TBD" 5) in
    let daily_check = add_task tests (init_task "Complete Daily Check" "TBD" 6) in 
    let flight = add_task daily_check (init_task "Cancel flights" "TBD" 7) in
    add_new_cat flight

  let make_auto () =
    car_list (); 
    school_list (); 
    household_list (); 
    shopping_list (); 
    pandemic_list ()

  let access_cat () = !categories

  let delete_task t task =
    let new_task_list = List.filter (fun x -> x.name <> task.name) t.task_list in 
    {c_name = t.c_name; task_list = new_task_list}

  let complete_task t task =
    let new_completed = add_task (find_category "Completed Tasks") task in 
    let new_auto = delete_task t task in 
    categories := new_auto :: new_completed :: []

  let create_task name cat_name due_date priority= 
    let task = init_task name due_date priority in
    let new_list = add_task (find_category cat_name) task in 
    let old_list = find_category cat_name in  
    categories := (new_list :: (remove_cat old_list !categories))

  let change_priority cat_name task_name new_priority =
    let cat = find_category cat_name in
    let old_task =  List.find (fun x -> x.name = task_name) cat.task_list in
    let date = old_task.due_date in 
    let new_t = delete_task cat old_task in
    categories := (new_t :: (remove_cat cat !categories));
    create_task cat_name task_name date new_priority

  let change_due cat_name task_name new_date =
    let cat= find_category cat_name in
    let old_task =  List.find (fun x -> x.name = task_name) cat.task_list in
    let priority = old_task.priority in 
    let new_t = delete_task cat old_task in
    categories := (new_t :: (remove_cat cat !categories));
    create_task cat_name task_name new_date priority

end