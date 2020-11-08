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
  val create_task : string -> string -> int -> unit 

  val change_priority : t -> task -> int ->  unit

  val change_due : t -> task -> string -> unit

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


  let make_auto () =
    let init_list = empty_list "Car Tasks" in 
    let change_oil = add_task init_list (init_task "Change Oil" "TBD" 1) in 
    let change_steering = add_task change_oil (init_task "Change Steering Fluid" "TBD" 2) in 
    let emissions = add_task change_steering (init_task "Go for Emissions Test" "TBD" 3) in
    let car_wash = add_task emissions (init_task "Go to Car Wash" "TBD" 4) in
    let brakes = add_task car_wash (init_task "Change Brake Pads" "TBD" 5) in
    let battery = add_task brakes (init_task "Change battery" "TBD" 6) in 
    let radiator = add_task battery (init_task "Change radiator fluid" "TBD" 7) in
    add_new_cat radiator; add_new_cat (empty_list "Completed Tasks")

  let access_cat () = !categories


  let delete_task t task =
    let new_task_list = List.filter (fun x -> x.name <> task.name) t.task_list in 
    {c_name = t.c_name; task_list = new_task_list}

  let complete_task t task =
    let new_completed = add_task (find_category "Completed Tasks") task in 
    let new_auto = delete_task t task in 
    categories := new_auto :: new_completed :: []

  let create_task name due_date priority= 
    let task = init_task name due_date priority in
    let new_list = add_task (find_category "Car Tasks") task in 
    let old_list = find_category "Car Tasks" in  
    categories := (new_list :: (remove_cat old_list !categories))

  let change_priority t task int =
    failwith "rose's unimplemented"

  let change_due =
    failwith "rose's unimplemented"


end