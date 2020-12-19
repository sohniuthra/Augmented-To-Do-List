(** Module struct of Appointments sig
    Used to create appointments as part of a larger suite of planning *)
open Manual


type app = {
  title : string;
  app_date : string;
  time : string;
  location : string;
  notes : string;
}


let appointments = ref []

let one_app = ref []

let empty_appo () = ref []

let empty_finder () = ref []

let access_app ?(appo=appointments) () = !appo

let delete_app ?(appo=appointments) app_title =
  let new_list = List.filter (fun x -> x.title <> app_title) (!appo) in 
  appo := new_list

let delete_app_alt ?(one=one_app) app_title =
  let new_list = List.filter (fun x -> x.title <> app_title) (!one) in 
  one := new_list

let add_app ?(appo=appointments) title date time = 
  let new_app = {title = title; app_date = date; time = time ;
                 location = "Location"; notes = "Notes about Appointment"} in 
  appo := new_app :: (!appo)


(* HELPER FUNCTION TO OTHER THINGS *)
let find_app appo app_title =
  List.find (fun x -> x.title = app_title) appo

let find_app_user ?(one=one_app) ?(appo=appointments) app_title =
  let correct_app = List.find (fun x -> x.title = app_title) (!appo) in 
  one := correct_app :: (!one)



let complete_app ?(appo=appointments) app_title = 
  let app = find_app (!appo) app_title in
  delete_app ~appo:appo app.title

let add_app_info ?(appo=appointments) app_title info = 
  let app = find_app (!appo) app_title in 
  let new_app = {title = app.title; app_date = app.app_date; time = app.time;
                 location = app.location; notes = info} in 
  delete_app ~appo:appo app.title; (appo := new_app :: (!appo))


let add_location ?(appo=appointments) app_title loc = 
  let app = find_app (!appo) app_title in 
  let new_app = {title = app.title; app_date = app.app_date; time = app.time;
                 location = loc; notes = app.notes} in 
  delete_app ~appo:appo app.title; (appo := new_app :: (!appo))

let find_date_and_time () =
  Unix.localtime (Unix.time ())

let rec to_list_app ?(appo=appointments) acc =
  match (!appo) with
  | [] -> acc
  | {title; app_date; time; location; notes} :: t -> 
    delete_app ~appo:appo title;
    to_list_app ~appo:appo
      (acc @ [title; app_date; time; location; notes])

let rec to_list_find ?(one=one_app) acc =
  match (!one) with
  | [] -> acc
  | {title; app_date; time; location; notes} :: t -> 
    delete_app_alt ~one:one title;
    to_list_find ~one:one 
      (acc @ [title; app_date; time; location; notes])

let rec to_list_helper app_lst acc =
  match app_lst with 
  | [] -> acc 
  | {title; app_date; time; location; notes} :: t -> 
    to_list_helper t (acc @ [title; app_date; time; location; notes])

let to_list_alt ?(appo=appointments) () = 
  to_list_helper !appo []