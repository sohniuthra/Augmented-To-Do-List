(** Module struct of Appointments sig
    Used to create appointments as part of a larger suite of planning *)
open Manual

module AppSuite = struct

  type app = {
    title : string;
    app_date : string;
    time : string;
    location : string;
    notes : string;
  }

  type t = app list

  let appointments = ref []

  let empty_appo () = ref []

  let access_app ?(appo=appointments) () = !appo

  let delete_app ?(appo=appointments) app =
    let new_list = List.filter (fun x -> x <> app) (!appo) in 
    appo := new_list

  let add_app ?(appo=appointments) title date time = 
    let new_app = {title = title; app_date = date; time = time ;
                   location = "Location"; notes = "Notes about Appointment"} in 
    appo := new_app :: (!appo)


  let find_app appo app_title =
    List.find (fun x -> x.title = app_title) appo


  let complete_app ?(appo=appointments) app_title = 
    let app = find_app (!appo) app_title in
    delete_app ~appo:appo app

  let add_app_info ?(appo=appointments) app_title info = 
    let app = find_app (!appo) app_title in 
    let new_app = {title = app.title; app_date = app.date; time = app.time;
                   location = app.location; notes = info} in 
    delete_app appo app; (appo := new_app :: (!appo))


  let add_location ?(appo=appointments) app_title loc = 
    let app = find_app (!appo) app_title in 
    let new_app = {title = app.title; app_date = app.date; time = app.time;
                   location = loc; notes = app.notes} in 
    delete_app appo app; (appo := new_app :: (!appo))

  let find_date_and_time () =
    Unix.localtime (Unix.time)

  let rec to_list_app ?(appo=appointments) acc =
    match (!appo) with
    | [] -> acc
    | {title; app_date; time; location; notes} :: t -> 
      to_list_helper t 
        (acc @ [title; app_date; time; location; notes])
end 
