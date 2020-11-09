(** A module that lets the user create appointments 
    this will have a better representation when a GUI is created *)

module type Appointment = sig 

  type app

  type t 

  val add_app

  val complete_app

  val delete_app

  val add_app_info

  val add_location
end 