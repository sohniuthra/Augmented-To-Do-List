(** A module that lets the user create appointments 
    this will have a better representation when a GUI is created *)

module type Appointment = sig 

  type app

  type t 

  val add_app : unit -> unit

  val complete_app : unit -> unit

  val delete_app : unit -> unit

  val add_app_info : unit -> unit

  val add_location : unit -> unit
end 