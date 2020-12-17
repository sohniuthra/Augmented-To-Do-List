(** A module that lets the user create appointments 
    this will have a better representation when a GUI is created *)

module type Appointment = sig 

  type app

  type t 

  val access_app : ?cat:(t list ref) -> unit -> t list

  val delete_app : ?cat:(t list ref) -> t -> unit

  val add_app : ?cat:(t list ref) -> string -> string -> string -> unit

  val complete_app : ?cat:(t list ref) -> string -> unit

  val add_app_info : ?cat:(t list ref) -> string -> string -> unit

  val add_location : ?cat:(t list ref) -> string -> string -> unit
end 