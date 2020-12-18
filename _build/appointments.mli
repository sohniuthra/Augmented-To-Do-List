(** A module that lets the user create appointments 
    this will have a better representation when a GUI is created *)


type app

val empty_appo : unit -> app list ref 

val access_app : ?appo:(app list ref) -> unit -> app list

val delete_app : ?appo:(app list ref) -> string -> unit

val add_app : ?appo:(app list ref) -> string -> string -> string -> unit

val complete_app : ?appo:(app list ref) -> string -> unit

val add_app_info : ?appo:(app list ref) -> string -> string -> unit

val add_location : ?appo:(app list ref) -> string -> string -> unit

val to_list_app : ?appo:(app list ref) -> string list -> string list