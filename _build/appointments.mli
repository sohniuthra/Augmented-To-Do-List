(** 
   Representation of an appointment book.

   This module represents the data stored in a digital appointment book,
   including the data for each individual appointment.
*)

(** [app] is the type that represents a singular appointment, including
    all of the data that is associated with the appointment.*)
type app

(** [empty_appo ()] creates an empty appointment book.  Upon creation,
    there is no appointments or data whatsoever in the appointment book. *)
val empty_appo : unit -> app list ref 

(** [access_app ()] unlocks and opens the appointment book so all 
    appointments and data associated with them are visible.*)
val access_app : ?appo:(app list ref) -> unit -> app list

(** [delete_app t] finds the appointment with the name [t]
    and deletes that appointment from the appointment book. *)
val delete_app : ?appo:(app list ref) -> string -> unit

(** [add_app t d i] adds an appointment to the digital appointment book. 
    For an appointment to be added, a name [t], date [d], and time [i] for the 
    appointment is required. *)
val add_app : ?appo:(app list ref) -> string -> string -> string -> unit

(** [complete_app t] completes an appointment with name [t] by 
    deleting the appointment from the appointment book. *)
val complete_app : ?appo:(app list ref) -> string -> unit

(** [add_app_info t i] adds information [i] to the data of the appointment with
    name [t]. *)
val add_app_info : ?appo:(app list ref) -> string -> string -> unit

(** [add_location t l] adds the location [l] of the appointment with name
    [t] to the data of the appointment with name [t]. *) 
val add_location : ?appo:(app list ref) -> string -> string -> unit

(** [to_list_app a] creates a list representation with each appointment name 
    and all the data associated with each appointment. *)
val to_list_app : ?appo:(app list ref) -> string list -> string list

(** [empty_finder ()] creates a pointer to assist in finding the specific
    appointment a user wants. *)
val empty_finder : unit -> app list ref 

(** [find_app_user t] finds the appointment with name [t] and its associated 
    data.*)
val find_app_user : ?one:(app list ref) -> ?appo:(app list ref) -> 
  string -> unit

(** [to_list_find l] creates a list representation with the specific 
    user-desired appointment from function [find_app_user] and the data 
    associated with it. [l] is the empty list the answer is
    built from.*)
val to_list_find : ?one:(app list ref) -> string list -> string list

(** [to_list_alt ()] is a to_list function that works for the gui. *)
val to_list_alt : ?appo:(app list ref) -> unit -> string list