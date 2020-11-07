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
  val complete_task : t -> task -> t

  (** Function for user to be able to delete task out of 
      automatic list.  Also used to delete task out of 
      completed list when completed list becomes too large. *)
  val delete_task : t -> task -> t 

end

(** Module representation of a to-do list with  *)

module CarList : Automatic = struct


  type t = unit

  type task = unit

  exception UnknownTask of task

  let make_auto =
    failwith "rose's unimplemented <3"

  let access_cat =
    failwith "rose's unimplemented"

  let complete_task =
    failwith "rose's unimplemented"

  let delete_task =
    failwith "rose's unimplemented"



end