(** Module struct of Appointments sig
    Used to create appointments as part of a larger suite of planning *)
open Manual

module AppSuite = struct

  type app = {
    title : string;
    app_date : string;
    location : string;
    notes : string;
  }

  type t = unit

  let add_app = failwith "rose's unimplementecd"

  let complete_app = failwith "rose's unimplemented"

  let delete_app = failwith "rose's unimplemented"

  let add_app_info = failwith "rose's unimplemented"

  let add_location = failwith "rose's unimplemented"
end 
