(* open Asttypes *)
open Ast

let default_loc = ref Location.none

module Pat = struct
  let mk ?(loc = !default_loc) d = {
      ppat_desc = d;
      ppat_loc = loc;}
end


module Exp = struct
  let mk ?(loc = !default_loc) d = {
      pexp_desc = d;
      pexp_loc = loc;}
end

module Str = struct
  let mk ?(loc = !default_loc) d = { pstr_desc = d; pstr_loc = loc }
end
