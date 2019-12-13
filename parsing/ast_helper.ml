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

  let case lhs ?guard rhs = {
      pc_lhs = lhs;
      pc_guard = guard;
      pc_rhs = rhs;
    }
end

module Str = struct
  let mk ?(loc = !default_loc) d = { pstr_desc = d; pstr_loc = loc }
end


module Vb = struct
  let mk ?(loc = !default_loc) pat expr = {
      pvb_pat = pat;
      pvb_expr = expr;
      pvb_loc = loc;
    }
end
