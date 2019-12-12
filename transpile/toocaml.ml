open Core
open Ast

let rec str_of_structure strs =
  String.concat ~sep:"\n"
  @@ List.map ~f:str_of_structure_item strs

and str_of_structure_item stri =
  match stri.pstr_desc with
  | Pstr_value _ -> "unimplemented"
  | Pstr_act _ -> "unimplemented"
  | Pstr_defctx (id, ctx) ->
     Printf.sprintf "module %s = %s" id.txt (str_of_context ctx)

and str_of_context = function
  | Pctx_ident id -> Longident.str_of id.txt
