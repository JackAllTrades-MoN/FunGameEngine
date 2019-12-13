open Core
open Ast

let rec str_of_structure strs =
  String.concat ~sep:"\n"
  @@ List.map ~f:str_of_structure_item strs

and str_of_structure_item stri =
  match stri.pstr_desc with
  | Pstr_value (rec_flag, vbs) -> str_of_str_value (rec_flag, vbs)
  | Pstr_act _ -> "unimplemented"
  | Pstr_defctx (id, ctx) ->
     Printf.sprintf "module %s = %s" id.txt (str_of_context ctx)

and str_of_context = function
  | Pctx_ident id -> Longident.str_of id.txt


and str_of_str_value (rec_flag, value_bindings) =
  "let " ^ (str_of_rec_flag rec_flag) ^
    (String.concat ~sep:","
     @@ List.map ~f:str_of_value_binding value_bindings)

and str_of_value_binding vb =
  (str_of_pattern vb.pvb_pat) ^ "=" ^ (str_of_expression vb.pvb_expr)

and str_of_pattern pat =
  match pat.ppat_desc with
  | Ppat_any -> "_"
  | Ppat_constant constant -> str_of_constant constant
  | Ppat_var v -> v.txt

and str_of_constant = function
  | Pconst_integer (num, _dig) -> num
  | Pconst_char c -> Char.to_string c
  | Pconst_string (s, _, _) -> s
  | Pconst_float (num, _) -> num

and str_of_expression exp =
  match exp.pexp_desc with
  | Pexp_ident _lid -> "unimplemented"
  | _ -> "unimplemented"

and str_of_rec_flag = function
  | Asttypes.Recursive -> "rec"
  | Asttypes.Nonrecursive -> ""
