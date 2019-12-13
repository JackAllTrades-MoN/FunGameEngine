open Asttypes

(*
type top_level =
  | DefContext of string loc * context
  | DefLet of rec_flag * value_binding list * expression
  | DefAction of Longident.t * string loc * action_binding list *)

type structure = structure_item list

and structure_item =
  {
    pstr_desc : structure_item_desc;
    pstr_loc  : Location.t;
  }

and structure_item_desc =
  | Pstr_value of rec_flag * value_binding list
  | Pstr_act of Longident.t * string loc * action_binding list
  | Pstr_defctx of string loc * context

and context =
  | Pctx_ident of Longident.t loc

and expression =
  {
    pexp_desc: expression_desc;
    pexp_loc: Location.t;
  }

and expression_desc =
  | Pexp_ident of Longident.t loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list * expression
  | Pexp_app of expression * (arg_label * expression) list
  | Pexp_match of expression * expression case list
  | Pexp_tuple of expression list
  | Pexp_variant of label * expression option
  | Pexp_ite of expression * expression * expression option

and action =
  | Pact_match of expression * action case list
  | Pact_bind of string loc * expression
  | Pact_ret of expression
  | Pact_exp of expression

and constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * Location.t * string option
  | Pconst_float of string * char option

and pattern =
  {
    ppat_desc: pattern_desc;
    ppat_loc: Location.t;
  }

and pattern_desc =
  | Ppat_any
  | Ppat_var of string loc
  | Ppat_constant of constant

and 'a case =
  {
    body : 'a;
  }

and value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_loc: Location.t;
  }

and action_binding =
  {
    pab_name: string loc;
    pab_expr: action list;
    pab_loc: Location.t;
  }


type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive

and toplevel_directive =
  {
    pdir_name : string loc;
    pdir_arg : directive_argument option;
    pdir_loc : Location.t;
  }

and directive_argument =
  {
    pdira_desc : directive_argument_desc;
    pdira_loc : Location.t;
  }

and directive_argument_desc =
  | Pdir_string of string
  | Pdir_bool of bool
