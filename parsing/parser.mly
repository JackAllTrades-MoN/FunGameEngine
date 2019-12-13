%{
  open Asttypes
  open Ast
  open Ast_helper

  let mkloc = Location.mkloc

  let make_loc (startpos, endpos) = {
      Location.loc_start = startpos;
      Location.loc_end = endpos;
    }

  let mkpat ~loc d = Pat.mk ~loc:(make_loc loc) d
  let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d
  let mkstr ~loc d = Str.mk ~loc:(make_loc loc) d

  let syntax_error () =
    raise Syntaxerr.Escape_error

(*
  let expecting loc nonterm =
    raise Syntaxerr.(Error(Expecting(make_loc loc, nonterm)))

  let not_expecting loc nonterm =
    raise Syntaxerr.(Error(Not_expecting(make_loc loc, nonterm))) *)

  let mk_directive_arg ~loc k =
    { pdira_desc = k; pdira_loc = make_loc loc; }

  let mk_directive ~loc name arg =
    Ptop_dir { pdir_name = name; pdir_arg = arg; pdir_loc = make_loc loc; }

  let mkrhs rhs loc = mkloc rhs (make_loc loc)

  type let_binding =
    { lb_pattern: pattern;
      lb_expression: expression;
      lb_loc: Location.t; }

  type let_bindings =
    { lbs_bindings: let_binding list;
      lbs_rec: rec_flag;
      lbs_loc: Location.t
    }

  let mklb ~loc (p, e) = {
    lb_pattern = p;
    lb_expression = e;
    lb_loc = make_loc loc;
  }

  let mklbs ~loc rf lb = {
    lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_loc = make_loc loc;
  }

  let addlb lbs lb =
    { lbs with lbs_bindings = lb :: lbs.lbs_bindings}

  let val_of_let_bindings ~loc lbs =
    let bindings =
      List.map (fun lb ->
                 Vb.mk ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
               lbs.lbs_bindings in
    mkstr ~loc (Pstr_value(lbs.lbs_rec, List.rev bindings))

  let mkpatvar ~loc name =
    mkpat ~loc (Ppat_var (mkrhs name loc))

%}
/*
%token COLON
%token COMMA
*/
%token AND
%token ACTION
%token BAR
%token <string * Location.t> COMMENT
%token CONTEXT
%token DOT
%token EQUAL
%token FALSE
%token HASH
%token HASHOP
%token <string * char option> INT
%token LET
%token MATCH
%token REC
%token SEMISEMI
%token <string * Location.t * string option> STRING
%token TRUE
%token <string> LIDENT
%token <string> UIDENT
%token WITH
%token EOL
%token EOF

// precedence

%nonassoc LET
%nonassoc AND
%nonassoc SEMISEMI
%left EQUAL
%nonassoc LIDENT UIDENT
%nonassoc CONTEXT
%nonassoc HASH
%left HASHOP
%nonassoc DOT


/* Entry points */

%start implementation /* for implementation files */
%type <Ast.structure> implementation

%start toplevel_phrase
%type <Ast.toplevel_phrase> toplevel_phrase

%%

/* macros */
%inline mkrhs(symb): s = symb { mkrhs s $sloc }
%inline mkexp(symb): s = symb { mkexp ~loc:$sloc s }
%inline mk_directive_arg(symb): s = symb { mk_directive_arg ~loc:$sloc s }

toplevel_phrase:
  | s = structure SEMISEMI { Ast.Ptop_def s }
  | d = toplevel_directive SEMISEMI { d }
  | EOF { raise End_of_file }

implementation:
  s = structure EOF { s };

structure:
 | s = structure_item { [s] }
 | si = structure_item s = structure { si :: s }

structure_item:
  | let_bindings { val_of_let_bindings ~loc:$sloc $1 }
  | CONTEXT uid = UIDENT EQUAL ctx = context
     {
       { pstr_desc = Ast.Pstr_defctx(Location.emb_dummy uid, ctx);
         pstr_loc = Location.dummy
       }
     }
  | CONTEXT _lid = LIDENT EQUAL _ctx = context
    { syntax_error () }

context:
  | clid = cont_longident { Ast.Pctx_ident(Location.emb_dummy clid) }
  | _lid = LIDENT { syntax_error () }


ident:
    uid = UIDENT { uid }
  | lid = LIDENT { lid }

val_ident:
  | lid = LIDENT { lid }

%inline let_ident:
  vi = val_ident { mkpatvar ~loc:$sloc vi }


cont_longident:
  | uid = UIDENT { Longident.Lident uid }
  | clid = cont_longident DOT uid = UIDENT { Longident.Ldot(clid, uid) }

/* TODO: stub */
seq_expr:
  | exp = expr { exp }

/* TODO: stub  */
expr:
  | se = simple_expr { se }

/* TODO: stub */
simple_expr:
  | mkexp(simple_expr_) { $1 }

/* TODO: stub */
%inline simple_expr_:
  | constant { Pexp_constant $1 }

let_bindings:
  | lb = let_binding { lb }
  | lbs = let_bindings alb = and_let_binding { addlb lbs alb }

%inline let_binding:
  LET
  rec_flag = rec_flag
  body = let_binding_body
  { mklbs ~loc:$sloc rec_flag (mklb ~loc:$sloc body) }

and_let_binding:
  AND
  body = let_binding_body { mklb ~loc:$sloc body }

let_binding_body:
  lid = let_ident sb = strict_binding { (lid, sb) }

strict_binding:
  | EQUAL exp = seq_expr { exp }

rec_flag:
  |  { Nonrecursive }
  | REC { Recursive }

/* TODO: stub */
constant:
  | INT { let (n, m) = $1 in Pconst_integer (n, m) }
  | STRING { let (s, strloc, d) = $1 in Pconst_string (s, strloc, d) }

toplevel_directive:
  HASH dir = mkrhs(ident)
  arg = ioption(mk_directive_arg(toplevel_directive_argument))
    { mk_directive ~loc:$sloc dir arg }

%inline toplevel_directive_argument:
  | s = STRING { let (s,_,_) = s in Pdir_string s }
  | FALSE { Pdir_bool false }
  | TRUE  { Pdir_bool true }
//  | other = _ { expecting $loc(other) "String or Bool" }
