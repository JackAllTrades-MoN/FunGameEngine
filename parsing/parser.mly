%{
  open Ast

  let mkloc = Location.mkloc

  let make_loc (startpos, endpos) = {
      Location.loc_start = startpos;
      Location.loc_end = endpos;
    }

  let _syntax_error () =
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

%}
/*
%token AND
%token COLON
%token COMMA
*/
%token CONTEXT
//%token DOT
%token EQUAL
%token FALSE
%token HASH
%token HASHOP
%token SEMISEMI
%token <string * Location.t * string option> STRING
%token TRUE
%token <string> LIDENT
%token <string> UIDENT
%token EOL
%token EOF

// precedence

%nonassoc SEMISEMI
%left EQUAL
%nonassoc LIDENT UIDENT
%nonassoc CONTEXT
%nonassoc HASH
%left HASHOP


/* Entry points */

%start implementation /* for implementation files */
%type <Ast.structure> implementation

%start toplevel_phrase
%type <Ast.toplevel_phrase> toplevel_phrase

%%

/* macros */
%inline mkrhs(symb): s = symb { mkrhs s $sloc }
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
  | CONTEXT uid = UIDENT EQUAL ctx = context
     {
       { pstr_desc = Ast.Pstr_defctx(Location.emb_dummy uid, ctx);
         pstr_loc = Location.dummy
       }
     }

context:
  | lid = LIDENT
    { Ast.Pctx_ident(Location.emb_dummy @@ Longident.lid lid) }


ident:
    uid = UIDENT { uid }
  | lid = LIDENT { lid }


toplevel_directive:
  HASH dir = mkrhs(ident)
  arg = ioption(mk_directive_arg(toplevel_directive_argument))
    { mk_directive ~loc:$sloc dir arg }

%inline toplevel_directive_argument:
  | s = STRING { let (s,_,_) = s in Pdir_string s }
  | FALSE { Pdir_bool false }
  | TRUE  { Pdir_bool true }
//  | other = _ { expecting $loc(other) "String or Bool" }
