%{
  open Ast
%}
/*
%token AND
%token COLON
%token COMMA
*/
%token CONTEXT
//%token DOT
%token EQUAL
%token <string> LIDENT
%token <string> UIDENT
%token EOF

// precedence

%left EQUAL
%nonassoc LIDENT UIDENT
%nonassoc CONTEXT


/* Entry points */

%start implementation /* for implementation files */
%type <Ast.structure> implementation

%%

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
