%{
  open Ast
  let get_ident id = Ast.Var (Ast.Ident id)

  let rec curry' (expr: Ast.expr) (acc: Ast.expr list) : (Ast.expr * Ast.expr list) =
    begin match expr with
      Ast.App (e1, e2) ->
        begin match e1 with
          Ast.App (e3,e4) ->
            curry' e3 (e4 :: e2 :: acc)
          | _ -> e1, List.rev (e2 :: acc)
          end
      | _ -> (expr, acc)
      end
  
  let rec curry expr =
    let (app,tpl) = curry' expr [] in
    let tup = if List.length tpl == 1 then List.hd tpl else Ast.Tuple tpl
    in Ast.App (app, tup)

%}

%token EOF
%token <string> IDENT
%token IF THEN ELSE
// %token BOOL
%token TRUE FALSE
%token PLUS MINUS MUL DIV
%token EQ
%token LET IN
%token BACKSLASH
// %token INT
%token CALLCC THROW SHIFT RESET
%token <int64> INT_LITERAL
%token LPAREN
%token RPAREN
%token UNDERSCORE
%token RARROW

%start prog
%type <Ast.prog> prog

%%

prog:
    e = expr EOF { Some e }
  | e = EOF { None }

// each expr_* encodes precedence levels of the particular operations
expr:
  | LET ident = IDENT EQ e1 = expr IN e2 = expr { Ast.App (Ast.Fn (Ast.Ident ident, e1), e2) }
  // {Let (Ast.Ident ident, e1, e2)}
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr {Ast.IfEl (e1, e2, e3)}
  | e = expr_add { e }
  | l = lambda { l }

// + before *
expr_add:
    e1 = expr_add op = add_ops e2 = expr_mul { Ast.App (Ast.Primop op, Ast.Tuple [e2; e1]) }
  | e = expr_mul { e }

// * before application
expr_mul:
    e1 = expr_mul op = mul_ops e2 = expr_app { Ast.App (Ast.Primop op, Ast.Tuple [e2; e1]) }
  | e = expr_app { e }

// app before constants or ()
expr_app:
  e1 = expr_curry e2 = expr_val { curry (Ast.App(e1, e2))}
| e = expr_val { e 
}

expr_curry:
  e1 = expr_curry e2 = expr_val {Ast.App (e1, e2)}
| e = expr_val { e }

// '(' expr ')' recurses back to the first rule
expr_val:
    v = value { v }
  | LPAREN e = expr RPAREN { e }

value:
    v = INT_LITERAL { Ast.Int (Int64.to_int v) }
  | x = IDENT { get_ident x }
  | b = bool { b }

bool:
  | TRUE { Ast.Bool true }
  | FALSE { Ast.Bool false }

add_ops:
    PLUS { Ast.Add }
  | MINUS { Ast.Sub }

mul_ops:
  | MUL { Ast.Mul }
  | DIV { Ast.Div }

// TODO: maybe make lambdas take one argument only and use tuples if necessary?
lambda:
    BACKSLASH i = ident RARROW e = expr { Ast.Fn (i,e)}
    // BACKSLASH i = ident* RARROW e = expr {Ast.Lambda (i, e)}

ident:
    i = IDENT { Ast.Ident i}