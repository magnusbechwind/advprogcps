%{
  let get_ident id = Ast.Var (Ast.Ident id)
%}

%token EOF
%token <string> IDENT
%token IF THEN ELSE
// %token BOOL
%token TRUE FALSE
%token PLUS MINUS MUL DIV
%token EQ ASGN LT
%token LET IN
%token BACKSLASH
// %token INT
%token CALLCC THROW SHIFT RESET
%token <int64> INT_LITERAL
%token LPAREN
%token RPAREN
%token UNDERSCORE
%token RARROW
%token COMMA

%start prog
%type <Ast.prog> prog

%%

prog:
    e = expr EOF { Some e }
  | EOF { None }

// each expr_* encodes precedence levels of the particular operations
expr:
  | LET id = ident ASGN e1 = expr IN e2 = expr { Ast.App (Ast.Fn (id, e2), e1) }
  // {Let (Ast.Ident ident, e1, e2)}
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr {Ast.IfEl (e1, e2, e3)}
  | e = expr_add { e }
  | l = lambda { l }
  | CALLCC k = ident IN e = expr { Ast.App (Ast.Primop Callcc, Ast.Fn(k, e)) }

// + before *
expr_add:
    e1 = expr_add op = add_ops e2 = expr_mul { Ast.App (Ast.Primop op, Ast.Tuple [e1; e2]) }
  | e = expr_mul { e }

// * before application
expr_mul:
    e1 = expr_mul op = mul_ops e2 = expr_cond { Ast.App (Ast.Primop op, Ast.Tuple [e1; e2]) }
  | e = expr_cond { e }

expr_cond:
  | e1 = expr_cond cond = conds e2 = expr_app { Ast.App (Ast.Primop cond, Ast.Tuple [e1;e2]) }
  | e = expr_app { e }

conds:
  | EQ { Ast.Eq }
  | LT { Ast.Lt }

// app before constants or ()
expr_app:
  e1 = expr_app e2 = expr_val { (Ast.App(e1, e2))}
| e = expr_val { e }

// '(' expr ')' recurses back to the first rule
expr_val:
    v = value { v }

value:
    v = INT_LITERAL { Ast.Int (Int64.to_int v) }
  | x = IDENT { get_ident x }
  | b = bool { b }
  | t = tuple {
      match t with
      | Ast.Tuple [e] -> e
      | Ast.Tuple _ -> t
      | _ -> failwith "unreachable"
      }
  // | LPAREN e = expr RPAREN { e }

tuple:
  | LPAREN l = separated_list(COMMA, expr) RPAREN { Ast.Tuple l}


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
  // | BACKSLASH t = tuple RARROW e = expr { t }
  | BACKSLASH i = ident RARROW e = expr { Ast.Fn (i,e)}
    // BACKSLASH i = ident* RARROW e = expr {Ast.Lambda (i, e)}

ident:

    i = IDENT { Ast.Ident i}