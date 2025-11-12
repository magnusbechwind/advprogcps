%{
  let get_ident id = Ast.Var (Ast.Ident id)
%}

%token EOF
%token <string> IDENT
%token <string> STRING
%token IF THEN ELSE
%token SELECT
// %token BOOL
%token TRUE FALSE
%token PLUS MINUS MUL DIV
%token EQ ASGN LT
%token LET IN
%token BACKSLASH
%token CALLCC THROW SHIFT RESET
%token PRINT READ
%token <int64> INT_LITERAL
%token LPAREN
%token RPAREN
%token UNDERSCORE
%token RARROW
%token COMMA SEMICOLON

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
  | e1 = expr_add SEMICOLON e2 = expr { Ast.App(Ast.Fn(Ast.Wildcard, e2), e1) } 
  | CALLCC k = ident IN e = expr { Ast.App (Ast.Primop Callcc, Ast.Fn(k, e)) }
  | CALLCC k = lambda { Ast.App (Ast.Primop Callcc, k) }
  | THROW k = ident e = expr { Ast.App(Ast.Primop Throw, Ast.Tuple [Ast.Var k; e]) }
  // | THROW k = ident e = expr { Ast.App(Ast.Primop Throw, Ast.App(Ast.Var k, e)) }
  | RESET e = expr { Ast.App(Ast.Primop Reset, e) }
  | SHIFT k = ident IN e = expr { Ast.App(Ast.Primop Shift, Ast.Fn(k, e))}
  | l = lambda { l }
  | e = expr_add { e }

// + before *
expr_add:
  | PRINT v = value { Ast.App(Ast.Primop Print, v)}
  | READ { Ast.App(Ast.Primop Read, Ast.Var Wildcard)}
  | e1 = expr_add op = add_ops e2 = expr_mul { Ast.App (Ast.Primop op, Ast.Tuple [e1; e2]) }
  | e = expr_mul { e }

// * before application
expr_mul:
    e1 = expr_mul op = mul_ops e2 = expr_cond { Ast.App (Ast.Primop op, Ast.Tuple [e1; e2]) }
  | e = expr_cond { e }

expr_cond:
  | e1 = expr_cond cond = conds e2 = expr_sel { Ast.App (Ast.Primop cond, Ast.Tuple [e1;e2]) }
  | e = expr_sel { e }

conds:
  | EQ { Ast.Eq }
  | LT { Ast.Lt }

// select before app
expr_sel:
    SELECT i = INT_LITERAL e = expr_sel { Ast.Select (Int64.to_int i, e) }
  | e = expr_app { e }

expr_app:
  e1 = expr_app e2 = expr_val { (Ast.App(e1, e2))}
| e = expr_val { e }

// '(' expr ')' recurses back to the first rule
expr_val:
    v = value { v }

value:
    v = INT_LITERAL { Ast.Int (Int64.to_int v) }
  | x = ident { Ast.Var x }
  | b = bool { b }
  | str = STRING { Ast.String str }
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
  | BACKSLASH i = ident RARROW e = expr { Ast.Fn (i,e) }

ident:
  | UNDERSCORE { Ast.Wildcard }
  | i = IDENT { Ast.Ident i }
