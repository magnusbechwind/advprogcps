{
    open Parser
}

rule token = parse
| eof { EOF }

| "let" { LET }
| "in" { IN }

(* | "bool" { BOOL } *)
| "true" { TRUE }
| "false" { FALSE }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }

(* operators *)
| "->" { RARROW }
| '\\' { BACKSLASH }
| '(' { LPAREN }
| ')' { RPAREN }
| '+' { PLUS }
| '-' { MINUS }
| '*' { MUL }
| '/' { DIV }
| '=' { EQ }
| "call/cc" { CALLCC }
| "throw" { THROW }
| "shift" { SHIFT }
| "reset" { RESET }

(* file manipulation stuff *)
| [' ''\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }

(* | "int" { INT } *)
| ['1'-'9']['0'-'9']* as num { INT_LITERAL (Int64.of_string num) }

| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_']* as id { IDENT id}