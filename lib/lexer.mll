{
    open Parser
}

rule token = parse
| eof { EOF }

| "let" { LET }
| "in" { IN }
| "," { COMMA }

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
| '=' { ASGN }
| "==" { EQ }
| "<" { LT }
| "call/cc" { CALLCC }
| "throw" { THROW }
| "shift" { SHIFT }
| "reset" { RESET }


(* file manipulation stuff *)
| [' ''\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }

(* numbers stuff *)
| ['1'-'9']['0'-'9']* as num { INT_LITERAL (Int64.of_string num) }
| '0' { INT_LITERAL (Int64.zero) }
| ['0'-'9']['0'-'9']+  { failwith "cannot have leading zeroes" }

| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_']* as id { IDENT id }