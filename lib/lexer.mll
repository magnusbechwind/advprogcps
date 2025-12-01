{
    open Parser

    let stri = ref ""
}

rule token = parse
| eof { EOF }

| "let" { LET }
| "in" { IN }
| "," { COMMA }
| ";" { SEMICOLON }

(* | "bool" { BOOL } *)
| "true" { TRUE }
| "false" { FALSE }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }

(* select *)
| "#" { SELECT }
| '_' { UNDERSCORE }

(* I/O operators *)
| "print" { PRINT }
| "println" { PRINTLN }
| "read" { READ }

(* operators *)
| "->" { RARROW }
| '\\' { BACKSLASH }
| "//" { comment lexbuf}
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
| "rec" { REC }
| "and" { AND }

(* file manipulation stuff *)
| [' ''\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }

(* string reading *)
| '"' {stri := ""; STRING (Scanf.unescaped (str lexbuf))}

(* numbers stuff *)
| ['1'-'9']['0'-'9']* as num { INT_LITERAL (Int64.of_string num) }
| '0' { INT_LITERAL (Int64.zero) }
| ['0'-'9']['0'-'9']+  { failwith "cannot have leading zeroes" }

| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''_''0'-'9']* as id { IDENT id }

and str = parse
| "\\\\"        as s {stri := (!stri) ^ s; str lexbuf}
| "\\\""        as s {stri := (!stri) ^ s; str lexbuf}
| '"'          {!stri}
| _             as c {stri := (!stri) ^ (String.make 1 c); str lexbuf}

and comment = parse
| "\n"          {Lexing.new_line lexbuf; token lexbuf}
| _             {comment lexbuf}
