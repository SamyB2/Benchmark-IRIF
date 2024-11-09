{
    open Parser
}

let letters = [ 'a'-'z''A'-'Z' ]
let digit = [ '0'-'9' ]
let layout = [' ' '\t' '\r' '\n']

let name = letters (letters | digit)* ('_' (letters | digit)+)*
let number = digit digit*
let float = number '.' number
let comments = "/*" (number | float | letters | layout | ',' | '=' | '>')* "*/"

rule token = parse
(* end of file *)
| eof { EOF }

(* comments *)
| comments { token lexbuf }

(* token for key words *)
| "system" { SYSTEM }
| "task" { TASK }
| "segment" { SEGMENT }
| "utilization" {token lexbuf}

(* token for values *)
| name as n { NAME n }
| float {token lexbuf}
| number as n { NUMBER (int_of_string n) }

(* token for layout *)
| layout { token lexbuf }
