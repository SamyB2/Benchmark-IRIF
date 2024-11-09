{
  open Lexing
  open Parser
}

let newline = '\n' | '\r' | "\r\n"
let blank   = [' ' '\t' '\r']

let digit    = ['0'-'9']
let alpha    = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit | '_'

let identifier = alpha alphanum*

rule token = parse
  | newline            { new_line lexbuf; token lexbuf }
  | blank+             { token lexbuf           }

  (* Comment *)
  | "/*"               { comment 1 lexbuf      }
  | "#" [^ '\n' '\r']*  { token lexbuf }

  (* Keywords *)
  | "cores"            { CORE        }
  | "data"             { DATA        }
  | "in"               { IN          }
  | "out"              { OUT         }
  | "task"             { TASK        }
  | "segment"          { SEGMENT     }
  | "transition"       { TRANSITION  }
  | "->"               { ARROW       }
  | "act"              { ACT         }
  | "end"              { END         }
  | "loop"             { LOOP        }
  | "policy"           { POLICY      }
  | "sequence"         { SEQUENCE    }
  | "task-fair"        { TASKFAIR    }
  | "task-fair RW"     { TASKFAIRRW  }
  | "phase-fair RW"    { PHASEFAIRRW }

  | "affinity_" (identifier as t) { AFFINITY t }

  | identifier as i    { ID i        }
  | digit+ as d        { NUM (int_of_string d) }

  | eof                { EOF         }
  | _ as c             { failwith (Printf.sprintf "unexpected character: %c" c) }
and

comment level = parse
  | "*/"     { if level = 1 then token lexbuf else comment (pred level) lexbuf }
  | "/*"     { comment (succ level) lexbuf }
  | eof      { EOF }
  | newline  { new_line lexbuf; comment level lexbuf }
  | _        { comment level lexbuf }
