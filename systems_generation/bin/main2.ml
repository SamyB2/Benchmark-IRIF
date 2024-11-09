open Translator.Write_out
open Sy_to_in_format.Parser
open Sy_to_in_format.Lexer 
open Generator.Scheduability_test

let lexbuf = Lexing.from_channel stdin

let system = system token lexbuf


let _ = 
  let argc = Array.length Sys.argv in
  if argc < 2 then
      Printf.printf "Usage: ./translate < path_to_fic.sy <test>\n" 
  else 
    let scheduability_test = match Sys.argv.(1) with
      | "none" -> (fun _ -> false)
      | "test1" -> test1
      | _ -> failwith "Unknown type of scheduability test. Please check README\n"
  in
  make_out [system] scheduability_test 

