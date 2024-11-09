let parse_args () =
  let filename = ref "" in
  let get_test s =
    if Sys.file_exists (s ^ ".rt") && Sys.file_exists (s ^ ".sol") then
      filename := s
    else invalid_arg (Format.sprintf "%s : file not found" s)
  in
  Arg.parse [] get_test "";
  if !filename = "" then failwith "no filename specified";
  !filename

let parse parser filename =
  let lex = Lexing.from_channel (open_in filename) in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename };
    parser Lexer.token lex
  with
  | Parser.Error ->
      let p = lex.lex_curr_p in
      let col = p.pos_cnum - p.pos_bol in
      let msg =
        Format.asprintf "Parser error in file %s, line %d, column %d\n"
          p.pos_fname p.pos_lnum col
      in
      failwith msg
  | Failure err ->
      let p = lex.lex_curr_p in
      let col = p.pos_cnum - p.pos_bol in
      let msg =
        Format.asprintf "Lexer error in file %s, line %d, column %d\n : %s"
          p.pos_fname p.pos_lnum col err
      in
      failwith msg

let _ =
  let () = Printexc.record_backtrace true in
  let () = Printexc.print_backtrace stderr in
  if Array.length Sys.argv = 0 then
    Format.printf "bad usage: filename expected\n%!"
  else
    let filename = parse_args () in
    try
      let rt = parse Parser.rt (filename ^ ".rt") in
      let affinity = parse Parser.affinity (filename ^ ".sol") in
      let _ =
        print_endline "\nrt";
        Format.printf "%a\n" Ast.Rt.pp rt;
        print_endline "\naffinity";
        Format.printf "%a\n" Ast.Affinity.pp affinity;
        Ast.Rt.check affinity rt
        (* print_endline "\ndata";
           Format.printf "%a\n" Rt.pp_data (Rt.get_data rt);
           print_endline "\noverhead";
           Format.printf "%a\n" Xta.pp_data (Xta.get_overhead affinity rt) *)
      in
      Xta.IntMap.iter
        (fun i xta ->
          let oc = open_out (filename ^ "_" ^ string_of_int i ^ ".xta") in
          Printf.fprintf oc "%s" xta)
        (Xta.rt_to_xta affinity rt)
    with Failure err -> Printf.eprintf "%s" err
