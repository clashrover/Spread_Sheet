let _ =
  try
    let filename = Sys.argv.(1) in
    let file_handle = open_in filename in
    let lexbuf = Lexing.from_channel file_handle in
    while true do
      let result = Parser.line Lexer.token lexbuf in
        print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0