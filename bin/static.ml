open Cmdliner

let main_cmd =
  let doc = "a static html generator using pinc as the template language." in
  let info = Cmd.info "pinc" ~version:"0.1" ~doc in

  Cmd.group info [ Cmd_compile.cmd; Cmd_make.cmd ]
;;

let () = exit (Cmd.eval main_cmd)
