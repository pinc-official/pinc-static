type model = {
  (* the choices that will be used and whether they are selected or unselected *)
  choices : (string * [ `selected | `unselected ]) list;
  (* the current position of the cursor *)
  cursor : int;
}

let run_tui () =
  let open Minttea in
  let initial_model =
    {
      cursor = 0;
      choices =
        [
          ("Buy empanadas 🥟", `unselected);
          ("Buy carrots 🥕", `unselected);
          ("Buy cupcakes 🧁", `unselected);
        ];
    }
  in

  let init _model = Command.Noop in

  let update event model =
    match event with
    (* if we press `q` or the escape key, we exit *)
    | Event.KeyDown (Key "q" | Escape) -> (model, Command.Quit)
    (* if we press up or `k`, we move up in the list *)
    | Event.KeyDown (Up | Key "k") ->
        let cursor =
          if model.cursor = 0 then
            List.length model.choices - 1
          else
            model.cursor - 1
        in
        ({ model with cursor }, Command.Noop)
    (* if we press down or `j`, we move down in the list *)
    | Event.KeyDown (Down | Key "j") ->
        let cursor =
          if model.cursor = List.length model.choices - 1 then
            0
          else
            model.cursor + 1
        in
        ({ model with cursor }, Command.Noop)
    (* when we press enter or space we toggle the item in the list
       that the cursor points to *)
    | Event.KeyDown (Enter | Space) ->
        let toggle status =
          match status with
          | `selected -> `unselected
          | `unselected -> `selected
        in
        let choices =
          List.mapi
            (fun idx (name, status) ->
              let status =
                if idx = model.cursor then
                  toggle status
                else
                  status
              in
              (name, status))
            model.choices
        in
        ({ model with choices }, Command.Noop)
    (* for all other events, we do nothing *)
    | _ -> (model, Command.Noop)
  in

  let view model =
    (* we create our options by mapping over them *)
    let options =
      model.choices
      |> List.mapi (fun idx (name, checked) ->
             let cursor =
               if model.cursor = idx then
                 ">"
               else
                 " "
             in
             let checked =
               if checked = `selected then
                 "x"
               else
                 " "
             in
             Format.sprintf "%s [%s] %s" cursor checked name)
      |> String.concat "\n"
    in
    (* and we send the UI for rendering! *)
    Format.sprintf
      {|
What should we buy at the market?

%s

Press q to quit.

  |}
      options
  in

  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model
;;

let run source_dir =
  let source_dir =
    if Filename.is_relative source_dir then
      Filename.concat (Unix.getcwd ()) source_dir
    else
      source_dir
  in

  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in

  Eio.Switch.run @@ fun sw ->
  let base_path = Eio.Path.open_dir ~sw Eio.Path.(fs / source_dir) in
  let _sources = Pinc_static.Declarations.load_pinc_sources ~base_path in
  (* let declarations = sources |> Pinc.Declarations.of_sources in
     let pages = declarations |> Pinc.Declarations.get_pages in *)
  run_tui ()
;;

let cmd =
  let open Cmdliner in
  let source_dir =
    let doc = "The directory where your pinc templates are located." in
    Arg.(required & pos 0 (some dir) None & info [] ~docv:"SOURCE" ~doc)
  in
  let doc = "create a new yaml data file based on a pinc template" in
  let info = Cmd.info "make" ~doc in
  Cmd.v info Term.(const run $ source_dir)
;;
