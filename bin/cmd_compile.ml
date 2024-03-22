open Pinc_static

let compile_and_save sources source =
  let yaml = Declarations.get_yaml source in
  let out_file, result =
    Declarations.eval sources source @@ Tag_data_provider.run yaml sources
  in

  Eio.Path.split out_file
  |> Option.map fst
  |> Option.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o777);

  Eio.Path.save ~append:false ~create:(`Or_truncate 0o644) out_file result
;;

let run source_dir output_dir _watch =
  let source_dir =
    if Filename.is_relative source_dir then
      Filename.concat (Unix.getcwd ()) source_dir
    else
      source_dir
  in

  let output_dir =
    if Filename.is_relative output_dir then
      Filename.concat (Unix.getcwd ()) output_dir
    else
      output_dir
  in

  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in

  Eio.Switch.run @@ fun sw ->
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 Eio.Path.(fs / output_dir);
  let base = Eio.Path.open_dir ~sw Eio.Path.(fs / source_dir) in
  let out = Eio.Path.open_dir ~sw Eio.Path.(fs / output_dir) in

  let sources = Declarations.build ~base ~out in

  sources
  |> Declarations.get_pages
  |> Declarations.map (fun source () ->
         Eio.Fiber.fork ~sw @@ fun () -> compile_and_save sources source)
  |> Eio.Fiber.all
;;

let cmd =
  let open Cmdliner in
  let source_dir =
    let doc = "The directory where your source files (pi, yaml) are located." in
    Arg.(required & pos 0 (some dir) None & info [] ~docv:"SOURCE" ~doc)
  in

  let output_dir =
    let doc = "The directory where the compiled html files should be put in." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"DEST" ~doc)
  in

  let watch =
    let doc =
      "Watch the source directory for changes and recompile when something changed. This \
       is useful for development."
    in
    Arg.(value & flag & info [ "watch" ] ~doc)
  in

  let doc = "compile all yaml data files into html files using pinc templates" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(const run $ source_dir $ output_dir $ watch)
;;
