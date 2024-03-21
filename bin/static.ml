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

let main ~base ~out ~sw =
  let sources = Declarations.build ~base ~out in

  sources
  |> Declarations.get_pages
  |> Declarations.map (fun source () ->
         Eio.Fiber.fork ~sw @@ fun () -> compile_and_save sources source)
  |> Eio.Fiber.all
;;

let () =
  let ( / ) = Eio.Path.( / ) in

  Eio_main.run @@ fun env ->
  let directory = Sys.argv.(1) in
  let output = try Sys.argv.(2) with Invalid_argument _ -> "output" in
  let fs = Eio.Stdenv.fs env in

  Eio.Switch.run @@ fun sw ->
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 (fs / output);
  let out = Eio.Path.open_dir ~sw (fs / output) in
  let base = Eio.Path.open_dir ~sw (fs / directory) in

  (* let pool =
       Eio.Executor_pool.create
         ~sw
         (Eio.Stdenv.domain_mgr env)
         ~domain_count:(Domain.recommended_domain_count ())
     in *)
  main ~base ~out ~sw
;;
