open Pinc

let ( / ) = Eio.Path.( / )

let get_declarations_and_data ~sw directory =
  let rec loop ~sw ~data ~declarations = function
    | dir :: rest when Eio.Path.is_directory dir ->
        dir
        |> Eio.Path.read_dir
        |> List.map (fun file -> dir / file)
        |> List.append rest
        |> loop ~sw ~data ~declarations
    | ((_dir, filename) as file) :: rest when Filename.extension filename = ".pi" ->
        let declarations =
          let decls = file |> Eio.Path.load |> Parser.parse ~filename in
          let f key _ _ =
            Pinc_Diagnostics.error
              (Pinc_Diagnostics.Location.make
                 ~s:(Pinc_Diagnostics.Location.Position.make ~filename ~line:0 ~column:0)
                 ())
              ("Found multiple declarations with identifier " ^ key)
          in
          StringMap.union f declarations decls
        in
        rest |> loop ~sw ~data ~declarations
    | ((_dir, filename) as file) :: rest when Filename.extension filename = ".yaml" ->
        let data =
          let content = file |> Eio.Path.load in
          let basename = Filename.chop_suffix (filename |> Filename.basename) ".yaml" in
          (file, basename, content) :: data
        in
        rest |> loop ~sw ~data ~declarations
    | _ :: rest -> rest |> loop ~sw ~data ~declarations
    | [] -> (declarations, data)
  in
  loop ~sw ~data:[] ~declarations:StringMap.empty [ directory ]
;;

let yaml_to_pinc_env yaml =
  let open Yaml in
  let ( let* ) = Result.bind in
  let* yaml_data = of_string yaml in
  let rec to_pinc_value value =
    match value with
    | `A list -> list |> List.map to_pinc_value |> Pinc.Interpreter.Value.of_list
    | `Bool b -> Pinc.Interpreter.Value.of_bool b
    | `Float f -> Pinc.Interpreter.Value.of_float f
    | `Null -> Pinc.Interpreter.Value.null ()
    | `O assoc ->
        assoc
        |> Pinc.StringMap.of_list
        |> StringMap.map to_pinc_value
        |> Pinc.Interpreter.Value.of_string_map
    | `String s -> Pinc.Interpreter.Value.of_string s
  in
  let* keys = yaml_data |> Util.keys in
  let slot_environment, tag_environment =
    keys
    |> List.partition_map (function
           | "\\slots" -> Either.left (Pinc.Interpreter.Value.null ())
           | key ->
               let value =
                 yaml_data
                 |> Util.find_exn key
                 |> Option.value ~default:`Null
                 |> to_pinc_value
               in
               Either.right (key, value))
  in
  (slot_environment, Pinc.StringMap.of_list tag_environment) |> Result.ok
;;

let main ~env =
  Printexc.record_backtrace true;

  let directory = Sys.argv.(1) in
  let output = try Sys.argv.(2) with Invalid_argument _ -> "output" in

  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 (fs / output);

  Eio.Switch.run @@ fun sw ->
  let out = Eio.Path.open_dir ~sw (fs / output) in
  let base = Eio.Path.open_dir ~sw (cwd / directory) in
  let declarations, data = get_declarations_and_data ~sw base in
  data
  |> List.iter @@ fun (file, filename, content) ->
     match filename |> String.split_on_char '~' with
     | "" :: _ -> ()
     | [ root; name ] -> (
         match yaml_to_pinc_env content with
         | Error (`Msg s) -> failwith s
         | Ok (slot_environment, tag_environment) ->
             let result =
               declarations
               |> Interpreter.eval ~slot_environment ~tag_environment ~root
               |> Interpreter.State.get_output
               |> Interpreter.Value.to_string
             in
             let files =
               let rec loop files item =
                 match Eio.Path.split item with
                 | None -> snd item :: files
                 | Some (dir, fname) -> loop (fname :: files) dir
               in
               loop [] file
             in
             let out_path =
               files |> List.rev |> List.tl |> List.rev |> List.fold_left ( / ) out
             in
             Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 out_path;
             Eio.Path.save
               ~append:false
               ~create:(`Or_truncate 0o644)
               (out_path / (name ^ ".html"))
               result)
     | _ -> ()
;;

let () = Eio_main.run @@ fun env -> main ~env
