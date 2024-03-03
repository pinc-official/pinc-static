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

let rec yaml_to_pinc_value = function
  | `A list -> list |> List.map yaml_to_pinc_value |> Pinc.Value.of_list
  | `Bool b -> Pinc.Value.of_bool b
  | `Float f -> Pinc.Value.of_float f
  | `Null -> Pinc.Value.null ()
  | `O assoc ->
      assoc
      |> Pinc.StringMap.of_list
      |> StringMap.map yaml_to_pinc_value
      |> Pinc.Value.of_string_map
  | `String s -> Pinc.Value.of_string s
;;

let root_and_name filename =
  match filename |> String.split_on_char '~' with
  | "" :: _ -> None
  | [ root ] -> Some (root, "index")
  | [ root; name ] -> Some (root, name)
  | _ -> None
;;

let main ~env =
  Printexc.record_backtrace false;

  let directory = Sys.argv.(1) in
  let output = try Sys.argv.(2) with Invalid_argument _ -> "output" in

  let cwd = Eio.Stdenv.cwd env in
  let fs = Eio.Stdenv.fs env in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 (fs / output);

  Eio.Switch.run @@ fun sw ->
  let out = Eio.Path.open_dir ~sw (fs / output) in
  let base = Eio.Path.open_dir ~sw (cwd / directory) in
  let declarations, data = get_declarations_and_data ~sw base in
  let declarations_with_data =
    data
    |> List.filter_map (fun data ->
           let ( let* ) = Option.bind in

           let _file, filename, _content = data in
           let* root, name = root_and_name filename in
           let* declaration = declarations |> StringMap.find_opt root in
           Some (root, (declaration, name, data)))
    |> StringMap.of_list
  in
  let () =
    declarations_with_data
    |> StringMap.filter (fun _ (declaration, _name, (_file, _filename, _content)) ->
           match declaration with
           | Pinc.Ast.{ declaration_type = Declaration_Page _; _ } -> true
           | _ -> false)
    |> StringMap.iter (fun root (_declaration, name, (file, _filename, content)) ->
           let tag_data_provider ~tag ~attributes ~key:path yaml =
             let open Pinc.Interpreter.Types.Type_Tag in
             match tag with
             | Tag_Store -> (
                 let _, name, _ =
                   attributes
                   |> Pinc.Typer.Expect.(required (attribute "id" definition_info))
                 in
                 match declarations_with_data |> StringMap.find_opt name with
                 | None -> None
                 | Some (_declaration, _name, (_file, _filename, content)) ->
                     content
                     |> Yaml.of_string
                     |> Result.to_option
                     |> Option.map yaml_to_pinc_value)
             | Tag_Slot make_component ->
                 let ( let* ) = Option.bind in
                 let* slot =
                   path
                   |> List.fold_left
                        (fun _ segment -> yaml |> Yaml.Util.find_exn segment)
                        None
                 in
                 let* elements =
                   match slot with
                   | `A a -> Some a
                   | _ -> None
                 in
                 let components =
                   elements
                   |> List.filter_map (fun component_definition ->
                          let* tag =
                            component_definition
                            |> Yaml.Util.find_exn "component"
                            |> Option.map Yaml.Util.to_string_exn
                          in
                          let* attributes =
                            component_definition
                            |> Yaml.Util.find_exn "data"
                            |> Option.map (function
                                   | `O assoc ->
                                       assoc
                                       |> List.map (fun (key, value) ->
                                              (key, yaml_to_pinc_value value))
                                   | _ -> [])
                          in
                          make_component ~tag ~attributes |> Option.some)
                 in

                 components |> Pinc.Value.of_list |> Option.some
             | Tag_Array ->
                 path
                 |> List.fold_left
                      (fun acc segment ->
                        acc
                        |> Fun.flip Option.bind (function
                               | `A l -> List.nth_opt l (int_of_string segment)
                               | `O obj -> obj |> List.assoc_opt segment
                               | _ -> None))
                      (Some yaml)
                 |> Fun.flip Option.bind (function
                        | `A a -> List.length a |> Pinc.Value.of_int |> Option.some
                        | _ -> None)
             | _ ->
                 path
                 |> List.fold_left
                      (fun acc segment ->
                        acc
                        |> Fun.flip Option.bind (function
                               | `A l -> List.nth_opt l (int_of_string segment)
                               | `O obj -> obj |> List.assoc_opt segment
                               | _ -> None))
                      (Some yaml)
                 |> Option.map yaml_to_pinc_value
           in

           let yaml = Yaml.of_string_exn content in
           let tag_data_provider = tag_data_provider yaml in
           let result = declarations |> Interpreter.eval ~tag_data_provider ~root in
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
  in
  ()
;;

let () = Eio_main.run @@ fun env -> main ~env
