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

let find_path path yaml =
  path
  |> List.fold_left
       (fun acc segment ->
         acc
         |> Fun.flip Option.bind (function
                | `A l -> List.nth_opt l (int_of_string segment)
                | `O obj -> obj |> List.assoc_opt segment
                | _ -> None))
       (Some yaml)
;;

let main ~env =
  Printexc.record_backtrace false;

  let directory = Sys.argv.(1) in
  let output = try Sys.argv.(2) with Invalid_argument _ -> "output" in

  let fs = Eio.Stdenv.fs env in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 (fs / output);

  Eio.Switch.run @@ fun sw ->
  let out = Eio.Path.open_dir ~sw (fs / output) in
  let base = Eio.Path.open_dir ~sw (fs / directory) in
  let declarations, data = get_declarations_and_data ~sw base in
  let declarations_with_data =
    data
    |> List.fold_left
         (fun acc data ->
           let _file, filename, _content = data in

           let result =
             Option.bind (root_and_name filename) @@ fun (root, name) ->
             Option.bind (declarations |> StringMap.find_opt root) @@ fun declaration ->
             Some (root, declaration, name, data)
           in

           match result with
           | None -> acc
           | Some (root, declaration, name, data) when not (StringMap.mem root acc) ->
               let value = [ (declaration, name, data) ] in
               StringMap.add root value acc
           | Some (root, declaration, name, data) ->
               let value = (declaration, name, data) :: StringMap.find root acc in
               StringMap.add root value acc)
         StringMap.empty
  in
  let pages =
    declarations_with_data
    |> StringMap.filter_map @@ fun _ list ->
       let result =
         list
         |> List.filter (fun (declaration, _name, (_file, _filename, _content)) ->
                match declaration with
                | Pinc.Ast.{ declaration_type = Declaration_Page _; _ } -> true
                | _ -> false)
       in
       match result with
       | [] -> None
       | list -> Some list
  in

  pages
  |> StringMap.iter @@ fun root ->
     List.iter @@ fun (_declaration, name, (file, _filename, content)) ->
     let rec tag_data_provider ~tag ~attributes ~key:path yaml =
       let ( let* ) = Option.bind in
       let open Pinc.Interpreter.Types.Type_Tag in
       match tag with
       | Tag_Store store when Interpreter.Types.Type_Store.is_singleton store ->
           let _, name, _ =
             attributes |> Pinc.Typer.Expect.(required (attribute "id" definition_info))
           in
           let* _declaration, _name, (_file, _filename, content) =
             declarations_with_data |> StringMap.find_opt name |> Option.map List.hd
           in
           content |> Yaml.of_string |> Result.to_option |> Option.map yaml_to_pinc_value
       | Tag_Store _store -> (
           let _, name, _ =
             attributes |> Pinc.Typer.Expect.(required (attribute "id" definition_info))
           in
           let* _declaration, _name, (_file, _filename, content) =
             declarations_with_data |> StringMap.find_opt name |> Option.map List.hd
           in
           let* store_yaml = content |> Yaml.of_string |> Result.to_option in
           let* store_obj =
             match store_yaml with
             | `O o -> Some o
             | _ -> None
           in

           let value = yaml |> find_path path in
           match value with
           | Some (`String "__all__") ->
               store_obj
               |> List.map snd
               |> List.map yaml_to_pinc_value
               |> Pinc.Value.of_list
               |> Option.some
           | Some (`A list) ->
               list
               |> List.filter_map (function
                      | `String key -> store_obj |> List.assoc_opt key
                      | _ -> None)
               |> List.map yaml_to_pinc_value
               |> Pinc.Value.of_list
               |> Option.some
           | Some _ | None -> None)
       | Tag_Slot make_component ->
           let* slot =
             path
             |> List.fold_left (fun _ segment -> yaml |> Yaml.Util.find_exn segment) None
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
                    let* attributes = component_definition |> Yaml.Util.find_exn "data" in
                    make_component ~tag ~tag_data_provider:(tag_data_provider attributes)
                    |> Option.some)
           in

           components |> Pinc.Value.of_list |> Option.some
       | Tag_Array ->
           yaml
           |> find_path path
           |> Fun.flip Option.bind (function
                  | `A a -> List.length a |> Pinc.Value.of_int |> Option.some
                  | _ -> None)
       | _ -> yaml |> find_path path |> Option.map yaml_to_pinc_value
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
       result
;;

let () = Eio_main.run @@ fun env -> main ~env
