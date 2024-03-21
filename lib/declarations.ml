type source = {
  declaration_name : string;
  declaration : Pinc.Ast.declaration;
  out_path : Eio.Fs.dir_ty Eio.Path.t;
  yaml : Yaml.value;
}

type t = Pinc.Ast.declaration Pinc.StringMap.t * source list

let root_and_name filename =
  match filename |> String.split_on_char '~' with
  | "" :: _ -> None
  | [ root ] -> Some (root, "index")
  | [ root; name ] -> Some (root, name)
  | _ -> None
;;

let build ~base ~out : t =
  let rec loop ~data ~declarations = function
    | dir :: rest when Eio.Path.is_directory dir ->
        dir
        |> Eio.Path.read_dir
        |> List.map (fun file -> Eio.Path.(dir / file))
        |> List.append rest
        |> loop ~data ~declarations
    | ((_dir, filename) as file) :: rest when Filename.extension filename = ".pi" ->
        let declarations =
          let decls = file |> Eio.Path.load |> Pinc.Parser.parse ~filename in
          let f key _ _ =
            Pinc_Diagnostics.error
              (Pinc_Diagnostics.Location.make
                 ~s:(Pinc_Diagnostics.Location.Position.make ~filename ~line:0 ~column:0)
                 ())
              ("Found multiple declarations with identifier " ^ key)
          in
          Pinc.StringMap.union f declarations decls
        in
        rest |> loop ~data ~declarations
    | ((_dir, filename) as file) :: rest when Filename.extension filename = ".yaml" ->
        let data =
          let content = file |> Eio.Path.load in
          let basename = Filename.chop_suffix (filename |> Filename.basename) ".yaml" in
          (file, basename, content) :: data
        in
        rest |> loop ~data ~declarations
    | _ :: rest -> rest |> loop ~data ~declarations
    | [] -> (declarations, data)
  in

  let declarations, data = loop ~data:[] ~declarations:Pinc.StringMap.empty [ base ] in

  let sources =
    data
    |> List.filter_map (fun data ->
           let file, filename, content = data in
           let yaml = Yaml.of_string_exn content in

           let result =
             Option.bind (root_and_name filename)
             @@ fun (declaration_name, target_name) ->
             Option.bind (declarations |> Pinc.StringMap.find_opt declaration_name)
             @@ fun declaration -> Some (declaration_name, declaration, target_name)
           in

           match result with
           | None -> None
           | Some (declaration_name, declaration, target_name) ->
               let files =
                 let rec loop files item =
                   match Eio.Path.split item with
                   | None -> snd item :: files
                   | Some (dir, fname) -> loop (fname :: files) dir
                 in
                 loop [] file
               in
               let out_path =
                 files
                 |> List.rev
                 |> List.tl
                 |> List.rev
                 |> List.fold_left Eio.Path.( / ) out
               in
               let out_path = Eio.Path.(out_path / (target_name ^ ".html")) in

               Some { declaration_name; declaration; out_path; yaml })
  in

  (declarations, sources)
;;

let get_pages (t : t) : t =
  ( t |> fst,
    t
    |> snd
    |> List.filter @@ fun { declaration_name = _; declaration; out_path = _; yaml = _ } ->
       match declaration with
       | Pinc.Ast.{ declaration_type = Declaration_Page _; _ } -> true
       | _ -> false )
;;

let map fn (t : t) = t |> snd |> List.map fn
let iter fn (t : t) = t |> snd |> List.iter fn
let get_yaml source = source.yaml

let eval (t : t) source tag_data_provider =
  let result =
    t |> fst |> Pinc.Interpreter.eval ~tag_data_provider ~root:source.declaration_name
  in

  (source.out_path, result)
;;

let find_first_source name (t : t) =
  t |> snd |> List.find_opt (fun source -> source.declaration_name = name)
;;
