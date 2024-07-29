type build_target = {
  declaration_name : string;
  out_path : Eio.Fs.dir_ty Eio.Path.t;
  yaml : Yaml.value;
}

type t = Pinc.Source.t list * build_target list

let root_and_name filename =
  match filename |> String.split_on_char '~' with
  | "" :: _ -> None
  | [ root ] -> Some (root, "index")
  | [ root; name ] -> Some (root, name)
  | _ -> None
;;

let build ~base ~out : t =
  let rec loop ~build_targets ~sources = function
    | dir :: rest when Eio.Path.is_directory dir ->
        dir
        |> Eio.Path.read_dir
        |> List.map (fun file -> Eio.Path.(dir / file))
        |> List.append rest
        |> loop ~build_targets ~sources
    | ((_dir, filename) as file) :: rest when Filename.extension filename = ".pi" ->
        let source = file |> Eio.Path.load |> Pinc.Source.of_string ~filename in
        let sources = source :: sources in
        rest |> loop ~build_targets ~sources
    | ((_dir, filename) as file) :: rest when Filename.extension filename = ".yaml" ->
        let build_targets =
          let content = file |> Eio.Path.load in
          let basename = Filename.chop_suffix (filename |> Filename.basename) ".yaml" in
          (file, basename, content) :: build_targets
        in
        rest |> loop ~build_targets ~sources
    | _ :: rest -> rest |> loop ~build_targets ~sources
    | [] -> (sources, build_targets)
  in

  let pinc_sources, build_targets = [ base ] |> loop ~build_targets:[] ~sources:[] in

  let build_targets =
    build_targets
    |> List.filter_map (fun data ->
           let file, filename, content = data in
           let yaml = Yaml.of_string_exn content in

           let result = root_and_name filename in

           match result with
           | None -> None
           | Some (declaration_name, target_name) ->
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

               Some { declaration_name; out_path; yaml })
  in

  (pinc_sources, build_targets)
;;

let map fn (t : t) = t |> snd |> List.map fn
let iter fn (t : t) = t |> snd |> List.iter fn
let get_yaml build_target = build_target.yaml

let eval (t : t) build_target tag_data_provider =
  let result =
    try
      t
      |> fst
      |> Pinc.Interpreter.eval_sources
           ~tag_data_provider
           ~root:build_target.declaration_name
      |> Option.some
    with Invalid_argument _ -> None
  in

  (build_target.out_path, result)
;;

let find_first_build_target name (t : t) =
  t |> snd |> List.find_opt (fun build_target -> build_target.declaration_name = name)
;;
