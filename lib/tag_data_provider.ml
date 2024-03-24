let ( let* ) = Option.bind

let rec yaml_to_pinc_value = function
  | `A list -> list |> List.map yaml_to_pinc_value |> Pinc.Value.of_list
  | `Bool b -> Pinc.Value.of_bool b
  | `Float f -> Pinc.Value.of_float f
  | `Null -> Pinc.Value.null ()
  | `O assoc ->
      assoc
      |> Pinc.StringMap.of_list
      |> Pinc.StringMap.map yaml_to_pinc_value
      |> Pinc.Value.of_string_map
  | `String s -> Pinc.Value.of_string s
;;

let find_path path yaml =
  path
  |> List.fold_left
       (fun acc segment ->
         match acc with
         | Some (`A l) -> List.nth_opt l (int_of_string segment)
         | Some (`O obj) -> obj |> List.assoc_opt segment
         | _ -> None)
       (Some yaml)
;;

let rec eval_singleton_store attributes data =
  let _, name, _ =
    attributes |> Pinc.Typer.Expect.(required (attribute "id" definition_info))
  in
  data
  |> Declarations.find_first_build_target name
  |> Option.map Declarations.get_yaml
  |> Option.map yaml_to_pinc_value

and eval_multi_store attributes path data yaml =
  let _, name, _ =
    attributes |> Pinc.Typer.Expect.(required (attribute "id" definition_info))
  in
  let* content =
    data |> Declarations.find_first_build_target name |> Option.map Declarations.get_yaml
  in
  let* store_obj =
    match content with
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
  | Some _ | None -> None

and eval_slot path data yaml make_component =
  let* slot =
    path |> List.fold_left (fun _ segment -> yaml |> Yaml.Util.find_exn segment) None
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
           make_component ~tag ~tag_data_provider:(run attributes data) |> Option.some)
  in

  components |> Pinc.Value.of_list |> Option.some

and eval_array path yaml =
  let* value = yaml |> find_path path in
  match value with
  | `A a -> List.length a |> Pinc.Value.of_int |> Option.some
  | _ -> None

and run ~tag ~attributes ~key:path yaml data =
  let open Pinc.Interpreter.Types.Type_Tag in
  match tag with
  | Tag_Store store when Pinc.Interpreter.Types.Type_Store.is_singleton store ->
      eval_singleton_store attributes data
  | Tag_Store _ -> eval_multi_store attributes path data yaml
  | Tag_Slot make_component -> eval_slot path data yaml make_component
  | Tag_Array -> eval_array path yaml
  | _ -> yaml |> find_path path |> Option.map yaml_to_pinc_value
;;
