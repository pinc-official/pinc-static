let get_modification_time file =
  let stat = Eio.Path.stat ~follow:true file in
  stat.mtime
;;

module FileData = struct
  type t = { mutable last_modification_time : float }

  let make path = { last_modification_time = get_modification_time path }

  (** Update FileData modification time with argument value or get last system value *)
  let update_modification_time ?time t file =
    t.last_modification_time <-
      (match time with
      | Some t -> t
      | None -> get_modification_time file)
  ;;

  (** Returns true if the file has been modified. The internal last modification time is also updated.*)
  let has_been_modified t file =
    let time = get_modification_time file in
    let was_modified = time -. t.last_modification_time > 0. in

    if was_modified then
      update_modification_time ~time t file;

    was_modified
  ;;
end

module Dir = struct
  type t = (Eio.Fs.dir_ty Eio.Path.t * FileData.t) list

  (** Creates a Dir structure and populates its internal files corresponding to the [path] parameter *)
  let make path : t =
    let rec loop files = function
      | dir :: rest when Eio.Path.is_directory dir ->
          dir
          |> Eio.Path.read_dir
          |> List.map (fun file -> Eio.Path.(dir / file))
          |> List.append rest
          |> loop files
      | ((_dir, filename) as file) :: rest
        when Filename.extension filename = ".pi" || Filename.extension filename = ".yaml"
        -> rest |> loop ((file, FileData.make file) :: files)
      | _ :: rest -> rest |> loop files
      | [] -> files
    in

    let files = loop [] [ path ] in

    files
  ;;

  (** Loop over all files in the specified directory and return true if a modification has been detected *)
  let files_have_been_modified t =
    t |> List.exists (fun (path, file) -> FileData.has_been_modified file path)
  ;;
end

(** Watch for modifications in a specific path (file or directory) *)
let watch_path ~clock ~delay_in_s ~path on_modification =
  let d = Dir.make path in
  let rec watch_process () =
    Eio.Time.sleep clock delay_in_s;

    (* execute function f if modifications are detected *)
    if Dir.files_have_been_modified d then
      on_modification ();

    watch_process ()
  in
  watch_process ()
;;
