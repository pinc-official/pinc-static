type build_target
type t

val map : (build_target -> 'a) -> t -> 'a list
val iter : (build_target -> unit) -> t -> unit
val load_pinc_sources : base_path:Eio.Fs.dir_ty Eio.Path.t -> Pinc.Source.t list
val build : base:Eio.Fs.dir_ty Eio.Path.t -> out:Eio.Fs.dir_ty Eio.Path.t -> t
val get_yaml : build_target -> Yaml.value

val eval :
  t ->
  build_target ->
  Pinc.Interpreter.Types.Type_Tag.data_provider ->
  Eio.Fs.dir_ty Eio.Path.t * string option

val find_first_build_target : string -> t -> build_target option
