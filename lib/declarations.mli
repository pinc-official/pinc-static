type source
type t

val map : (source -> 'a) -> t -> 'a list
val iter : (source -> unit) -> t -> unit
val build : base:Eio.Fs.dir_ty Eio.Path.t -> out:Eio.Fs.dir_ty Eio.Path.t -> t
val get_pages : t -> t
val get_yaml : source -> Yaml.value

val eval :
  t ->
  source ->
  Pinc.Interpreter.Types.Type_Tag.data_provider ->
  Eio.Fs.dir_ty Eio.Path.t * string

val find_first_source : string -> t -> source option
