val push_url : string -> 'msg BsOakCore.Cmd.t

val replace_url : string -> 'msg BsOakCore.Cmd.t

val back : int -> 'msg BsOakCore.Cmd.t

val forward : int -> 'msg BsOakCore.Cmd.t

val load : string -> (unit, unit) BsOakCore.Task.t

val reload : unit -> (unit, unit) BsOakCore.Task.t

val listen : (string -> 'msg) -> 'msg BsOakCore.Sub.t