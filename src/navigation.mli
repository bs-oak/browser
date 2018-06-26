val push_url : string -> 'msg BsOakCore.Cmd.t

val replace_url : string -> 'msg BsOakCore.Cmd.t

val back : int -> 'msg BsOakCore.Cmd.t

val forward : int -> 'msg BsOakCore.Cmd.t

val load : string -> 'msg BsOakCore.Cmd.t

val reload : unit -> 'msg BsOakCore.Cmd.t

val listen : (string -> 'msg) -> 'msg BsOakCore.Sub.t