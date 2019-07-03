(* keyboard *)

val on_key_press: 'msg BsOakJson.Decode.decoder -> 'msg BsOakCore.Sub.t

val on_key_down: 'msg BsOakJson.Decode.decoder -> 'msg BsOakCore.Sub.t

val on_key_up: 'msg BsOakJson.Decode.decoder -> 'msg BsOakCore.Sub.t

(* mouse *)

val on_click: 'msg BsOakJson.Decode.decoder -> 'msg BsOakCore.Sub.t

val on_mouse_move: 'msg BsOakJson.Decode.decoder -> 'msg BsOakCore.Sub.t

val on_mouse_down: 'msg BsOakJson.Decode.decoder -> 'msg BsOakCore.Sub.t

val on_mouse_up: 'msg BsOakJson.Decode.decoder -> 'msg BsOakCore.Sub.t