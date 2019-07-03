module Decode = BsOakJson.Decode
module Fx = BsOakCore.Fx
module Platform = BsOakCore.Platform
module Scheduler = BsOakCore.Fx_scheduler
module StringMap = BsOakExt.Ext.Map.Make(String)

type node =
  | Document
  | Window

type 'msg my_sub = 
  | MySub of node * string * 'msg Decode.decoder

let sub_map fn (MySub (node, name, decoder)) =
  MySub (node, name, (Decode.map fn decoder))

let ctx = Fx.ctx ()

let on node name decoder =
  MySub (node, name, decoder)
  |> Fx.subscription ctx 

(* keyboard *)

let on_key_press decoder = on Document "keypress" decoder

let on_key_down decoder = on Document "keydown" decoder

let on_key_up decoder = on Document "keyup" decoder

(* mouse *)

let on_click decoder = on Document "click" decoder

let on_mouse_move decoder = on Document "mousemove" decoder

let on_mouse_down decoder = on Document "mousedown" decoder

let on_mouse_up decoder = on Document "mouseup" decoder


(* fx manager *)

type 'msg state =
  { subs: (string * 'msg my_sub) list;
    pids:  Platform.process_id StringMap.t;
  }  

type event =
  { key: string;
    event: Decode.value;
  }  

let init =
  Scheduler.succeed { subs = []; pids = StringMap.empty }

let add_event_listener node event_name send_to_self =
  Scheduler.spawn (Scheduler.binding (fun _callback -> 
    let handler event = 
      let _ = Scheduler.raw_spawn (send_to_self (Obj.magic event)) in
      ()      
    in
    match node with
    | Window ->
      let () = Webapi.Dom.Window.addEventListener event_name handler Webapi.Dom.window in
      (fun () -> Webapi.Dom.Window.removeEventListener event_name handler Webapi.Dom.window)
    | Document ->
      let () = Webapi.Dom.Document.addEventListener event_name handler Webapi.Dom.document in
      (fun () -> Webapi.Dom.Document.removeEventListener event_name handler Webapi.Dom.document)    
  ))

let spawn router key (MySub (node, name, _decoder)) =
  (fun event -> Fx.send_to_self router {key; event})
  |> add_event_listener node name 
  |> Scheduler.map (fun value -> key, value)

let add_key (MySub (node, name, decoder)) =
  let prefix =
    match node with
    | Document -> "d_"
    | Window -> "w_"
  in
  prefix ^ name, MySub (node, name, decoder)

let on_effects router subs state =
  let new_subs =
    List.map add_key subs
  in

  let right_map =
    List.fold_left (fun a (k, v) -> StringMap.add k v a) StringMap.empty new_subs
  in

  let step_left _ pid (deads, lives, news) =
    pid :: deads, lives, news
  in

  let step_both key pid _ (deads, lives, news) =
    deads, StringMap.add key pid lives, news
  in

  let step_right key sub (deads, lives, news) =
    deads, lives, (spawn router key sub) :: news
  in

  let (dead_pids, live_pids, make_new_pids) =
    StringMap.merge step_left step_both step_right state.pids right_map ([], StringMap.empty, [])
  in

  Scheduler.sequence (List.map Scheduler.kill dead_pids)
    |> Scheduler.and_then (fun _ -> Scheduler.sequence make_new_pids)
    |> Scheduler.and_then (fun pids -> Scheduler.succeed {subs = new_subs; pids = StringMap.union live_pids (StringMap.from_list pids)})
  

let on_self_msg router {key; event} state =
  let to_message (sub_key, (MySub (_node, _name, decoder))) accum =
    if sub_key = key then
      match Decode.decode_value decoder event with
      | Ok v -> v :: accum
      | Error _msg -> accum
    else
      accum
  in

  let messages =
    List.fold_right to_message state.subs []
  in

  Scheduler.sequence (List.map (Fx.send_to_app router) messages)
    |> Scheduler.and_then (fun _ -> Scheduler.succeed state)

let () = 
  Fx.sub_manager
  ~ctx: ctx
  ~init: init
  ~on_effects: on_effects
  ~on_self_msg: on_self_msg
  ~sub_map: sub_map