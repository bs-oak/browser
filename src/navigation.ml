module Fx = BsOakCore.Fx
module Task = BsOakCore.Task
module Cmd = BsOakCore.Cmd

(* ffi *)

let js_push_state: (string -> unit) = [%raw fun url -> " window.history.pushState({}, '', url) "]
let js_replace_state: (string -> unit) = [%raw fun url -> " window.history.replaceState({}, '', url) "]

(* /ffi *)

let ctx = Fx.ctx ()

let load url =
  Fx.Scheduler.binding (fun _callback -> 
    let () = Webapi.Dom.Window.setLocation Webapi.Dom.window url in
    (* kill *)
    fun _ -> ()
  )
  |> Task.perform (fun x -> x)

let reload () =
  Fx.Scheduler.binding (fun _callback -> 
    let () =
      Webapi.Dom.window
      |> Webapi.Dom.Window.location
      |> Webapi.Dom.Location.reload      
    in
    (* kill *)
    fun _ -> ()
  )
  |> Task.perform (fun x -> x)

(* commands *)

type 'msg my_cmd =
  | Go of int
  | Push of string
  | Replace of string

let cmd_map _ mycmd =
  match mycmd with
  | Go n -> Go n
  | Push url -> Push url
  | Replace url -> Replace url

let forward n =
  Fx.command ctx (Go n)

let back n =
  forward (-n)

let push_url url =
  Fx.command ctx (Push url)

let replace_url url =
  Fx.command ctx (Replace url)

(* Subscription *)

type 'msg my_sub =
  Listen of (string -> 'msg)

let sub_map func (Listen tagger) =
  Listen (fun x -> func (tagger x))

let listen tagger =
  Fx.subscription ctx (Listen tagger)

(* state *)

type pop_watcher = 
  | Normal of Fx.Scheduler.process_id

type 'msg state =
  { subs : 'msg my_sub list
  ; pop_watcher : pop_watcher option
  }

(* init *)

let init =
  Fx.Scheduler.succeed 
    { subs = []
    ; pop_watcher = None
    }

(* self message *)

let ignore task b =
  task 
  |> Fx.Scheduler.and_then (fun _ -> Fx.Scheduler.succeed b)

let notify router subs url =
  let send (Listen tagger) = 
    Fx.send_to_app router (tagger url)
  in
  ignore (Fx.Scheduler.sequence (List.map send subs)) ()

let on_self_msg router url state =
  ignore (notify router state.subs url) state

let window_on event_name send_to_self =
  let binding _callback =
    let handler event =
      let _ = Fx.Scheduler.raw_spawn (send_to_self event) in
      ()
    in
    let () = 
      Webapi.Dom.Window.addEventListener event_name handler Webapi.Dom.window 
    in
    let kill_func _pid =
      Webapi.Dom.Window.removeEventListener event_name handler Webapi.Dom.window
    in
    kill_func
  in
  Fx.Scheduler.spawn (Fx.Scheduler.binding binding)

let report_url name router =
  window_on name (fun _evt ->
    Fx.send_to_self 
      router
      (Webapi.Dom.window
      |> Webapi.Dom.Window.location
      |> Webapi.Dom.Location.href)
  )

let spawn_pop_watcher router =
  Fx.Scheduler.map (fun x -> Normal x) (report_url "popstate" router)

let push_state url =
  Fx.Scheduler.binding (fun callback ->
    let kill_func _ = () in
    let () = js_push_state url in
    let loc_href = Webapi.Dom.location |> Webapi.Dom.Location.href in
    let () = callback (Fx.Scheduler.succeed loc_href) in
    kill_func
  )  

let replace_state url =
  Fx.Scheduler.binding (fun callback ->
    let kill_func _ = () in
    let () = js_replace_state url in
    let loc_href = Webapi.Dom.location |> Webapi.Dom.Location.href in
    let () = callback (Fx.Scheduler.succeed loc_href) in
    kill_func
  )  

let go n =
  Fx.Scheduler.binding (fun callback ->
    let kill_none _ = () in
    let () = 
      if 
        n <> 0 
      then 
        Webapi.Dom.window
        |> Webapi.Dom.Window.history
        |> Webapi.Dom.History.go n
    in
    let () = callback (Fx.Scheduler.succeed ()) in
    kill_none
  )

let cmd_help router subs cmd =
  match cmd with
  | Go n ->
    go n

  | Push url ->
    push_state url
    |> Fx.Scheduler.and_then (notify router subs)

  | Replace url ->
    replace_state url
    |> Fx.Scheduler.and_then (notify router subs)

let kill_pop_watcher (Normal pid) =
  Fx.Scheduler.kill pid

let on_effects router cmds subs {pop_watcher; _} =
  let step_state =
    match (subs, pop_watcher) with
    | ([], Some watcher) -> 
      ignore (kill_pop_watcher watcher) {subs; pop_watcher = None}
    
    | (_ :: _, None) ->
      Fx.Scheduler.map (fun a -> {subs; pop_watcher = Some a}) (spawn_pop_watcher router)

    | (_, _) ->
      Fx.Scheduler.succeed {subs; pop_watcher}
  in
    Fx.Scheduler.sequence (List.map (cmd_help router subs) cmds)
    |> Fx.Scheduler.and_then (fun _ -> step_state)

(* register effect manager *)

let () = 
  Fx.manager
  ~ctx: ctx
  ~init: init
  ~on_effects: on_effects
  ~on_self_msg: on_self_msg
  ~cmd_map: cmd_map
  ~sub_map: sub_map