module Fx = BsOakCore.Fx

open Webapi.Dom

let ctx = Fx.ctx ()

let load url =
  Fx.Scheduler.binding (fun _callback -> 
    let () = Window.setLocation window url in
    (* kill *)
    fun _ -> ()
  )

let reload () =
  Fx.Scheduler.binding (fun _callback -> 
    let () =
      window
      |> Window.location
      |> Location.reload      
    in
    (* kill *)
    fun _ -> ()
  )

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
      Window.addEventListener event_name handler window 
    in
    let kill_func _pid =
      Window.removeEventListener event_name handler window
    in
    kill_func
  in
  Fx.Scheduler.spawn (Fx.Scheduler.binding binding)

let report_url name router =
  let location_href =
    window
    |> Window.location
    |> Location.href
  in
  let handler _evt = Fx.send_to_self router location_href in
  window_on name handler

let spawn_pop_watcher router =
  Fx.Scheduler.map (fun x -> Normal x) (report_url "popstate" router)

let push_state url =
  Fx.Scheduler.binding (fun callback ->
    let kill_func _ = () in
    let history_state = 
      window
      |> Window.history
      |> History.state
    in
    let () =    
      window
      |> Window.history
      |> History.pushState history_state "" url
    in
    let loc_href = location |> Location.href in
    let () = callback (Fx.Scheduler.succeed loc_href) in
    kill_func
  )  

let replace_state url =
  Fx.Scheduler.binding (fun callback ->
    let kill_func _ = () in
    let history_state = 
      window
      |> Window.history
      |> History.state
    in
    let () =    
      window
      |> Window.history
      |> History.replaceState history_state "" url
    in
    let loc_href = location |> Location.href in
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
        window
        |> Window.history
        |> History.go n
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