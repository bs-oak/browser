type 'flags program

val worker : 
  init: ('flags -> 'model * 'msg BsOakCore.Cmd.t) ->
  update: ('msg -> 'model -> 'model * 'msg BsOakCore.Cmd.t ) ->
  subscriptions: ('model -> 'msg BsOakCore.Sub.t) ->
  'flags program

val element : 
  init: ('flags -> 'model * 'msg BsOakCore.Cmd.t) ->
  update: ('msg -> 'model -> 'model * 'msg BsOakCore.Cmd.t ) ->
  view: ('model -> 'msg BsOakHtml.Html.t) ->
  subscriptions: ('model -> 'msg BsOakCore.Sub.t) ->
  'flags program

type 'msg document =
  { title : string
  ; body : 'msg BsOakHtml.Html.t list
  }

val document : 
  init: ('flags -> 'model * 'msg BsOakCore.Cmd.t) ->
  update: ('msg -> 'model -> 'model * 'msg BsOakCore.Cmd.t) ->
  view: ('model -> 'msg document) ->
  subscriptions: ('model -> 'msg BsOakCore.Sub.t) ->
  'flags program

val application : 
  init: ('flags -> BsOakUrl.Url.url -> 'model * 'msg BsOakCore.Cmd.t) ->
  update: ('msg -> 'model -> 'model * 'msg BsOakCore.Cmd.t) ->
  view: ('model -> 'msg document) ->
  subscriptions: ('model -> 'msg BsOakCore.Sub.t) ->
  on_navigation: (BsOakUrl.Url.url -> 'msg) ->
  'flags program