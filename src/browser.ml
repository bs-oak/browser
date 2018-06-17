open Webapi.Dom
open BsOakBase
open BsOakVirtualDom
open BsOakBase.Infix

type 'flags config =
  { flags: 'flags
  ; node: Dom.element
  } 
  [@@bs.deriving abstract]  

type 'flags program = ('flags config -> unit) 

let worker ~init ~update ~subscriptions = fun cfg ->
  let stepper _model = () in
  let stepper_builder _send_to_app _model = stepper in
  BsOakCore.Internal.program 
    ~flags: (flags cfg)
    ~init: init
    ~update: update
    ~subscriptions: subscriptions
    ~stepper_builder: stepper_builder

let element ~init ~update ~view ~subscriptions = fun cfg ->
  let stepper send_to_app vdom model =
    let node = view model in
    let () = vdom := Virtual_dom.patch send_to_app node !vdom in
    ()
  in
  let stepper_builder send_to_app model = 
    let vdom = ref (Virtual_dom.create (node cfg)) in
    let () = stepper send_to_app vdom model in
    stepper send_to_app vdom
  in
  BsOakCore.Internal.program 
    ~flags: (flags cfg)
    ~init: init
    ~update: update
    ~subscriptions: subscriptions
    ~stepper_builder: stepper_builder
 
type 'msg document =
  { title : string
  ; body : 'msg BsOakHtml.Html.t list
  }

let document ~init ~update ~view ~subscriptions = fun cfg ->
  let stepper send_to_app vdom model =
    (* update vdom *)
    let doc = view model in
    let node = Virtual_dom.Node.node "body" [] doc.body in
    let () = vdom := Virtual_dom.patch send_to_app node !vdom in
    
    (* update title *)
    let title = 
      document
      |> Document.asHtmlDocument
      |> Option.map HtmlDocument.title
      |> Option.with_default ""
    in
    if title <> doc.title then
      document
      |> Document.asHtmlDocument
      |> Option.map (fun el -> HtmlDocument.setTitle el doc.title)  
      |> Option.with_default ()
  in
  let stepper_builder send_to_app model =
    let vdom =
      document
      |> Document.asHtmlDocument
      |> Option.and_then HtmlDocument.body
      |> Option.unsafe_unwrap "failed to get document.body element"
      |> Virtual_dom.create
      |> ref
    in
      let () = stepper send_to_app vdom model in
      stepper send_to_app vdom
  in
  BsOakCore.Internal.program 
    ~flags: (flags cfg)
    ~init: init
    ~update: update
    ~subscriptions: subscriptions
    ~stepper_builder: stepper_builder

let application ~init ~update ~view ~subscriptions ~on_navigation =
  let string_to_url str =
    BsOakUrl.Url.from_string str
    |> Option.unsafe_unwrap "failed to create url from window.location"
  in
  let init flags =
    init 
      flags
      (window
      |> Window.location
      |> Location.href
      |> BsOakUrl.Url.from_string
      |> BsOakBase.Option.unsafe_unwrap "failed to create url from window.location")
  in
  let subscriptions model =
    BsOakCore.Sub.batch 
      [ Navigation.listen (string_to_url >> on_navigation)
      ; subscriptions model
      ]
  in
  document
    ~init: init
    ~update: update
    ~view: view
    ~subscriptions: subscriptions