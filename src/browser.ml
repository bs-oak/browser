open BsOakVirtualDom

let (>>) f g x = g (f x)

module Option = struct 
  let map fn = function
    | Some a -> Some (fn a)
    | None -> None

  let with_default d = function
    | Some a -> a
    | None -> d    

  let and_then fn = function
    | Some a -> fn a
    | None -> None    

  let unwrap_exn msg = function
    | Some a -> a
    | None -> failwith(msg)    
end

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
    ~flags: (flagsGet cfg)
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
    let vdom = ref (Virtual_dom.create (nodeGet cfg)) in
    let () = stepper send_to_app vdom model in
    stepper send_to_app vdom
  in
  BsOakCore.Internal.program 
    ~flags: (flagsGet cfg)
    ~init: init
    ~update: update
    ~subscriptions: subscriptions
    ~stepper_builder: stepper_builder
 
type 'msg document =
  { title : string
  ; body : 'msg BsOakHtml.Html.t list
  }

let map_document fn doc =
  { doc with body = List.map (BsOakHtml.Html.map fn) doc.body }

let document ~init ~update ~view ~subscriptions = fun cfg ->
  let stepper send_to_app vdom model =
    (* update vdom *)
    let doc = view model in
    let node = Virtual_dom.Node.node "body" [] doc.body in
    let () = vdom := Virtual_dom.patch send_to_app node !vdom in
    
    (* update title *)
    let title = 
      Webapi.Dom.document
      |> Webapi.Dom.Document.asHtmlDocument
      |> Option.map Webapi.Dom.HtmlDocument.title
      |> Option.with_default ""
    in
    if title <> doc.title then
      Webapi.Dom.document
      |> Webapi.Dom.Document.asHtmlDocument
      |> Option.map (fun el -> Webapi.Dom.HtmlDocument.setTitle el doc.title)  
      |> Option.with_default ()
  in
  let stepper_builder send_to_app model =
    let vdom =
      Webapi.Dom.document
      |> Webapi.Dom.Document.asHtmlDocument
      |> Option.and_then Webapi.Dom.HtmlDocument.body
      |> Option.unwrap_exn "failed to get document.body element"
      |> Virtual_dom.create
      |> ref
    in
      let () = stepper send_to_app vdom model in
      stepper send_to_app vdom
  in
  BsOakCore.Internal.program 
    ~flags: (flagsGet cfg)
    ~init: init
    ~update: update
    ~subscriptions: subscriptions
    ~stepper_builder: stepper_builder

let application ~init ~update ~view ~subscriptions ~on_navigation =
  let init flags =
    init 
      flags
      (Webapi.Dom.window
      |> Webapi.Dom.Window.location
      |> Webapi.Dom.Location.href
      |> BsOakUrl.Url.from_string
      |> Option.unwrap_exn "failed to create url from window.location")
  in
  let string_to_url str =
    BsOakUrl.Url.from_string str
    |> Option.unwrap_exn "failed to create url from window.location"
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