open Contypes
open Msg
open Tea.Html

let getPagePos ev =
  Js.Json.decodeObject ev
  |> Option.bind
    (fun dict ->
       Js.Dict.get dict "pageX"
       |> Option.bind Js.Json.decodeNumber
       |> Option.map int_of_float
       |> Option.bind
         (fun x ->
            Js.Dict.get dict "pageY"
            |> Option.bind Js.Json.decodeNumber
            |> Option.map int_of_float
            |> Option.map (fun y -> (x,y))
         )
    )

external stopPropagation : Js.Json.t -> unit = "stopPropagation" [@@bs.send]
external preventDefault : Js.Json.t -> unit = "preventDefault" [@@bs.send]

let moveMouse f ev =
  let _ = preventDefault ev in
  let _ = stopPropagation ev in
  getPagePos ev
  |> Option.map (fun (x,y) -> WinMsg (f (x,y)))

let upMouse ev =
  let _ = stopPropagation ev in
  Some (WinMsg MouseUp)

let onMouseDown fn = onCB "mousedown" "md" (fun ev -> fn (Obj.magic ev))
let onMouseMove fn = onCB "mousemove" "mm" (fun ev -> fn (Obj.magic ev))
let onMouseUp fn = onCB "mouseup" "mu" (fun ev -> fn (Obj.magic ev))
