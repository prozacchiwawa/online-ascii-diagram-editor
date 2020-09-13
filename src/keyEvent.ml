open Contypes
open Event
open Msg
open Tea.Html

let getKeyData ev =
  Js.Json.decodeObject ev
  |> Option.bind (fun dict -> Js.Dict.get dict "detail")
  |> Option.bind Js.Json.decodeObject
  |> Option.bind
    (fun dict ->
       Js.Dict.get dict "keyCode"
       |> Option.bind Js.Json.decodeNumber
       |> Option.map int_of_float
       |> Option.bind
         (fun keyCode ->
            Js.Dict.get dict "key"
            |> Option.bind Js.Json.decodeString
            |> Option.map (fun keyName -> (keyCode,keyName))
         )
    )

let keyData f ev =
  let _ = preventDefault ev in
  let _ = stopPropagation ev in
  getKeyData ev
  |> Option.map (fun (code,name) -> KeyMsg (f (code,name)))

let onKeyDown fn = onCB "keydown" "kd" (fun ev -> fn (Obj.magic ev))
let onKeyUp fn = onCB "keyup" "ku" (fun ev -> fn (Obj.magic ev))
