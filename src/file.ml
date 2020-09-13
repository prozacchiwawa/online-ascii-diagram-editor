open Contypes
open Event
open Msg
open Tea.Html

type file
type fileprops

let props contentType : fileprops =
  [ ("type", Js.Json.string contentType)
  ] |> Js.Dict.fromList |> Js.Json.object_ |> Obj.magic

external newFile : string array -> string -> fileprops -> file = "File" [@@bs.new]
external createObjectURLFromFile : file -> string = "URL.createObjectURL" [@@bs.val]
external revokeObjectURL : string -> unit = "URL.revokeObjectURL" [@@bs.val]

external getFileContent : Js.Json.t -> Js.Json.t Js.Nullable.t = "getFileContent" [@@bs.val]

let dropFile ev =
  let _ = preventDefault ev in
  let _ = stopPropagation ev in
  let _ = getFileContent ev |> ignore in
  Some Noop

let dragOver ev =
  let _ = preventDefault ev in
  Some Noop

let takeFile ev =
  ev
  |> Option.bind Js.Json.decodeObject
  |> Option.bind (fun dict -> Js.Dict.get dict "detail")
  |> Option.bind Js.Json.decodeObject
  |> Option.bind
    (fun dict ->
       Js.Dict.get dict "name"
       |> Option.bind Js.Json.decodeString
       |> Option.bind
         (fun name ->
            Js.Dict.get dict "data"
            |> Option.bind Js.Json.decodeString
            |> Option.map Js.Json.parseExn
            |> Option.bind Js.Json.decodeObject
            |> Option.map (fun data -> (name,data))
         )
    )
  |> Option.map (fun (name,data) -> Load (name,data))

let onDrop fn = onCB "drop" "drop" (fun ev -> fn (Obj.magic ev))
let onFile fn = onCB "file" "file" (fun ev -> fn (Obj.magic ev))
let onDragOver fn = onCB "dragover" "dragover" (fun ev -> fn (Obj.magic ev))
