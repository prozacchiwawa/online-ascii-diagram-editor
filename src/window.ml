type element
type clientRect

external getCX : clientRect -> int = "x" [@@bs.get]
external getCY : clientRect -> int = "y" [@@bs.get]
external getCW : clientRect -> int = "width" [@@bs.get]
external getCH : clientRect -> int = "height" [@@bs.get]

external getElementById : string -> element Js.Nullable.t = "getElementById" [@@bs.scope "document"] [@@bs.val]
external getBoundingClientRect : element -> clientRect = "getBoundingClientRect" [@@bs.send]
