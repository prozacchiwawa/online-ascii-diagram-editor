type gridPt
let makeGridPt x y : gridPt =
  [ ("x", Js.Json.number (float_of_int x))
  ; ("y", Js.Json.number (float_of_int y))
  ] |> Js.Dict.fromList |> Js.Json.object_ |> Obj.magic

external getGPX : gridPt -> int = "x" [@@bs.get]
external getGPY : gridPt -> int = "y" [@@bs.get]

type grid

external grid : int -> int -> grid = "Grid" [@@bs.module "@cetfox24/pathfinding-js"] [@@bs.new]
external setSolid : grid -> int -> int -> bool -> unit = "setSolid" [@@bs.send]

type finder
type path

external aStarFinder : unit -> finder = "AStar" [@@bs.module "@cetfox24/pathfinding-js"] [@@bs.new]
external findPath : finder -> gridPt -> gridPt -> grid -> path = "findPath" [@@bs.send]

external getResultPath : path -> gridPt array Js.Nullable.t = "result" [@@bs.get]

