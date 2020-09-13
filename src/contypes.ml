module type Ord = sig
  type t
  val compare : t -> t -> int
end

module IntOrd = struct
  type t = int
  let compare a b = Pervasives.compare a b
end

module StringMap = Map.Make(String)
module IntMap = Map.Make(IntOrd)
module IntSet = Set.Make(IntOrd)

let split : string -> string -> string array = [%bs.raw {| function(sep,str) { return str.split(sep); } |} ]
let btoa : string -> string = [%bs.raw {| function(x) { return btoa(x); } |} ]

type date
external newDate : unit -> date = "Date" [@@bs.new]
external primGetTime : date -> float = "getTime" [@@bs.send]

module Option = struct
  let map f = function
    | Some v -> Some (f v)
    | None -> None

  let bind f = function
    | Some v -> f v
    | None -> None

  let default d = function
    | Some v -> v
    | None -> d

  let filter f = function
    | Some v -> if f v then Some v else None
    | _ -> None
end

let rec listAny f = function
  | [] -> false
  | hd :: tl -> (f hd) || listAny f tl

let rec catOptions = function
  | [] -> []
  | None :: tl -> catOptions tl
  | (Some a) :: tl -> a :: catOptions tl

let rec range a b =
  if a < b then
    a :: (range (a+1) b)
  else
    []

let padTo n s =
  let sl = String.length s in
  if sl < n then
    let defecit = n - sl in
    let padding = String.make defecit ' ' in
    s ^ padding
  else
    s

let replaceInString x ch s =
  let slen = String.length s in
  if slen < x then
    (padTo x s) ^ (String.make 1 ch)
  else if slen == x then
    s ^ (String.make 1 ch)
  else
    (String.sub s 0 x) ^ (String.make 1 ch) ^ (String.sub s (x+1) (slen - x - 1))
