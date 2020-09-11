open Contypes

open Tea.App

open Tea.Html
open Msg
open MouseEvent

let fontWidth = ref 13.0
let fontHeight = ref 15.0
let headerSize = 2

type rectshape =
  { top : int
  ; left : int
  ; height : int
  ; width : int
  ; content : string array
  }

type shape
  = Rectangle of rectshape

type mousedown =
  { mx : int
  ; my : int
  ; nx : int
  ; ny : int
  }

type move
  = Down of (int * int)
  | Drag of mousedown

type state =
  { rendered : string array
  ; shapes : shape IntMap.t
  ; serial : int
  ; mouseAction : move option
  }

let finishClick x y model = model

let findRect x y model =
  let found =
    model.shapes
    |> IntMap.bindings
    |> List.filter
      (function
        | (_,Rectangle r) ->
          let _ = Js.log r in
          let _ = Js.log (x,y) in
          let withinX = r.left <= x && (r.left + r.width) > x in
          let withinY = r.top <= y && (r.top + r.height) > y in
          let _ = Js.log withinX in
          let _ = Js.log withinY in
          withinX && withinY
      )
  in
  let _ = Js.log found in
  match found with
  | (nm,shape) :: _ -> Some (nm,shape)
  | _ -> None

let offsetShape x y = function
  | Rectangle r ->
    Rectangle { r with top = r.top + y ; left = r.left + x }

let finishDrag r model =
  match findRect r.mx r.my model with
  | Some (name,shape) ->
    let _ = Js.log shape in
    { model with
      shapes =
        IntMap.update name
          (fun _ ->
             Some (offsetShape (r.nx - r.mx) (r.ny - r.my) shape)
          )
          model.shapes
    }
  | _ -> model

let update model = function
  | WinMsg (MouseDown (x,y)) ->
    let cx = int_of_float @@ (float_of_int x) /. !fontWidth in
    let cy = (int_of_float @@ (float_of_int y) /. !fontHeight) - headerSize in
    { model with mouseAction = Some (Down (cx,cy)) }
  | WinMsg (MouseMove (x,y)) ->
    begin
      let cx = int_of_float @@ (float_of_int x) /. !fontWidth in
      let cy = (int_of_float @@ (float_of_int y) /. !fontHeight) - headerSize in
      match model.mouseAction with
      | None -> model
      | Some (Drag r) -> { model with mouseAction = Some (Drag { r with nx = cx ; ny = cy }) }
      | Some (Down (px,py)) ->
        if px == cx && py == cy then
          model
        else
          { model with mouseAction = Some (Drag { mx = px ; my = py ; nx = cx ; ny = cy }) }
    end
  | WinMsg MouseUp ->
    begin
      match model.mouseAction with
      | None -> model
      | Some (Down (px,py)) -> finishClick px py { model with mouseAction = None }
      | Some (Drag r) -> finishDrag r { model with mouseAction = None }
    end
  | _ -> model

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

let applyShape i s = function
  | Rectangle r ->
    begin
      let string_length = String.length s in
      let left_of = padTo r.left (String.sub s 0 (min string_length r.left)) in
      let right_idx = r.left + r.width in
      let right_of =
        if string_length > right_idx then
          String.sub s right_idx (string_length - right_idx)
        else
          ""
      in
      if i == r.top then
        left_of ^ "," ^ (String.make (r.width - 2) '-') ^ "." ^ right_of
      else if i >= r.top && i < (r.top + r.height - 1) then
        left_of ^ "|" ^ (String.make (r.width - 2) ' ') ^ "|" ^ right_of
      else if i == r.top + r.height - 1 then
        left_of ^ "`" ^ (String.make (r.width - 2) '-') ^ "'" ^ right_of
      else
        s
    end

let rerender model' =
  let model =
    match model'.mouseAction with
    | Some (Drag r) -> finishDrag r model'
    | _ -> model'
  in
  let upper_bound =
    model.shapes
    |> IntMap.bindings
    |> List.fold_left
      (fun ub -> function
        | (_, Rectangle r) -> max (r.top + r.height + 1) ub
      )
      0
  in
  { model' with
    rendered =
      range 0 upper_bound
      |> List.map
        (fun i ->
           model.shapes
           |> IntMap.bindings
           |> List.map (fun (_,x) -> x)
           |> List.fold_left (applyShape i) ""
        )
      |> Array.of_list
  }

let measureRuler () =
  let eltOpt = Window.getElementById "ruler" in
  match Js.Nullable.toOption eltOpt with
  | None -> false
  | Some elt ->
    begin
      let cr = Window.getBoundingClientRect elt in
      fontWidth := (float_of_int (Window.getCW cr)) /. 99.0 ;
      fontHeight := float_of_int (Window.getCH cr) ;
      let _ = Js.log @@ "font width " ^ (string_of_float !fontWidth) in
      let _ = Js.log @@ "font height " ^ (string_of_float !fontHeight) in
      true
    end

let rec setStartTimer () =
  Time.setTimeout
    (fun () ->
       if not (measureRuler ()) then
         setStartTimer ()
       else
         ()
    )
    100
  |> ignore

let init () =
  let _ = setStartTimer () in
  { rendered = [| |]
  ; shapes =
      [
        (0, Rectangle
          { top = 5
          ; left = 10
          ; height = 7
          ; width = 13
          ; content = [| "hi: foo" ; "there: bar" |]
          }
        )
      ]
      |> List.fold_left
        (fun m (n,v) -> IntMap.add n v m)
        IntMap.empty
  ; serial = 1
  ; mouseAction = None
  }
  |> rerender

let ruler () =
  range 0 10
  |> List.map (fun _ -> "....:....")
  |> String.concat "|"

let view model =
  let rendered_show = (* Add a few lines *)
    Array.concat
      [ model.rendered
      ; [| "" ; "" ; "" ; "" ; "" ; "" |]
      ]
  in
  div
    []
    [ div [ ] [ text "controls" ]
    ; div [ id "ruler-container" ]
        [ div [ id "ruler" ; classList [ ("dwg-row",true) ] ] [ text @@ ruler () ]
        ]
    ; div [ ]
        (
          rendered_show
          |> Array.map (fun t -> pre [ classList [("dwg-row",true)] ] [ text t ])
          |> Array.to_list
        )
    ; div
        [ id "mousecover"
        ; onMouseDown (moveMouse (fun p -> MouseDown p))
        ; onMouseMove (moveMouse (fun p -> MouseMove p))
        ; onMouseUp upMouse
        ] [ ]
    ]

let main =
  beginnerProgram
    { model = init ()
    ; update = (fun model msg -> update model msg |> rerender)
    ; view = view
  }
