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

type edit =
  { editingId : int
  ; editingText : string
  }

type state =
  { rendered : string array
  ; shapes : shape IntMap.t
  ; serial : int
  ; mouseAction : move option
  ; selectedForTyping : int
  ; editing : edit option
  ; prev : state option
  }

let ptInsideRect x y = function
  | Rectangle r ->
    let withinX = r.left <= x && (r.left + r.width) >= x in
    let withinY = r.top <= y && (r.top + r.height) >= y in
    withinX && withinY

let findRects x y model =
  model.shapes
  |> IntMap.bindings
  |> List.filter
    (function
      | (_,Rectangle r) -> ptInsideRect x y (Rectangle r)
    )

let findRect x y model =
  match findRects x y model with
  | (nm,shape) :: _ -> Some (nm,shape)
  | _ -> None

let overlappingShapes box model =
  match box with
  | Rectangle b ->
    let bright = b.left + b.width in
    let bbottom = b.top + b.height in
    let found =
      model.shapes
      |> IntMap.bindings
      |> List.filter
        (function
          | (_,Rectangle r) ->
            let rright = r.left + r.width in
            let rbottom = r.top + r.height in
            let rOverlapBox =
              [ (rright, r.top)
              ; (rright, rbottom)
              ; (r.left, r.top)
              ; (r.left, rbottom)
              ]
              |> listAny (fun (x,y) -> ptInsideRect x y box)
            in
            let boxOverlapR =
              [ (bright, b.top)
              ; (bright, bbottom)
              ; (b.left, b.top)
              ; (b.left, bbottom)
              ]
              |> listAny (fun (x,y) -> ptInsideRect x y (Rectangle r))
            in
            rOverlapBox || boxOverlapR
      )
    in
    found

let allowedBoxLocation id box model' =
  let model = { model' with shapes = IntMap.remove id model'.shapes } in
  let shapes = overlappingShapes box model in
  List.length shapes == 0

let finishClick x y model =
  match findRect x y model with
  | Some (name, Rectangle r) ->
    { model with
      selectedForTyping = name
    }
  | _ -> { model with selectedForTyping = -1 }

let offsetShape x y = function
  | Rectangle r ->
    let newX = max 0 (r.left + x) in
    let newY = max 0 (r.top + y) in
    Rectangle { r with top = newY ; left = newX }

let sizeShape x y = function
  | Rectangle r ->
    let newWidth = max 3 (r.width + x) in
    let newHeight = max 3 (r.height + y) in
    Rectangle { r with height = newHeight ; width = newWidth }

let finishDrag r model =
  match findRect r.mx r.my model with
  | Some (name,Rectangle shape) ->
    let llx = shape.left + shape.width in
    let lly = shape.top + shape.height in
    if llx == r.mx && lly == r.my then
      let updatedBox = sizeShape (r.nx - r.mx) (r.ny - r.my) (Rectangle shape) in
      if allowedBoxLocation name updatedBox model then
        { model with
          shapes = IntMap.update name (fun _ -> Some updatedBox) model.shapes
        ; selectedForTyping = name
        ; prev = Some model
        }
      else
        model
    else
      let updatedBox = offsetShape (r.nx - r.mx) (r.ny - r.my) (Rectangle shape) in
      if allowedBoxLocation name updatedBox model then
        { model with
          shapes = IntMap.update name (fun _ -> Some updatedBox) model.shapes
        ; selectedForTyping = name
        ; prev = Some model
        }
      else
        model
  | _ -> model

let rec findPositionForBox x y box model =
  if y > 100 then
    None
  else if x > 100 then
    findPositionForBox 0 (y + 1) box model
  else
    match box with
    | Rectangle r ->
      let tryit = Rectangle { r with left = x ; top = y } in
      if allowedBoxLocation model.serial tryit model then
        Some tryit
      else
        findPositionForBox (x + 1) y box model

let update model = function
  | NewBox ->
    begin
      let newbox = Rectangle { left = 0; top = 0; width = 6 ; height = 3 ; content = [| |] } in
      match findPositionForBox 0 0 newbox model with
      | None -> model
      | Some b ->
        { model with
          shapes = IntMap.add model.serial b model.shapes ;
          serial = model.serial + 1 ;
          prev = Some model
        }
    end
  | DelBox id ->
    { model with
      shapes = IntMap.remove id model.shapes ;
      prev = Some model
    }
  | Undo ->
    begin
      match model.prev with
      | Some p -> p
      | None -> model
    end
  | Edit id ->
    begin
      try
        let toEdit = IntMap.find id model.shapes in
        match toEdit with
        | Rectangle te ->
          let editString =
            Array.to_list te.content
            |> String.concat "\n"
          in
          { model with editing = Some { editingId = id ; editingText = editString } }
      with _ ->
        model
    end
  | CancelEdit -> { model with editing = None }
  | ChangeText (id,t) -> { model with editing = Some { editingId = id ; editingText = t } }
  | UseEdit (id,t) ->
    { model with
      shapes =
        IntMap.update
          id
          (fun s ->
             match s with
             | Some (Rectangle r) ->
               let newContent = split "\n" t in
               Some (Rectangle { r with content = newContent })
             | None -> None
          )
          model.shapes
    ; editing = None
    ; prev = Some { model with editing = None }
    }
  | WinMsg (MouseDown (x,y)) ->
    let cx = int_of_float @@ (float_of_int x) /. !fontWidth in
    let cy = (int_of_float @@ (float_of_int y) /. !fontHeight) - headerSize in
    let _ = Js.log @@ Printf.sprintf "mouwe down %d %d" cx cy in
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

let applyShape model i s = function
  | (name, Rectangle r) ->
    begin
      let string_length = String.length s in
      let left_of = padTo r.left (String.sub s 0 (min string_length r.left)) in
      let right_idx = r.left + r.width + 1 in
      let right_of =
        if string_length > right_idx then
          String.sub s right_idx (string_length - right_idx)
        else
          ""
      in
      let hfill = if model.selectedForTyping == name then '=' else '-' in
      if i == r.top then
        left_of ^ "," ^ (String.make (r.width - 1) hfill) ^ "." ^ right_of
      else if i >= r.top && i < (r.top + r.height) then
        let boxIndex = i - r.top - 1 in
        let boxLine =
          if boxIndex < Array.length r.content then
            let blString = Array.get r.content boxIndex in
            if String.length blString > r.width - 1 then
              String.sub blString 0 (r.width - 1)
            else
              blString
          else
            ""
        in
        left_of ^ "|" ^ (padTo (r.width - 1) boxLine) ^ "|" ^ right_of
      else if i == r.top + r.height then
        left_of ^ "`" ^ (String.make (r.width - 1) hfill) ^ "'" ^ right_of
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
           |> List.fold_left (applyShape model i) ""
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
  ; selectedForTyping = 0
  ; mouseAction = None
  ; editing = None
  ; prev = None
  }
  |> rerender

let ruler () =
  range 0 10
  |> List.map (fun _ -> "....:....")
  |> String.concat "|"

let viewEditBody model =
  match model.editing with
  | None -> []
  | Some e ->
    [ textarea [ classList [("edit-box",true)] ; onChange (fun t -> ChangeText (e.editingId, t)) ] [ text e.editingText ] ;
      div [ classList [("edit-control-bar",true)] ]
        [ div [ classList [("control-spacer",true)] ] []
        ; div [ classList [("control",true)] ; onClick (UseEdit (e.editingId, e.editingText)) ] [ text "[ Accept ]" ]
        ; div [ classList [("control",true)] ; onClick CancelEdit ] [ text "[ Cancel ]" ]
        ]
    ]

let view model =
  let rendered_show = (* Add a few lines *)
    Array.concat
      [ model.rendered
      ; [| "" ; "" ; "" ; "" ; "" ; "" |]
      ]
  in
  div
    []
    [ div [ id "controls" ]
        [ text "controls"
        ; div [ classList [("control", true)] ; onClick NewBox ] [ text "[ new box ]" ]
        ; div [ classList [("control", true)] ; onClick (DelBox model.selectedForTyping) ] [ text "[ delete box ]" ]
        ; div [ classList [("control", true)] ; onClick (Edit model.selectedForTyping) ] [ text "[ edit ]" ]
        ; div [ classList [("control", true)] ; onClick Undo ] [ text "[ undo ]" ]
        ]
    ; div [ id "ruler-container" ]
        [ div [ id "ruler" ; classList [ ("dwg-row",true) ] ] [ text @@ ruler () ]
        ]
    ; div
        [ onMouseDown (moveMouse (fun p -> MouseDown p))
        ; onMouseMove (moveMouse (fun p -> MouseMove p))
        ; onMouseUp upMouse
        ]
        (
          rendered_show
          |> Array.map (fun t -> pre [ classList [("dwg-row",true)] ] [ text t ])
          |> Array.to_list
        )
    ; div
        [ id "edit" ; classList [ ("edit-active", model.editing <> None) ; ("edit-hidden", model.editing = None) ] ]
        (viewEditBody model)
    ]

let main =
  beginnerProgram
    { model = init ()
    ; update = (fun model msg -> update model msg |> rerender)
    ; view = view
  }
