open Contypes

open Tea.App

open Tea.Html
open Msg
open KeyEvent
open MouseEvent

let fontWidth = ref 13.0
let fontHeight = ref 15.0
let headerSize = 2

type ksaResult

external keySendToApp : unit -> ksaResult = "keySendToApp" [@@bs.scope "window"] [@@bs.val]
external keyCancel : ksaResult -> unit = "keyCancel" [@@bs.scope "window"] [@@bs.val]

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

type draw =
  { drawAtX : int
  ; drawAtY : int
  ; drawStartX : int
  ; drawStartY : int
  ; linePath : (int * int) list option
  }

module IntPairOrd = struct
  type t = int * int
  let compare a b = Pervasives.compare a b
end

module IntPairMap = Map.Make(IntPairOrd)

type state =
  { rendered : string array
  ; shapes : shape IntMap.t
  ; drawing : char IntPairMap.t
  ; serial : int
  ; mouseAction : move option
  ; selectedForTyping : int
  ; editing : edit option
  ; drawMode : draw option
  ; help : bool
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

let clearBox drawing r =
  IntPairMap.filter (fun (y,x) _ -> not @@ ptInsideRect x y r) drawing

let copyBox drawing r shape model =
  range shape.top (shape.top + shape.height + 1)
  |> List.fold_left
    (fun drawing i ->
       range shape.left (shape.left + shape.width + 1)
       |> List.fold_left
         (fun drawing j ->
            try
              let ch = IntPairMap.find (i,j) model.drawing in
              IntPairMap.update
                (i + r.ny - r.my,j + r.nx - r.mx)
                (fun _ -> Some ch)
                drawing
            with _ ->
              drawing
         )
         drawing
    )
    drawing

let finishDrag r model =
  match findRect r.mx r.my model with
  | Some (name,Rectangle shape) ->
    let llx = shape.left + shape.width in
    let lly = shape.top + shape.height in
    if llx == r.mx && lly == r.my then
      let updatedBox = sizeShape (r.nx - r.mx) (r.ny - r.my) (Rectangle shape) in
      if allowedBoxLocation name updatedBox model then
        let rightRect =
          { shape with left = shape.left + shape.width ; width = 0 }
        in
        let botRect =
          { shape with top = shape.top + shape.height ; height = 0 }
        in
        let clearedBox =
          clearBox (clearBox model.drawing (Rectangle rightRect)) (Rectangle botRect)
        in
        let newDrawingR =
          copyBox clearedBox { r with my = 0 ; ny = 0 } rightRect model
        in
        let newDrawingB =
          copyBox newDrawingR { r with mx = 0 ; nx = 0 } botRect model
        in
        { model with
          shapes = IntMap.update name (fun _ -> Some updatedBox) model.shapes
        ; drawing = newDrawingB
        ; selectedForTyping = name
        ; prev = Some model
        }
      else
        model
    else
      let updatedBox = offsetShape (r.nx - r.mx) (r.ny - r.my) (Rectangle shape) in
      let newDrawing =
        copyBox (clearBox model.drawing (Rectangle shape)) r shape model
      in
      if allowedBoxLocation name updatedBox model then
        { model with
          shapes = IntMap.update name (fun _ -> Some updatedBox) model.shapes
        ; drawing = newDrawing
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

let drawMouseDown cx cy dm model =
  { model with drawMode = Some { dm with drawAtX = cx ; drawAtY = cy ; drawStartX = cx ; drawStartY = cy } }

let rec removeUntilMatch elt = function
  | [] -> None
  | hd :: tl ->
    match removeUntilMatch elt tl with
    | Some tl2 -> Some tl2
    | None ->
      if hd = elt then
        Some tl
      else
        None

let cursorMovePath nx ny cx cy path =
  match path with
  | [] -> [(nx,ny);(cx,cy)]
  | lst ->
    match removeUntilMatch (nx,ny) lst with
    | Some prev -> (nx,ny)::prev
    | _ -> (nx,ny)::path

let keyDownDrawMode k d model =
  let keyChar =
    if String.length k.key == 1 then
      Some (String.get k.key 0)
    else
      None
  in
  match keyChar with
  | None ->
    if k.keyCode == 13 then
      { model with
        drawMode = Some { d with drawAtX = d.drawStartX ; drawAtY = d.drawAtY + 1 }
      }
    else if k.keyCode == 8 then
      { model with
        drawing = IntPairMap.remove (d.drawAtY, d.drawAtX - 1) model.drawing
      ; drawMode = Some { d with drawAtX = max 0 (d.drawAtX - 1) }
      }
    else if k.keyCode == 37 then
      { model with
        drawMode =
          Some
            { d with
              drawAtX = max 0 (d.drawAtX - 1)
            ; linePath =
                d.linePath
                |> Option.map (cursorMovePath (d.drawAtX - 1) d.drawAtY d.drawAtX d.drawAtY)
            }
      }
    else if k.keyCode == 38 then
      { model with
        drawMode =
          Some
            { d with
              drawAtY = max 0 (d.drawAtY - 1)
            ; linePath =
                d.linePath
                |> Option.map (cursorMovePath d.drawAtX (d.drawAtY - 1) d.drawAtX d.drawAtY)
            }
      }
    else if k.keyCode == 39 then
      { model with
        drawMode =
          Some
            { d with
              drawAtX = d.drawAtX + 1
            ; linePath =
                d.linePath
                |> Option.map (cursorMovePath (d.drawAtX + 1) d.drawAtY d.drawAtX d.drawAtY)
            }
      }
    else if k.keyCode == 40 then
      { model with
        drawMode =
          Some
            { d with
              drawAtY = d.drawAtY + 1
            ; linePath =
                d.linePath
                |> Option.map (cursorMovePath d.drawAtX (d.drawAtY + 1) d.drawAtX d.drawAtY)
            }
      }
    else if k.keyCode == 16 then
      { model with
        drawMode = Some { d with linePath = Some [] }
      }
    else
      model

  | Some ch ->
    let code = Char.code ch in
    if code >= 0x20 && code < 0x7f then
      { model with
        drawing =
          IntPairMap.update
            (d.drawAtY, d.drawAtX)
            (fun _ -> Some ch)
            model.drawing
      ; drawMode = Some { d with drawAtX = d.drawAtX + 1 }
      ; prev = Some { model with drawMode = None }
      }
    else
      model

let renderLinePath lp drawing =
  List.fold_left
    (fun drawing (x,y) ->
       IntPairMap.update
         (y,x)
         (fun _ -> Some '*')
         drawing
    )
    drawing
    lp

let keyUpDrawMode k dm model =
  if k.keyCode == 16 then
    match dm.linePath with
    | Some lp ->
      { model with
        drawMode = Some { dm with linePath = None }
      ; drawing = renderLinePath lp model.drawing
      ; prev = Some { model with drawMode = None }
      }
    | None -> model
  else
    model

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
  | DrawMode ->
    { model with
      drawMode =
        Some
          { drawAtX = 0
          ; drawAtY = 0
          ; drawStartX = 0
          ; drawStartY = 0
          ; linePath = None
          }
    }
  | BoxMode -> { model with drawMode = None }
  | KeyMsg (KeyDown k) ->
    begin
      match model.drawMode with
      | Some d -> keyDownDrawMode k d model
      | _ -> model
    end
  | KeyMsg (KeyUp k) ->
    begin
      match model.drawMode with
      | Some d -> keyUpDrawMode k d model
      | _ -> model
    end
  | MouseMsg (MouseDown (x,y)) ->
    begin
      let cx = int_of_float @@ (float_of_int x) /. !fontWidth in
      let cy = (int_of_float @@ (float_of_int y) /. !fontHeight) - headerSize in
      match model.drawMode with
      | None -> { model with mouseAction = Some (Down (cx,cy)) }
      | Some dm -> drawMouseDown cx cy dm model
    end
  | MouseMsg (MouseMove (x,y)) ->
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
  | MouseMsg MouseUp ->
    begin
      match model.mouseAction with
      | None -> model
      | Some (Down (px,py)) -> finishClick px py { model with mouseAction = None }
      | Some (Drag r) -> finishDrag r { model with mouseAction = None }
    end
  | Help -> { model with help = true }
  | NoHelp -> { model with help = false }
  | _ -> model

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

let extractLines =
  List.fold_left
    (fun lmap ((y,x),ch) ->
       IntMap.update
         y
         (function
           | None -> Some (IntMap.add x ch IntMap.empty)
           | Some m -> Some (IntMap.add x ch m)
         )
         lmap
    )

let replaceInString x ch s =
  let slen = String.length s in
  if slen < x then
    (padTo x s) ^ (String.make 1 ch)
  else if slen == x then
    s ^ (String.make 1 ch)
  else
    (String.sub s 0 x) ^ (String.make 1 ch) ^ (String.sub s (x+1) (slen - x - 1))

let applyLines strings lmap =
  Array.mapi
    (fun i s ->
       try
         let charsInLine = IntMap.find i lmap in
         let charsArray =
           charsInLine
           |> IntMap.bindings
           |> Array.of_list
         in
         charsArray
         |> Array.fold_left (fun s (x,ch) -> replaceInString x ch s) s
       with _ -> s
    )
    strings

let applyPath model =
  match model.drawMode |> Option.bind (fun d -> d.linePath) with
  | Some lp -> renderLinePath lp model.drawing
  | _ -> model.drawing

let applyDrawing drawing strings =
  drawing
  |> IntPairMap.bindings
  |> extractLines IntMap.empty
  |> applyLines strings

let rerender model' =
  let model =
    match model'.mouseAction with
    | Some (Drag r) -> finishDrag r model'
    | _ -> model'
  in
  let upper_bound_shapes =
    model.shapes
    |> IntMap.bindings
    |> List.fold_left
      (fun ub -> function
        | (_, Rectangle r) -> max (r.top + r.height + 1) ub
      )
      0
  in
  let upper_bound_chars =
    try
      let ((y,x),_) = IntPairMap.max_binding model.drawing in
      y + 1
    with _ -> 0
  in
  let upper_bound = max upper_bound_shapes upper_bound_chars in
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
      |> applyDrawing (applyPath model)
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
  ; drawing = IntPairMap.empty
  ; serial = 1
  ; selectedForTyping = 0
  ; mouseAction = None
  ; editing = None
  ; drawMode = None
  ; prev = None
  ; help = false
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

let drawingCursorDiv model =
  match model.drawMode with
  | None ->
    div
      [ id "drawing-cursor"
      ; classList [("drawing-cursor",false)]
      ; noProp
      ; noProp
      ; noProp
      ] []
  | Some d ->
    let xloc = Printf.sprintf "%fpx" ((float_of_int d.drawAtX) *. !fontWidth) in
    let yloc = Printf.sprintf "%fpx" ((float_of_int (d.drawAtY + headerSize)) *. !fontHeight) in
    let width = Printf.sprintf "%fpx" !fontWidth in
    let height = Printf.sprintf "%fpx" !fontHeight in
    div
      [ id "drawing-cursor"
      ; classList [("drawing-cursor",true)]
      ; styles [ ("left", xloc) ; ("top", yloc) ; ("width", width) ; ("height", height) ]
      ; onKeyDown (keyData (fun (code,name) -> KeyDown { keyCode = code ; key = name }))
      ; onKeyUp (keyData (fun (code,name) -> KeyUp { keyCode = code ; key = name }))
      ] []

let controlsDiv model =
  match model.drawMode with
  | Some d ->
    div [ id "controls" ]
      [ text "controls"
      ; div [ classList [("control", true)] ; onClick BoxMode ] [ text "[ -> box mode ]" ]
      ; div [ classList [("control-spacer",true)] ] []
      ; div [ classList [("control", true)] ; onClick Help ] [ text "[ help ]" ]
      ]
  | _ ->
    div [ id "controls" ]
      [ text "controls"
      ; div [ classList [("control", true)] ; onClick DrawMode ] [ text "[ -> draw mode ]" ]
      ; div [ classList [("control", true)] ; onClick NewBox ] [ text "[ new box ]" ]
      ; div [ classList [("control", true)] ; onClick (DelBox model.selectedForTyping) ] [ text "[ delete box ]" ]
      ; div [ classList [("control", true)] ; onClick (Edit model.selectedForTyping) ] [ text "[ edit ]" ]
      ; div [ classList [("control", true)] ; onClick Undo ] [ text "[ undo ]" ]
      ; div [ classList [("control-spacer",true)] ] []
      ; div [ classList [("control", true)] ; onClick Help ] [ text "[ help ]" ]
      ]

let showHelp model =
  div
    [ classList
        [ ("help-container", model.help)
        ; ("hidden-help-container", not model.help)
        ]
    ]
    [ iframe [ classList [("help-frame",true)] ; src "./help.html" ] []
    ; div [ classList [("edit-control-bar",true)] ]
        [ div [ classList [("control-spacer",true)] ] []
        ; div [ classList [("control",true)] ; onClick NoHelp ] [ text "[ Ok ]" ]
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
    [ id "app-div"
    ]
    [ controlsDiv model
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
    ; drawingCursorDiv model
    ; showHelp model
    ; div
        [ id "edit" ; classList [ ("edit-active", model.editing <> None) ; ("edit-hidden", model.editing = None) ] ]
        (viewEditBody model)
    ]

let main =
  ignore @@ keySendToApp () ;
  beginnerProgram
    { model = init ()
    ; update = (fun model msg -> update model msg |> rerender)
    ; view = view
  }
