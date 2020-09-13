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

type drawDrag
  = NoSelection of (int * int)
  | PrevSelection of (int * int * mousedown)

type draw =
  { drawAtX : int
  ; drawAtY : int
  ; drawStartX : int
  ; drawStartY : int
  ; drawMouseDown : bool
  ; dragEnd : drawDrag option
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
  ; ctrl : bool
  ; help : bool
  ; filename : string
  ; saveUrl : string option
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
  match dm.dragEnd with
  | Some (NoSelection (x,y)) ->
    let minX = min dm.drawAtX x in
    let minY = min dm.drawAtY y in
    let maxX = max dm.drawAtX x + 1 in
    let maxY = max dm.drawAtY y + 1 in
    if cx >= minX && cx <= maxX && cy >= minY && cy <= maxY then
      { model with
        drawMode =
          Some
            { dm with
              drawAtX = cx
            ; drawAtY = cy
            ; drawMouseDown = true
            ; dragEnd =
                Some
                  (PrevSelection (cx,cy,{ mx = minX ; my = minY ; nx = maxX ; ny = maxY }))
            }
      }
    else
      { model with
        drawMode =
          Some
            { dm with
              drawAtX = cx
            ; drawAtY = cy
            ; drawStartX = cx
            ; drawStartY = cy
            ; drawMouseDown = true
            ; dragEnd = None
            }
      }
  | _ ->
    { model with
      drawMode =
        Some
          { dm with
            drawAtX = cx
          ; drawAtY = cy
          ; drawStartX = cx
          ; drawStartY = cy
          ; drawMouseDown = true
          ; dragEnd = None
          }
    }

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
      match d.dragEnd with
      | Some (NoSelection (x,y)) ->
        let minX = min d.drawAtX x in
        let minY = min d.drawAtY y in
        let maxX = max d.drawAtX x in
        let maxY = max d.drawAtY y in
        { model with
          drawing = clearBox model.drawing (Rectangle { left = minX ; top = minY ; width = maxX - minX ; height = maxY - minY ; content = [| |] })
        ; drawMode = Some { d with dragEnd = None }
        ; prev = Some { model with drawMode = None }
        }
      | _ ->
        { model with
          drawing = IntPairMap.remove (d.drawAtY, d.drawAtX - 1) model.drawing
        ; drawMode = Some { d with drawAtX = max 0 (d.drawAtX - 1) }
        ; prev = Some { model with drawMode = None }
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
  | Some ' ' ->
    { model with
      drawing = IntPairMap.remove (d.drawAtY, d.drawAtX) model.drawing
    ; drawMode = Some { d with drawAtX = d.drawAtX + 1 }
    ; prev = Some { model with drawMode = None }
    }
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

type renderLinePathState =
  { horizontal : bool option
  ; prevX : int
  ; prevY : int
  }

let renderLinePath lp drawing =
  match lp with
  | [] -> drawing
  | (x,y) :: tl ->
    let (_,result) =
      List.fold_left
        (fun (prev,drawing) (x,y) ->
           if prev.horizontal = Some true && y <> prev.prevY then
             let next = { horizontal = Some false ; prevX = x ; prevY = y } in
             let nd =
               IntPairMap.update
                 (prev.prevY,prev.prevX)
                 (fun _ -> Some '+')
                 (IntPairMap.update
                    (y,x)
                    (fun _ -> Some '|')
                    drawing
                 )
             in
             (next,nd)
           else if prev.horizontal = Some false && x <> prev.prevX then
             let next = { horizontal = Some true ; prevX = x ; prevY = y } in
             let nd =
               IntPairMap.update
                 (prev.prevY,prev.prevX)
                 (fun _ -> Some '+')
                 (IntPairMap.update
                    (y,x)
                    (fun _ -> Some '-')
                    drawing
                 )
             in
             (next,nd)
           else if prev.horizontal = None && prev.prevX != x then
             let next = { horizontal = Some true ; prevX = x ; prevY = y } in
             let nd =
               IntPairMap.update
                 (prev.prevY,prev.prevX)
                 (fun _ -> Some '-')
                 (IntPairMap.update
                    (y,x)
                    (fun _ -> Some '-')
                    drawing
                 )
             in
             (next,nd)
           else if prev.horizontal = None && prev.prevY != y then
             let next = { horizontal = Some false ; prevX = x ; prevY = y } in
             let nd =
               IntPairMap.update
                 (prev.prevY,prev.prevX)
                 (fun _ -> Some '|')
                 (IntPairMap.update
                    (y,x)
                    (fun _ -> Some '|')
                    drawing
                 )
             in
             (next,nd)
           else
             let char = if prev.horizontal = Some true then '-' else '|' in
             let next = { prev with prevX = x ; prevY = y } in
             let nd =
               IntPairMap.update (y,x) (fun _ -> Some char) drawing
             in
             (next,nd)
        )
        ({ horizontal = None ; prevX = x ; prevY = y },drawing)
        tl
    in
    result

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

let finishDrawDrag sx sy (drag : mousedown) d model =
  let originalShape =
    { left = drag.mx
    ; top = drag.my
    ; width = drag.nx - drag.mx - 1
    ; height = drag.ny - drag.my - 1
    ; content = [| |]
    }
  in
  let clearedDrawing = clearBox model.drawing (Rectangle originalShape) in
  { model with
    drawing =
      copyBox
        clearedDrawing
        { nx = sx ; ny = sy ; mx = d.drawAtX ; my = d.drawAtY }
        originalShape
        model
  ; drawMode =
      Some
        { d with
          dragEnd =
            Some (NoSelection (drag.nx + sx - d.drawAtX - 1,drag.ny + sy - d.drawAtY - 1))
        ; drawMouseDown = false
        ; drawAtX = drag.mx + sx - d.drawAtX
        ; drawAtY = drag.my + sy - d.drawAtY
        }
  ; prev = Some { model with drawMode = None }
  }

let undo model =
  match model.prev with
  | Some p -> p
  | None -> model

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

let applyLine s charsInLine =
  let charsArray =
    charsInLine
    |> IntMap.bindings
    |> Array.of_list
  in
  charsArray
  |> Array.fold_left (fun s (x,ch) -> replaceInString x ch s) s

let saveFile model =
  let encodedShapes =
    model.shapes
    |> IntMap.bindings
    |> List.map
      (fun (id,shape) ->
         match shape with
         | Rectangle r ->
           [ ("type", Js.Json.string "rectangle")
           ; ("id", Js.Json.number (float_of_int id))
           ; ("top", Js.Json.number (float_of_int r.top))
           ; ("left", Js.Json.number (float_of_int r.left))
           ; ("height", Js.Json.number (float_of_int r.height))
           ; ("width", Js.Json.number (float_of_int r.width))
           ; ("content", Js.Json.string (String.concat "\n" (Array.to_list r.content)))
           ]
           |> Js.Dict.fromList
           |> Js.Json.object_
      )
    |> Array.of_list
  in
  let encodedPlane =
    model.drawing
    |> IntPairMap.bindings
    |> extractLines IntMap.empty
    |> IntMap.bindings
    |> List.map
      (fun (y,row) ->
         [ ("y", Js.Json.number (float_of_int y))
         ; ("row", Js.Json.string (applyLine "" row))
         ]
         |> Js.Dict.fromList
         |> Js.Json.object_
      )
    |> Array.of_list
  in
  let encoded =
    [ ("name", Js.Json.string model.filename)
    ; ("shapes", Js.Json.array encodedShapes)
    ; ("charplane", Js.Json.array encodedPlane)
    ]
    |> Js.Dict.fromList
    |> Js.Json.object_
  in
  let encodedString = Js.Json.stringify encoded in
  let f = File.newFile [| encodedString |] model.filename (File.props "application/json") in
  let u = File.createObjectURLFromFile f in
  { model with saveUrl = Some u }

let decodeInt js = Js.Json.decodeNumber js |> Option.map int_of_float

let decodeShape dict =
  let _ = Js.log "decodeShape" in
  let _ = Js.log dict in
  let type_ =
    Js.Dict.get dict "type"
    |> Option.bind Js.Json.decodeString
  in
  let id =
    Js.Dict.get dict "id"
    |> Option.bind decodeInt
  in
  let top =
    Js.Dict.get dict "top"
    |> Option.bind decodeInt
  in
  let left =
    Js.Dict.get dict "left"
    |> Option.bind decodeInt
  in
  let height =
    Js.Dict.get dict "height"
    |> Option.bind decodeInt
  in
  let width =
    Js.Dict.get dict "width"
    |> Option.bind decodeInt
  in
  let content =
    Js.Dict.get dict "content"
    |> Option.bind Js.Json.decodeString
    |> Option.map (split "\n")
  in
  match (type_, id, top, left, height, width, content) with
  | (Some "rectangle", Some id, Some top, Some left, Some height, Some width, Some content) ->
    Some
      ( id
      , Rectangle
          { top = top
          ; left = left
          ; height = height
          ; width = width
          ; content = content
          }
      )
  | _ -> None

let charLineToPair y row =
  let slen = String.length row in
  range 0 slen
  |> List.map
    (fun i ->
       let ch = String.get row i in
       if ch <> ' ' then
         Some ((y,i),ch)
       else
         None
    )
  |> catOptions

let decodeCharLine dict =
  let y = Js.Dict.get dict "y" |> Option.bind decodeInt in
  let row = Js.Dict.get dict "row" |> Option.bind Js.Json.decodeString in
  match (y,row) with
  | (Some y, Some row) ->
    Some (charLineToPair y row)
  | _ -> None

let loadFile name data model =
  let _ = Printf.printf "loadFile %s" name in
  let _ = Js.log data in
  let name =
    Js.Dict.get data "name"
    |> Option.bind Js.Json.decodeString
    |> Option.default "drawing.json"
  in
  let shapes =
    Js.Dict.get data "shapes"
    |> Option.bind Js.Json.decodeArray
    |> Option.default [| |]
    |> Array.to_list
    |> List.map Js.Json.decodeObject
    |> catOptions
    |> List.map decodeShape
    |> catOptions
    |> List.fold_left
      (fun shmap (id,shape) -> IntMap.add id shape shmap)
      IntMap.empty
  in
  let drawing =
    Js.Dict.get data "charplane"
    |> Option.bind Js.Json.decodeArray
    |> Option.default [| |]
    |> Array.to_list
    |> List.map Js.Json.decodeObject
    |> catOptions
    |> List.map decodeCharLine
    |> catOptions
    |> List.concat
    |> List.fold_left
      (fun chmap (pt,ch) -> IntPairMap.add pt ch chmap)
      IntPairMap.empty
  in
  let serial =
    try
      shapes
      |> IntMap.max_binding
      |> (fun (id,_) -> id + 1)
    with _ -> 2
  in
  { model with
    shapes = shapes ;
    drawing = drawing ;
    filename = name ;
    serial = serial ;
    prev = None
  }

let applyShape decorate model i s = function
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
      let hfill = if model.selectedForTyping == name && decorate then '=' else '-' in
      if i == r.top then
        left_of ^ "," ^ (String.make (r.width - 1) hfill) ^ "." ^ right_of
      else if i >= r.top && i < (r.top + r.height) then
        let boxIndex = i - r.top - 1 in
        let boxLine =
          if boxIndex < Array.length r.content then
            let blString = " " ^ Array.get r.content boxIndex in
            if String.length blString > r.width - 2 then
              String.sub blString 0 (r.width - 2)
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

let applyLines strings lmap =
  Array.mapi
    (fun i s ->
       try
         let charsInLine = IntMap.find i lmap in
         applyLine s charsInLine
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

let renderDrawDrag model =
  match model.drawMode with
  | None -> model
  | Some d ->
    match d.dragEnd with
    | Some (PrevSelection (x,y,rect)) -> finishDrawDrag x y rect d model
    | _ -> model

let rerender decorate model' =
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
           |> List.fold_left (applyShape decorate model i) ""
        )
      |> Array.of_list
      |> applyDrawing (applyPath (renderDrawDrag model))
  }

let exportFile model =
  let renderModel = rerender false model in
  let rendered =
    Array.to_list renderModel.rendered
    |> String.concat "\n"
  in
  let filenameSplit = split "." model.filename in
  let _ = Array.set filenameSplit ((Array.length filenameSplit) - 1) ".txt" in
  let outputFilename = String.concat "." (Array.to_list filenameSplit) in
  let f = File.newFile [| rendered |] outputFilename (File.props "text/plain") in
  let u = File.createObjectURLFromFile f in
  { model with saveUrl = Some u }

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
  | Undo -> undo model
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
          ; drawMouseDown = false
          ; linePath = None
          ; dragEnd = None
          }
    }
  | BoxMode -> { model with drawMode = None }
  | KeyMsg (KeyDown k) ->
    begin
      let _ = Js.log "keydown" in
      let _ = Js.log k in
      if k.keyCode == 17 then
        { model with ctrl = true }
      else if model.ctrl then
        if k.key == "z" then
          let undid = undo model in
          { undid with ctrl = true }
        else
          model
      else
        match model.drawMode with
        | Some d -> keyDownDrawMode k d model
        | _ -> model
    end
  | KeyMsg (KeyUp k) ->
    begin
      if k.keyCode == 17 then
        { model with ctrl = false }
      else
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
      match model.drawMode with
      | Some d ->
        if d.drawAtX == cx && d.drawAtY == cy then
          model
        else if d.drawMouseDown then
          match d.dragEnd with
          | None ->
            { model with drawMode = Some { d with dragEnd = Some (NoSelection (cx,cy)) } }
          | Some (NoSelection (_,_)) ->
            let _ = Printf.printf "change sel %dx%d\n" cx cy in
            { model with drawMode = Some { d with dragEnd = Some (NoSelection (cx,cy)) } }
          | Some (PrevSelection (_,_,rect)) ->
            let _ = Printf.printf "move sel %dx%d\n" cx cy in
            { model with drawMode = Some { d with dragEnd = Some (PrevSelection (cx,cy,rect)) } }
        else
          model
      | _ ->
        match model.mouseAction with
        | None -> model
        | Some (Drag r) ->
          { model with mouseAction = Some (Drag { r with nx = cx ; ny = cy }) }
        | Some (Down (px,py)) ->
          if px == cx && py == cy then
            model
          else
            { model with mouseAction = Some (Drag { mx = px ; my = py ; nx = cx ; ny = cy }) }
    end
  | MouseMsg MouseUp ->
    begin
      let _ = Js.log "mouse up" in
      match model.drawMode with
      | Some d ->
        begin
          match d.dragEnd with
          | Some (PrevSelection (sx,sy,drag)) -> finishDrawDrag sx sy drag d model
          | _ ->
            { model with
              drawMode =
                Some { d with drawMouseDown = false }
            ; mouseAction = None
            }
        end
      | _ ->
        match model.mouseAction with
        | None -> model
        | Some (Down (px,py)) -> finishClick px py { model with mouseAction = None }
        | Some (Drag r) -> finishDrag r { model with mouseAction = None }
    end
  | Help -> { model with help = true }
  | NoHelp -> { model with help = false }
  | Save -> saveFile model
  | Export -> exportFile model
  | EndSave url ->
    File.revokeObjectURL url ;
    { model with saveUrl = None }
  | Load (name,data) -> loadFile name data model
  | _ -> model

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
  ; ctrl = false
  ; help = false
  ; filename = "drawing.json"
  ; saveUrl = None
  }
  |> rerender true

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

let dragOfDrawMode model =
  model.drawMode
  |> Option.map
    (fun d ->
       match d.dragEnd with
       | Some (NoSelection (ex,ey)) ->
         { mx = min d.drawAtX ex
         ; my = min d.drawAtY ey
         ; nx = (max d.drawAtX ex) + 1
         ; ny = (max d.drawAtY ey) + 1
         }
       | Some (PrevSelection (ex,ey,drag)) ->
         { mx = drag.mx + ex - d.drawAtX
         ; my = drag.my + ey - d.drawAtY
         ; nx = drag.nx + ex - d.drawAtX
         ; ny = drag.ny + ey - d.drawAtY
         }
       | _ ->
         { mx = d.drawAtX
         ; my = d.drawAtY
         ; nx = d.drawAtX + 1
         ; ny = d.drawAtY + 1
         }
    )

let drawingCursorDiv model =
  match dragOfDrawMode model with
  | None ->
    div
      [ id "drawing-cursor"
      ; classList [("drawing-cursor",false)]
      ; noProp
      ; noProp
      ; noProp
      ] []
  | Some drag ->
    let _ = Js.log drag in
    let xmin = min drag.mx drag.nx in
    let ymin = min drag.my drag.ny in
    let xmax = max drag.mx drag.nx in
    let ymax = max drag.my drag.ny in
    let xloc = Printf.sprintf "%fpx" ((float_of_int xmin) *. !fontWidth) in
    let yloc = Printf.sprintf "%fpx" ((float_of_int (ymin + headerSize)) *. !fontHeight) in
    let width = Printf.sprintf "%fpx" ((float_of_int (xmax - xmin)) *. !fontWidth) in
    let height = Printf.sprintf "%fpx" ((float_of_int (ymax - ymin)) *. !fontHeight) in
    div
      [ id "drawing-cursor"
      ; classList [("drawing-cursor",true)]
      ; styles [ ("left", xloc) ; ("top", yloc) ; ("width", width) ; ("height", height) ]
      ] []

let controlsDiv model =
  match model.drawMode with
  | Some d ->
    div [ id "controls" ]
      [ text "controls"
      ; div [ classList [("control", true)] ; onClick Save ] [ text "[ save ]" ]
      ; div [ classList [("control", true)] ; onClick Export ] [ text "[ export ]" ]
      ; div [ classList [("control", true)] ; onClick BoxMode ] [ text "[ -> box mode ]" ]
      ; div [ classList [("control", true)] ; onClick Undo ] [ text "[ undo ]" ]
      ; div [ classList [("control-spacer",true)] ] []
      ; div [ classList [("control", true)] ; onClick Help ] [ text "[ help ]" ]
      ]
  | _ ->
    div [ id "controls" ]
      [ text "controls"
      ; div [ classList [("control", true)] ; onClick Save ] [ text "[ save ]" ]
      ; div [ classList [("control", true)] ; onClick Export ] [ text "[ export ]" ]
      ; div [ classList [("control", true)] ; onClick DrawMode ] [ text "[ -> draw mode ]" ]
      ; div [ classList [("control", true)] ; onClick NewBox ] [ text "[ new box ]" ]
      ; div [ classList [("control", true)] ; onClick (DelBox model.selectedForTyping) ] [ text "[ delete box ]" ]
      ; div [ classList [("control", true)] ; onClick (Edit model.selectedForTyping) ] [ text "[ edit ]" ]
      ; div [ classList [("control", true)] ; onClick Undo ] [ text "[ undo ]" ]
      ; div [ classList [("control-spacer",true)] ] []
      ; div [ classList [("control", true)] ; onClick Help ] [ text "[ help ]" ]
      ]

let showSave model =
  div
    [ classList
        [ ("save-container", model.saveUrl <> None)
        ; ("save-container-hidden", model.saveUrl = None)
        ]
    ]
    (match model.saveUrl with
     | None -> [ div [ id "save-box-container" ] [] ]
     | Some link ->
       [ div
           [ id "save-box-container" ]
           [ div [ id "save-box-link" ] [ a [ href link ; target "_blank" ] [ text "Save Link" ] ]
           ; div
               [ classList [("edit-control-bar",true)] ]
               [ div [ classList [("control-spacer",true)] ] []
               ; div
                   [ classList [("control",true)] ; onClick (EndSave link) ]
                   [ text "[ Done ]" ]
               ]
           ]
       ]
    )

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
    ; div
        [ id "ruler-container"
        ; onKeyDown (keyData (fun (code,name) -> KeyDown { keyCode = code ; key = name }))
        ; onKeyUp (keyData (fun (code,name) -> KeyUp { keyCode = code ; key = name }))
        ]
        [ div [ id "ruler" ; classList [ ("dwg-row",true) ] ] [ text @@ ruler () ]
        ]
    ; div
        []
        (
          rendered_show
          |> Array.map (fun t -> pre [ classList [("dwg-row",true)] ] [ text t ])
          |> Array.to_list
        )
    ; drawingCursorDiv model
    ; showHelp model
    ; showSave model
    ; div
        [ id "mousecover"
        ; onMouseDown (moveMouse (fun p -> MouseDown p))
        ; onMouseMove (moveMouse (fun p -> MouseMove p))
        ; onMouseUp upMouse
        ; File.onFile File.takeFile
        ; File.onDrop File.dropFile
        ; File.onDragOver File.dragOver
        ] []
    ; div
        [ id "edit" ; classList [ ("edit-active", model.editing <> None) ; ("edit-hidden", model.editing = None) ] ]
        (viewEditBody model)
    ]

let main =
  ignore @@ keySendToApp () ;
  beginnerProgram
    { model = init ()
    ; update = (fun model msg -> update model msg |> rerender true)
    ; view = view
  }
