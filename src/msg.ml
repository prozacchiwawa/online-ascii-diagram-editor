type mouseMsg =
  | MouseUp
  | MouseMove of (int * int)
  | MouseDown of (int * int)

type key =
  { keyCode : int
  ; key : string
  }

type keyMsg =
  | KeyUp of key
  | KeyDown of key

type msg =
  | Noop
  | MouseMsg of mouseMsg
  | KeyMsg of keyMsg
  | NewBox
  | DelBox of int
  | Edit of int
  | Undo
  | BoxMode
  | DrawMode
  | CancelEdit
  | ChangeText of (int * string)
  | UseEdit of (int * string)
[@@bs.deriving {accessors}]
