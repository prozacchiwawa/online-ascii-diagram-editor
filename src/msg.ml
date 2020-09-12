type mouseMsg =
  | MouseUp
  | MouseMove of (int * int)
  | MouseDown of (int * int)

type msg =
  | Noop
  | WinMsg of mouseMsg
  | NewBox
  | DelBox of int
  | Edit of int
  | Undo
  | CancelEdit
  | ChangeText of (int * string)
  | UseEdit of (int * string)
[@@bs.deriving {accessors}]
