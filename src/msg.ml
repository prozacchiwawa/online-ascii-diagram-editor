type mouseMsg =
  | MouseUp
  | MouseMove of (int * int)
  | MouseDown of (int * int)

type msg =
  | Noop
  | WinMsg of mouseMsg
[@@bs.deriving {accessors}]
