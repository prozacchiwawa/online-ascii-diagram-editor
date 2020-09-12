type mouseMsg =
  | MouseUp
  | MouseMove of (int * int)
  | MouseDown of (int * int)

type msg =
  | Noop
  | WinMsg of mouseMsg
  | NewBox
  | DelBox of int
[@@bs.deriving {accessors}]
