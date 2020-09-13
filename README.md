# online ascii diagram editor

![screenshot](https://raw.githubusercontent.com/prozacchiwawa/online-ascii-diagram-editor/base/screenshot.png)

An experiment in making an editor that has the features I use from emacs' picture mode.

## Files

You can drag a saved file onto the app to load it.

```[ Save ]``` will present a link that contains the app's internal state.  You can
reload the same data to resume editing.

```[ Export ]``` will present a link that contains the rendered view as text.

## Box mode

In box mode, you can drag and create boxes with the mouse.
The box with === in its top and bottom border is the selected box. 

You can edit its contents with ```[ edit ]```

You can drag boxes.

You can resize boxes by dragging the lower right corner.

## Draw mode

In draw mode, you can add characters to the drawing, on top of and between boxes.

Type anything at the cursor.

Hold down shift and move the cursor to draw diagram lines.

You can place the cursor with the mouse and use the arrows to move.

You can drag open a selection rectangle and move it with the mouse or delete the
contents with the backspace key.
