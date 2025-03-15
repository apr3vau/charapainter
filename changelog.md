# v0.2.1

- Use multiprocessing to accelerate drawing
- Fix a bug of undo overlapped selected region

# v0.2.0

Warning: This version has incompatible changes. Savings from old versions cannot be load.

Functions:
- Support multiple layers and alpha blending
- Support bold, italic and underline
- Support scaling drawing board
- Brush, Eraser and Stroke tools support painting to foreground, background or character selectively
- Rectangle tool support any combination of filling styles for border and inner area
- Stroke tool support connecting with surrounding characters
- Stroke tool support automatically adding arrow at the end
- Remove coloring tool as its function has been replaced by Brush
- Introduce new keyboard shortcuts: Option + arrow key to draw strokes, replace Ctrl-Space
- Support scale font size by touchpad zoom operation
- Add file icon and auto-opening for `.charap` file

UI:
- Using an echo-area below the drawing board to introduce shortcuts, replace list panel at left side
- Use dashed rectangle to mark selection border instead of characters
- Add grids to drawing board to indicate character edges
- Add preview images for brush, eraser, stroke and rectangle tools
- Add font size entry to toolbar
- Move "Copy to" option from toolbar to menu bar
- Add "Cursor movement" options to menu bar
- Remove MOTD

Fix:
- Fix an error for exporting images
- Fix mistakenly raised popup when exporting to HTML, ANSI & Text
- Fix an error for selecting area
- Fix an error of wrongly converted 8-bit colors

Internal:
- New structure: LAYER
- New slot for PIXEL: bold-p, italic-p, underline-p
- New slot for TERM-COLOR: alpha
- Rewrite saving functions to support multiple layers and new internal structure slots
- Move font family & font size settings and caches from global variable to project attribute
- Move foreground, background and character from global variable to drawing board attribute
- Use plain-text `.sexp` format to save settings
- Remove CL-PPCRE and ITERATE from dependencies
- Optimize loop functions to improve code efficiency
- Add more declarations for optimization
- ...
