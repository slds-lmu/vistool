# vistool theming utilities

Lightweight theme model and helpers to manage plotting style in a single
place.

## Usage

``` r
vistool_theme(
  palette = "viridis",
  text_size = 11,
  theme = "minimal",
  alpha = 0.8,
  line_width = 1.2,
  point_size = 2,
  legend_position = "right",
  show_grid = TRUE,
  grid_color = "gray90",
  background = "white"
)
```

## Arguments

- palette:

  Character. Color palette to use. One of "viridis", "plasma", or
  "grayscale".

- text_size:

  Numeric. Base text size for plots.

- theme:

  Character. ggplot2 theme to use. One of "minimal", "bw", "classic",
  "gray", "grey", "light", "dark", or "void".

- alpha:

  Numeric. Transparency level (0-1).

- line_width:

  Numeric. Default line width for plots.

- point_size:

  Numeric. Default point size for plots.

- legend_position:

  Character. Position of legend. One of "top", "right", "bottom",
  "left", or "none".

- show_grid:

  Logical. Whether to show grid lines.

- grid_color:

  Character. Color of grid lines.

- background:

  Character. Background color of plots.

## Examples

``` r
th = vistool_theme(palette = "plasma", text_size = 12)
```
