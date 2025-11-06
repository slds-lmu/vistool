# Base visualizer class

Base class for all visualizers. Provides a common interface for creating
and saving plots across different plotting backends (ggplot2 for 1D/2D,
plotly for 3D).

## Methods

### Public methods

- [`Visualizer$set_theme()`](#method-Visualizer-set_theme)

- [`Visualizer$theme()`](#method-Visualizer-theme)

- [`Visualizer$plot()`](#method-Visualizer-plot)

- [`Visualizer$resolve_layer_colors()`](#method-Visualizer-resolve_layer_colors)

- [`Visualizer$save()`](#method-Visualizer-save)

- [`Visualizer$add_points()`](#method-Visualizer-add_points)

- [`Visualizer$add_contours()`](#method-Visualizer-add_contours)

- [`Visualizer$add_annotation()`](#method-Visualizer-add_annotation)

- [`Visualizer$clone()`](#method-Visualizer-clone)

------------------------------------------------------------------------

### Method `set_theme()`

Set the instance theme (partial override stored separately)

#### Usage

    Visualizer$set_theme(theme)

#### Arguments

- `theme`:

  (`list`) Partial theme created with vistool_theme() or a named list

#### Returns

Invisible self

------------------------------------------------------------------------

### Method `theme()`

Get the instance theme (merged with current package default)

#### Usage

    Visualizer$theme()

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Base plot method that sets up common plot settings and resolves layer
colors. This method should be called by all child classes via
`super$plot(...)`.

#### Usage

    Visualizer$plot(
      theme = NULL,
      show_title = TRUE,
      plot_title = NULL,
      plot_subtitle = NULL,
      show_legend = TRUE,
      legend_title = NULL,
      x_lab = NULL,
      y_lab = NULL,
      z_lab = NULL,
      x_limits = NULL,
      y_limits = NULL,
      z_limits = NULL,
      latex = NULL
    )

#### Arguments

- `theme`:

  (`list`)  
  Partial theme override for this render; see vistool_theme().

- `show_title`:

  (`logical(1)`)  
  Indicator whether to show the title of the plot.

- `plot_title`:

  (`character(1)`)  
  Main plot title. If NULL, uses the visualizer's default title. Default
  is NULL.

- `plot_subtitle`:

  (`character(1)`)  
  Plot subtitle. If NULL, no subtitle is shown. Default is NULL.

- `show_legend`:

  (`logical(1)`)  
  Whether to show the legend. Default is TRUE.

- `legend_title`:

  (`character(1)`)  
  Title for the legend. If NULL, uses default based on visualizer type.
  Default is NULL.

- `x_lab`:

  (`character(1)`)  
  X-axis label. If NULL, uses the visualizer's default label. Default is
  NULL.

- `y_lab`:

  (`character(1)`)  
  Y-axis label. If NULL, uses the visualizer's default label. Default is
  NULL.

- `z_lab`:

  (`character(1)`)  
  Z-axis label (for surface plots). If NULL, uses the visualizer's
  default label. Default is NULL.

- `x_limits`:

  (`numeric(2)`)  
  X-axis limits as c(min, max). If NULL, uses automatic limits. Default
  is NULL.

- `y_limits`:

  (`numeric(2)`)  
  Y-axis limits as c(min, max). If NULL, uses automatic limits. Default
  is NULL.

- `z_limits`:

  (`numeric(2)`)  
  Z-axis limits as c(min, max) (for surface plots). If NULL, uses
  automatic limits. Default is NULL.

- `latex`:

  (`NULL`\|`logical(1)`\|`list`) Controls LaTeX parsing for plot text.
  Supply `NULL` to auto-detect LaTeX markers, a logical scalar to toggle
  parsing globally, or a named list (keys such as `title`, `subtitle`,
  `legend`, `x`, `y`, `z`) to override individual components.

#### Returns

Invisible self for method chaining (child classes handle actual plot
creation).

------------------------------------------------------------------------

### Method `resolve_layer_colors()`

Resolve automatic color assignments in stored layers. This method should
be called by child classes after rendering layers.

#### Usage

    Visualizer$resolve_layer_colors()

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the plot to a file. The format is determined by the file extension.

#### Usage

    Visualizer$save(filename, width = NULL, height = NULL, dpi = 300, ...)

#### Arguments

- `filename`:

  (`character(1)`)  
  The filename to save the plot to. The file extension determines the
  format.

- `width`:

  (`numeric(1)`)  
  Width of the plot in pixels (for plotly) or inches (for ggplot2).

- `height`:

  (`numeric(1)`)  
  Height of the plot in pixels (for plotly) or inches (for ggplot2).

- `dpi`:

  (`numeric(1)`)  
  Resolution for ggplot2 plots (ignored for plotly plots).

- `...`:

  Additional arguments passed to the underlying save function.

------------------------------------------------------------------------

### Method `add_points()`

Add points to the plot. This method can be called multiple times to add
different sets of points.

#### Usage

    Visualizer$add_points(
      points,
      color = "auto",
      size = NULL,
      shape = 19,
      alpha = NULL,
      annotations = NULL,
      annotation_size = NULL,
      ordered = FALSE,
      arrow_color = NULL,
      arrow_size = 0.3,
      annotations_latex = NULL
    )

#### Arguments

- `points`:

  (`data.frame` or `matrix` or `list`)  
  The points to add.

  - For 1D: A `data.frame` or `matrix` with one column for x-values, or
    a numeric vector of x-values. If y-values are not provided, they
    will be inferred if possible (e.g., for objective functions).

  - For 2D/Surface: A `data.frame` or `matrix` with two columns (x1,
    x2), or a list of 2-element vectors.

- `color`:

  (`character(1)`)  
  Color of the points. Use "auto" for automatic color assignment from
  palette. Default is "auto".

- `size`:

  (`numeric(1)`)  
  Size of the points. If NULL, uses theme\$point_size. Default is NULL.

- `shape`:

  (`integer(1)` or `character(1)`)  
  Shape of the points. For ggplot2: integer codes (e.g., 19 for solid
  circle). For plotly: shape names. Default is 19/"circle".

- `alpha`:

  (`numeric(1)`)  
  Alpha transparency of the points. If NULL, uses theme\$alpha. Default
  is NULL.

- `annotations`:

  (`character`)  
  Optional text labels for each point. If provided, must be the same
  length as the number of points.

- `annotation_size`:

  (`numeric(1)`)  
  Size of annotation text. If NULL, defaults to text_size - 2 from
  plot().

- `ordered`:

  (`logical(1)`)  
  If `TRUE`, draws arrows between consecutive points to indicate order.
  Default is `FALSE`.

- `arrow_color`:

  (`character(1)`)  
  Color of arrows when ordered = TRUE. If NULL, uses point color.

- `arrow_size`:

  (`numeric(1)`)  
  Length/size of arrows when `ordered = TRUE`. Default is 0.3 units in
  the coordinate system.

- `annotations_latex`:

  (`NULL`\|[`logical()`](https://rdrr.io/r/base/logical.html))  
  Optional LaTeX flags for point annotations. Supply a single logical
  value to apply to all annotations or a vector matching `annotations`.

------------------------------------------------------------------------

### Method `add_contours()`

Add contour lines for grid-based visualizers (2D ggplot backends).

#### Usage

    Visualizer$add_contours(
      breaks = NULL,
      bins = NULL,
      binwidth = NULL,
      color = "auto",
      linewidth = NULL,
      linetype = "solid",
      alpha = NULL,
      mode = c("scale", "single"),
      palette = NULL,
      ...
    )

#### Arguments

- `breaks`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Numeric vector of
  contour levels. Passed to
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).
  Default `NULL`.

- `bins`:

  (`integer(1)`) Number of contour bins when `breaks` is not supplied.
  Default `NULL` (ggplot default).

- `binwidth`:

  (`numeric(1)`) Spacing between contour levels. Ignored when `breaks`
  supplied. Default `NULL`.

- `color`:

  (`character(1)`) Line color used when `mode = "single"`. Set to "auto"
  to draw from the active palette or provide an explicit hex/name.
  Ignored when `mode = "scale"`.

- `linewidth`:

  (`numeric(1)`) Line width. Falls back to the active theme when `NULL`.

- `linetype`:

  (`character(1)`) Line type for contour strokes. Default "solid".

- `alpha`:

  (`numeric(1)`) Opacity in `[0, 1]`. Uses the active theme when `NULL`.

- `mode`:

  (`character(1)`) Either "scale" (default) to colour individual contour
  levels using a continuous palette or "single" to draw all contours in
  the same colour.

- `palette`:

  (`character(1)`) Optional palette override ("viridis", "plasma",
  "grayscale") when `mode = "scale"`.

- `...`:

  Additional arguments forwarded to
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

#### Returns

Invisible self.

------------------------------------------------------------------------

### Method `add_annotation()`

Add a textual annotation that is resolved during plotting.

#### Usage

    Visualizer$add_annotation(
      text,
      latex = FALSE,
      color = "auto",
      size = NULL,
      opacity = NULL,
      x = NULL,
      y = NULL,
      z = NULL,
      position = NULL,
      hjust = 0.5,
      vjust = 0.5
    )

#### Arguments

- `text`:

  (`character(1)`) The annotation text. Must be non-empty.

- `latex`:

  (`logical(1)`) If `TRUE`, interpret `text` as LaTeX. Requires the
  `latex2exp` package for ggplot backends.

- `color`:

  (`character(1)`) Text color. Use "auto" to draw from the active theme
  palette. Defaults to "auto".

- `size`:

  (`numeric(1)`) Text size in points. Defaults to the effective theme
  `text_size`.

- `opacity`:

  (`numeric(1)`) Text opacity in `[0, 1]`. Defaults to the effective
  theme `alpha`.

- `x`:

  (`numeric(1)`) Absolute x-coordinate for the annotation. Required
  unless `position` is supplied.

- `y`:

  (`numeric(1)`) Absolute y-coordinate. Optional for 1D visualizers,
  required for 2D. Ignored when `position` is used with figure
  coordinates.

- `z`:

  (`numeric(1)`) Absolute z-coordinate for surface visualizers. Optional
  otherwise.

- `position`:

  ([`list()`](https://rdrr.io/r/base/list.html)\|`NULL`)  
  Optional relative placement specification for the annotation. Provide
  a named list with entries `x`, `y`, `z` (depending on dimensionality)
  holding values in `[0, 1]` and an optional `reference` that is either
  `"panel"` (default, data coordinates) or `"figure"` (normalized figure
  coordinates). When `position` is supplied, do not pass absolute
  coordinates via `x`, `y`, or `z`.

- `hjust`:

  (`numeric(1)`) Horizontal justification, following ggplot2 semantics.

- `vjust`:

  (`numeric(1)`) Vertical justification, following ggplot2 semantics.

#### Returns

Invisible self.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Visualizer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
