# Visualize 2D functions as interactive surfaces

Visualizes a two-dimensional function \\f: \mathbb{R}^2 \to \mathbb{R}\\
via interactive plotly renderings. Creates 3D surface and contour plots
for better visualization of 2D functions.

## Super class

[`vistool::Visualizer`](https://slds-lmu.github.io/vistool/reference/Visualizer.md)
-\> `VisualizerSurface`

## Public fields

- `grid`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List with the `x1` and `x2` grid.

- `zmat`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  The result of evaluation at each element of the cross product of
  `grid$x1` and `grid$x2`.

- `plot_lab`:

  (character(1)  
  Label of the plot.

- `x1_lab`:

  (character(1)  
  Label of the x1 axis.

- `x2_lab`:

  (character(1)  
  Label of the x2 axis.

- `z_lab`:

  (character(1)  
  Label of the z axis.

## Methods

### Public methods

- [`VisualizerSurface$new()`](#method-VisualizerSurface-new)

- [`VisualizerSurface$init_layer_contour()`](#method-VisualizerSurface-init_layer_contour)

- [`VisualizerSurface$init_layer_surface()`](#method-VisualizerSurface-init_layer_surface)

- [`VisualizerSurface$set_layout()`](#method-VisualizerSurface-set_layout)

- [`VisualizerSurface$set_scene()`](#method-VisualizerSurface-set_scene)

- [`VisualizerSurface$add_contours()`](#method-VisualizerSurface-add_contours)

- [`VisualizerSurface$plot()`](#method-VisualizerSurface-plot)

- [`VisualizerSurface$clone()`](#method-VisualizerSurface-clone)

Inherited methods

- [`vistool::Visualizer$add_annotation()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_annotation)
- [`vistool::Visualizer$add_points()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_points)
- [`vistool::Visualizer$resolve_layer_colors()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-resolve_layer_colors)
- [`vistool::Visualizer$save()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-save)
- [`vistool::Visualizer$set_theme()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-set_theme)
- [`vistool::Visualizer$theme()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-theme)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    VisualizerSurface$new(
      grid,
      zmat,
      plot_lab = NULL,
      x1_lab = "x1",
      x2_lab = "x2",
      z_lab = "y"
    )

#### Arguments

- `grid`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List with the `x1` and `x2` grid.

- `zmat`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  The result of evaluation at each element of the cross product of
  `grid$x1` and `grid$x2`.

- `plot_lab`:

  (`character(1)`)  
  Label of the plot.

- `x1_lab`:

  (`character(1)`)  
  Label of the x1 axis.

- `x2_lab`:

  (`character(1)`)  
  Label of the x2 axis.

- `z_lab`:

  (`character(1)`)  
  Label of the z axis.

------------------------------------------------------------------------

### Method `init_layer_contour()`

Initialize the plot as 2D contour. This method is called automatically
by plot() and should not be called directly.

#### Usage

    VisualizerSurface$init_layer_contour(
      opacity = NULL,
      colorscale = NULL,
      show_title = TRUE,
      ...
    )

#### Arguments

- `opacity`:

  (`numeric(1)`)  
  Opacity of the contour plot (0-1). If NULL, uses theme default.

- `colorscale`:

  (`list`)  
  Color scale for the contour plot. If NULL or "auto", uses theme
  palette.

- `show_title`:

  (`logical(1)`)  
  Whether to show the plot title. Default is TRUE.

- `...`:

  (`any`)  
  Further arguments passed to `add_trace(...)`.

- `...`:

  (`any`)  
  Further arguments passed to `add_trace(...)`.

------------------------------------------------------------------------

### Method `init_layer_surface()`

Initialize the plot as 3D surface. This method is called automatically
by plot() and should not be called directly.

#### Usage

    VisualizerSurface$init_layer_surface(
      opacity = NULL,
      colorscale = NULL,
      show_title = TRUE,
      ...
    )

#### Arguments

- `opacity`:

  (`numeric(1)`)  
  Opacity of the surface plot (0-1). If NULL, uses theme default.

- `colorscale`:

  (`list`)  
  Color scale for the surface plot. If NULL or "auto", uses theme
  palette.

- `show_title`:

  (`logical(1)`)  
  Whether to show the plot title. Default is TRUE.

- `...`:

  (`any`)  
  Further arguments passed to `add_trace(...)`.

- `...`:

  (`any`)  
  Further arguments passed to `add_trace(...)`.

------------------------------------------------------------------------

### Method `set_layout()`

Set the layout of the plotly plot. This method is used internally by
plot(layout = ...) and should not be called directly.

#### Usage

    VisualizerSurface$set_layout(...)

#### Arguments

- `...`:

  Layout options directly passed to `layout(...)`.

------------------------------------------------------------------------

### Method `set_scene()`

Set the view for a 3D plot. This method is used internally by plot(scene
= ...) and should not be called directly.

#### Usage

    VisualizerSurface$set_scene(x, y, z)

#### Arguments

- `x`:

  (`numeric(1)`) The view from which the "camera looks down" to the
  plot.

- `y`:

  (`numeric(1)`) The view from which the "camera looks down" to the
  plot.

- `z`:

  (`numeric(1)`) The view from which the "camera looks down" to the
  plot.

------------------------------------------------------------------------

### Method `add_contours()`

Add contours to the surface plot.

#### Usage

    VisualizerSurface$add_contours(contours = NULL, ...)

#### Arguments

- `contours`:

  ([`list()`](https://rdrr.io/r/base/list.html) or `NULL`)  
  Custom contour configuration. If `NULL` (default), adds default
  z-projected contours. Can specify x, y, and z contours with custom
  properties like start, end, size, and color. See plotly documentation
  for detailed contour options.

- `...`:

  Additional arguments passed to the contour trace.

#### Returns

Self (invisibly) for method chaining.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Create and return the plotly plot with model-specific layers.

#### Usage

    VisualizerSurface$plot(flatten = FALSE, layout = NULL, scene = NULL, ...)

#### Arguments

- `flatten`:

  (`logical(1)`)  
  If TRUE, display as 2D contour plot. If FALSE, display as 3D surface
  plot.

- `layout`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Layout options passed to `plotly::layout()`. Ignored by other
  visualizer types. Default is NULL.

- `scene`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Scene options for 3D plots. Ignored by other visualizer types. Default
  is NULL.

- `...`:

  Additional arguments passed to the parent plot method.

#### Returns

A plotly object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VisualizerSurface$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
