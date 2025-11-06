# Visualize objective as interactive surface

This class is used to create interactive surface visualizations and
animations of optimization traces for 2D objectives using plotly.

## Super classes

[`vistool::Visualizer`](https://slds-lmu.github.io/vistool/reference/Visualizer.md)
-\>
[`vistool::VisualizerSurface`](https://slds-lmu.github.io/vistool/reference/VisualizerSurface.md)
-\> `VisualizerSurfaceObj`

## Public fields

- `objective`:

  (`Objective`)  
  The objective function.

## Methods

### Public methods

- [`VisualizerSurfaceObj$new()`](#method-VisualizerSurfaceObj-new)

- [`VisualizerSurfaceObj$add_optimization_trace()`](#method-VisualizerSurfaceObj-add_optimization_trace)

- [`VisualizerSurfaceObj$plot()`](#method-VisualizerSurfaceObj-plot)

- [`VisualizerSurfaceObj$add_taylor()`](#method-VisualizerSurfaceObj-add_taylor)

- [`VisualizerSurfaceObj$add_hessian()`](#method-VisualizerSurfaceObj-add_hessian)

- [`VisualizerSurfaceObj$animate()`](#method-VisualizerSurfaceObj-animate)

- [`VisualizerSurfaceObj$clone()`](#method-VisualizerSurfaceObj-clone)

Inherited methods

- [`vistool::Visualizer$add_annotation()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_annotation)
- [`vistool::Visualizer$add_points()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_points)
- [`vistool::Visualizer$resolve_layer_colors()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-resolve_layer_colors)
- [`vistool::Visualizer$save()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-save)
- [`vistool::Visualizer$set_theme()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-set_theme)
- [`vistool::Visualizer$theme()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-theme)
- [`vistool::VisualizerSurface$add_contours()`](https://slds-lmu.github.io/vistool/reference/VisualizerSurface.html#method-add_contours)
- [`vistool::VisualizerSurface$init_layer_contour()`](https://slds-lmu.github.io/vistool/reference/VisualizerSurface.html#method-init_layer_contour)
- [`vistool::VisualizerSurface$init_layer_surface()`](https://slds-lmu.github.io/vistool/reference/VisualizerSurface.html#method-init_layer_surface)
- [`vistool::VisualizerSurface$set_layout()`](https://slds-lmu.github.io/vistool/reference/VisualizerSurface.html#method-set_layout)
- [`vistool::VisualizerSurface$set_scene()`](https://slds-lmu.github.io/vistool/reference/VisualizerSurface.html#method-set_scene)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    VisualizerSurfaceObj$new(
      objective,
      x1_limits = NULL,
      x2_limits = NULL,
      padding = 0,
      n_points = 100L
    )

#### Arguments

- `objective`:

  (`Objective`)  
  The objective to optimize.

- `x1_limits`:

  (`numeric(2)`)  
  The x1 limits.

- `x1_limits`:

  (`numeric(2)`)  
  The x1 limits.

- `x2_limits`:

  (`numeric(2)`)  
  The x2 limits.

- `x2_limits`:

  (`numeric(2)`)  
  The x2 limits.

- `padding`:

  (`numeric(1)`)  
  A margin that is added to x1limits and x2limits. The x1 margin is
  calculated by `max(x1lmits) - min(x1limits) * padding`.

- `padding`:

  (`numeric(1)`)  
  A margin that is added to x1limits and x2limits. The x1 margin is
  calculated by `max(x1lmits) - min(x1limits) * padding`.

- `n_points`:

  (`integer(1)`)  
  The number of generated point per dimension. Note that a grid of
  `npoints^2` values is generated and evaluated by `objective$eval(x)`
  to plot the surface.

- `n_points`:

  (`integer(1)`)  
  The number of generated point per dimension. Note that a grid of
  `npoints^2` values is generated and evaluated by `objective$eval(x)`
  to plot the surface.

------------------------------------------------------------------------

### Method `add_optimization_trace()`

Add an optimization trace.

#### Usage

    VisualizerSurfaceObj$add_optimization_trace(
      opt,
      line_color = NULL,
      mcolor_out = "black",
      line_width = NULL,
      npoints = NULL,
      npmax = NULL,
      name = NULL,
      offset = NULL,
      add_marker_at = 1,
      marker_shape = "circle",
      marker_color = NULL,
      line_type = NULL,
      ...
    )

#### Arguments

- `opt`:

  (`Optimizer`)  
  The optimizer from which the archive is extracted and used to plot the
  trace.

- `line_color`:

  (`character(1)`)  
  The color of the trace.

- `mcolor_out`:

  (`character(1)`)  
  The outer line color of the marker.

- `line_width`:

  (`numeric(1)`)  
  Width of the optimization trace line. If `NULL`, uses
  theme\$line_width.

- `npoints`:

  (`integer(1)`)  
  The number of used points from the archive. Default is `NULL` which
  means that all points are used. If set, a sequence from 1 to
  `nrow(opt$archive)` is created.

- `npmax`:

  (`integer(1)`)  
  The number of points used from the sequence
  `seq_len(nrow(opt$archive))[seq_len(npmax)]`

- `name`:

  (`character(1)`)  
  The name of the trace in the legend. Default is `NULL` which uses only
  `opt$id` (optimizer ID).

- `offset`:

  (`numeric(3)`)  
  Trace shift in direction (x, y, z).

- `add_marker_at`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of iterations at which a marker is added.

- `marker_shape`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Vector indicating the shape of the markers. If
  `length(marker_shape) == 1`, all markers get the same shape. The other
  option is to specify all markers individually by passing a vector of
  `length(add_marker_at)`. For a list of all shapes see
  `schema(F)$traces$XXX$attributes$marker$symbol$values` with `XXX` one
  of `scatter` or `scatter3d`. If `marker_shape = NA`, no marker are
  added.

- `marker_color`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The colors for the markers.

- `line_type`:

  (`character(1)`) Line type for the optimization trace (e.g., "solid",
  "dashed", "dotted", "dash", "dot", "dashdot"). Mapped to Plotly's
  `line$dash`. If `NULL`, defaults to solid.

- `...`:

  Further arguments passed to `add_trace(...)`.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Renders the surface plot with all added layers.

#### Usage

    VisualizerSurfaceObj$plot(...)

#### Arguments

- `...`:

  (`any`)  
  Additional arguments passed to the parent plot method.

#### Returns

The plotly object.

------------------------------------------------------------------------

### Method `add_taylor()`

Add a Taylor approximation (for 1 and 2 degrees).

#### Usage

    VisualizerSurfaceObj$add_taylor(
      x0,
      degree = 2,
      x1margin = 0,
      x2margin = 0,
      npoints_per_dim = 20L,
      zlim = NULL,
      ...
    )

#### Arguments

- `x0`:

  (`numeric()) `  
  The point around which the approximation is done.

- `degree`:

  (`integer(1)`)  
  The degree of the approximation (only 1 and 2 is implemented).

- `x1margin`:

  (`numeric(1)`)  
  The "length" of the hyperplane in direction x1.

- `x2margin`:

  (`numeric(1)`)  
  The "length" of the hyperplane in direction x2.

- `npoints_per_dim`:

  (`integer(1)`)  
  Number of points per dimension for the plotting grid.

- `zlim`:

  (`numeric(2)`)  
  The limits for z. Can be helpful if the hyperplane as a huge z range
  and therefore the plot looks ugly.

- `...`:

  (`any`)  
  Additional parameter passed to `add_surface()`.

------------------------------------------------------------------------

### Method `add_hessian()`

Add two "arrows" as eigenvectors of the Hessian.

#### Usage

    VisualizerSurfaceObj$add_hessian(x0, x1length = 0.1, x2length = 0.1, ...)

#### Arguments

- `x0`:

  (`numeric(2)`)  
  The point at which the Hessian is calculated.

- `x1length`:

  (`numeric(1)`)  
  The length of the first eigenvector.

- `x2length`:

  (`numeric(1)`)  
  The length of the second eigenvector.

- `...`:

  (`any`)  
  Additional arguments passed to `add_trace`.

------------------------------------------------------------------------

### Method `animate()`

Create an animation of `$plot()`.

#### Usage

    VisualizerSurfaceObj$animate(
      dir = "animation",
      nframes = 10L,
      view_start = list(x = 1, y = 1, z = 1),
      view_end = list(x = 1, y = 1, z = 1),
      fext = "png",
      stops = NULL,
      ...
    )

#### Arguments

- `dir`:

  (`character(1)`)  
  The directory in which all the images are saved.

- `nframes`:

  (`integer(1)`)  
  The number of frames.

- `view_start`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  The start view of the animation.

- `view_end`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  The end view of the animation.

- `fext`:

  (`character(1)`)  
  The file extension (default is `png`).

- `stops`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The step / iteration in the archives of the optimizers added by
  `$add_optimization_trace()` at which a frame is taken. Must have exact
  the same length as defined in `nframes`. By default, a sequence with
  equidistant points is generated for `stops`.

- `...`:

  (`any`)  
  Additional arguments passed to `$save(...)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VisualizerSurfaceObj$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
