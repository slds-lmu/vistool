# Visualize objective (unified 1D/2D)

This class provides a unified interface for visualizing objective
functions and optimization traces on both 1D and 2D objectives. It
automatically detects the dimensionality and creates appropriate
visualizations using ggplot2.

## Value

Returns self invisibly.

## Super class

[`vistool::Visualizer`](https://slds-lmu.github.io/vistool/reference/Visualizer.md)
-\> `VisualizerObj`

## Public fields

- `objective`:

  (`Objective`)  
  The objective function.

## Methods

### Public methods

- [`VisualizerObj$new()`](#method-VisualizerObj-new)

- [`VisualizerObj$add_optimization_trace()`](#method-VisualizerObj-add_optimization_trace)

- [`VisualizerObj$plot()`](#method-VisualizerObj-plot)

- [`VisualizerObj$clone()`](#method-VisualizerObj-clone)

Inherited methods

- [`vistool::Visualizer$add_annotation()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_annotation)
- [`vistool::Visualizer$add_contours()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_contours)
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

    VisualizerObj$new(
      objective,
      x1_limits = NULL,
      x2_limits = NULL,
      padding = 0,
      n_points = 100L,
      type = NULL
    )

#### Arguments

- `objective`:

  (`Objective`)  
  The objective to optimize.

- `x1_limits`:

  (`numeric(2)`)  
  Limits for the first dimension. For 1D objectives, this controls the
  x-axis range. If NULL, will be determined from objective bounds.

- `x2_limits`:

  (`numeric(2)`)  
  Limits for the second dimension (2D objectives only). Ignored for 1D
  objectives. If NULL, will be determined from objective bounds.

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

- `type`:

  (`character(1)`)  
  Optional visualization type ("1d" or "2d") to override automatic
  dimension detection. Useful when objective dimensions are unknown but
  visualization type is explicitly specified.

------------------------------------------------------------------------

### Method `add_optimization_trace()`

Add optimization trace to the plot.

#### Usage

    VisualizerObj$add_optimization_trace(
      optimizer,
      line_color = NULL,
      line_width = NULL,
      line_type = "solid",
      npoints = NULL,
      npmax = NULL,
      name = NULL,
      add_marker_at = 1,
      marker_size = 3,
      marker_shape = 16,
      marker_color = NULL,
      show_start_end = TRUE,
      alpha = NULL
    )

#### Arguments

- `optimizer`:

  (`Optimizer`)  
  The optimizer to add to the plot. Must have been run and contain
  archive data.

- `line_color`:

  (`character(1)`)  
  Color of the optimization trace line. If NULL, uses automatic color.

- `line_width`:

  (`numeric(1)`)  
  Width of the trace line. If NULL, uses theme\$line_width.

- `line_type`:

  (`character(1)`)  
  Type of the trace line. One of "solid", "dashed", "dotted". Default is
  "solid".

- `npoints`:

  (`integer(1)`)  
  Number of points to show from the trace. If NULL, shows all points.

- `npmax`:

  (`integer(1)`)  
  Maximum number of points to show. If NULL, no limit.

- `name`:

  (`character(1)`)  
  Name for the trace (used in legends). If NULL, uses optimizer ID.

- `add_marker_at`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Iteration numbers where to add markers. For 2D plots only.

- `marker_size`:

  (`numeric(1)`)  
  Size of markers. Default is 3.

- `marker_shape`:

  (`numeric(1)` or `character(1)`)  
  Shape of markers. Default is 16 (filled circle).

- `marker_color`:

  (`character(1)`)  
  Color of markers. If NULL, uses line color.

- `show_start_end`:

  (`logical(1)`)  
  Whether to highlight start and end points. Default is TRUE for 2D
  plots.

- `alpha`:

  (`numeric(1)`)  
  Alpha transparency. If NULL, uses theme\$alpha.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Create and return the ggplot2 plot with model-specific layers.

#### Usage

    VisualizerObj$plot(...)

#### Arguments

- `...`:

  Additional arguments passed to the parent plot method.

#### Returns

A ggplot2 object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VisualizerObj$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
