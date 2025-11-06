# Visualizer for loss functions

Visualize one or multiple loss functions.

## Super class

[`vistool::Visualizer`](https://slds-lmu.github.io/vistool/reference/Visualizer.md)
-\> `VisualizerLossFuns`

## Public fields

- `losses`:

  (`list`)  
  List of LossFunction objects.

- `task_type`:

  (`character(1)`)  
  Task type (regr or classif).

- `y_pred`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Predicted values.

- `y_true`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  True values.

- `input_type`:

  (`character(1)`)  
  Input scale for classification tasks: `"score"` (margin‑based,
  default) or `"probability"`.

## Methods

### Public methods

- [`VisualizerLossFuns$new()`](#method-VisualizerLossFuns-new)

- [`VisualizerLossFuns$plot()`](#method-VisualizerLossFuns-plot)

- [`VisualizerLossFuns$add_points()`](#method-VisualizerLossFuns-add_points)

- [`VisualizerLossFuns$clone()`](#method-VisualizerLossFuns-clone)

Inherited methods

- [`vistool::Visualizer$add_annotation()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_annotation)
- [`vistool::Visualizer$add_contours()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-add_contours)
- [`vistool::Visualizer$resolve_layer_colors()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-resolve_layer_colors)
- [`vistool::Visualizer$save()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-save)
- [`vistool::Visualizer$set_theme()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-set_theme)
- [`vistool::Visualizer$theme()`](https://slds-lmu.github.io/vistool/reference/Visualizer.html#method-theme)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    VisualizerLossFuns$new(
      losses,
      y_pred = NULL,
      y_true = NULL,
      input_type = "auto",
      n_points = 1000L,
      ...
    )

#### Arguments

- `losses`:

  (`list`)  
  List of LossFunction objects.

- `y_pred`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Predicted values. Optional.

- `y_true`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  True values. Optional.

- `input_type`:

  (`character(1)`)  
  Desired input scale. One of `"auto"`, `"score"`, `"probability"`.
  `"auto"` (default) chooses the common `input_default` of the supplied
  losses.

- `n_points`:

  (`integer(1)`) Default resolution used when calling `$plot()` without
  specifying `n_points`. Defaults to 1000.

- `...`:

  Additional arguments (currently unused).

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Create and return the ggplot2 plot with model-specific layers.

#### Usage

    VisualizerLossFuns$plot(
      n_points = NULL,
      y_curves = "both",
      line_width = NULL,
      line_col = NULL,
      line_type = NULL,
      ...
    )

#### Arguments

- `n_points`:

  (`integer(1)`)  
  Number of points to use for plotting the loss functions. Defaults to
  the value configured when constructing the visualizer (via
  [`as_visualizer()`](https://slds-lmu.github.io/vistool/reference/as_visualizer.md)),
  falling back to 1000 if not set.

- `y_curves`:

  (`character(1)`)  
  When `input_type = "probability"`, choose which curves to display:
  `"both"`, `"y1"`, or `"y0"`. Default is "both".

- `line_width`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Line widths for different loss functions. If NULL, uses default width
  of 1.2 for all lines.

- `line_col`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Line colors for different loss functions. If NULL, uses automatic
  color assignment.

- `line_type`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Line types for different loss functions. If NULL, uses "solid" for all
  lines.

- `...`:

  Additional arguments passed to the parent plot method.

#### Returns

A ggplot2 object.

------------------------------------------------------------------------

### Method `add_points()`

Add points to the loss function visualization with optional vertical
lines. Points are automatically positioned on the loss curve by
evaluating the selected loss function at the given x-coordinates.

#### Usage

    VisualizerLossFuns$add_points(
      x,
      loss_id = NULL,
      show_line = TRUE,
      color = "auto",
      size = NULL,
      alpha = NULL,
      line_color = NULL,
      line_alpha = NULL
    )

#### Arguments

- `x`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  x-coordinates where to place points on the loss curves. These
  represent the residual values (y-f for regression, y\*f for
  classification scores, or probabilities for classification probability
  mode).

- `loss_id`:

  (`character(1)`)  
  ID of the loss function to use for y-value calculation. If NULL
  (default), uses the first loss function.

- `show_line`:

  (`logical(1)`)  
  If TRUE (default), draws vertical lines from points to x-axis.

- `color`:

  (`character(1)`)  
  Color of the points and lines. Use "auto" for automatic color
  assignment. Default is "auto".

- `size`:

  (`numeric(1)`)  
  Size of the points. If NULL, uses theme\$point_size. Default is NULL.

- `alpha`:

  (`numeric(1)`)  
  Alpha transparency of points and lines. If NULL, uses theme\$alpha.
  Default is NULL.

- `line_color`:

  (`character(1)`)  
  Color of vertical lines. If NULL, uses the same color as points.

- `line_alpha`:

  (`numeric(1)`)  
  Alpha transparency of vertical lines. If NULL, uses alpha \* 0.7.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VisualizerLossFuns$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
