# Visualize model (unified 1D/2D)

This class provides a unified interface for visualizing machine learning
models on both 1D and 2D tasks. It automatically detects the
dimensionality and creates appropriate visualizations using ggplot2.

## Value

Returns self invisibly.

Returns self invisibly.

## Super class

[`vistool::Visualizer`](https://slds-lmu.github.io/vistool/reference/Visualizer.md)
-\> `VisualizerModel`

## Public fields

- `task`:

  ([`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html))  
  Task used to train the model.

- `learner`:

  ([`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner used to train the model.

## Methods

### Public methods

- [`VisualizerModel$new()`](#method-VisualizerModel-new)

- [`VisualizerModel$add_points()`](#method-VisualizerModel-add_points)

- [`VisualizerModel$add_training_data()`](#method-VisualizerModel-add_training_data)

- [`VisualizerModel$add_boundary()`](#method-VisualizerModel-add_boundary)

- [`VisualizerModel$plot()`](#method-VisualizerModel-plot)

- [`VisualizerModel$clone()`](#method-VisualizerModel-clone)

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

    VisualizerModel$new(
      task,
      learner,
      x1_limits = NULL,
      x2_limits = NULL,
      padding = 0,
      n_points = 100L,
      hypothesis = NULL,
      domain = NULL,
      retrain = TRUE
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  The task to train the model on.

- `learner`:

  ([`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html))  
  The learner to train the model with.

- `x1_limits`:

  (`numeric(2)`)  
  Limits for the first feature axis. For 1D tasks, this controls the
  x-axis range. If NULL, will be determined from task data.

- `x2_limits`:

  (`numeric(2)`)  
  Limits for the second feature axis (2D tasks only). Ignored for 1D
  tasks. If NULL, will be determined from task data.

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

- `hypothesis`:

  (`Hypothesis`\|`NULL`) Optional hypothesis object that provides
  predictions instead of a learner. Supply either `learner` or
  `hypothesis`, but not both.

- `domain`:

  (`list`\|`NULL`) Named list giving axis limits per predictor when no
  Task data is available, e.g., `list(x = c(-1, 1))` for 1D or
  `list(x = c(-1,1), y = c(-1,1))` for 2D.

- `retrain`:

  (`logical(1)`) Whether to (re)train the supplied learner on the task
  (default `TRUE`). Set to `FALSE` to reuse an already trained learner.
  If set to `FALSE` but the learner has not yet been trained
  (`learner$model` is `NULL`), a warning is emitted and training is
  performed to ensure predictions are available.

------------------------------------------------------------------------

### Method `add_points()`

Add points with optional residual loss geometry (1D regression only for
now). Extends base add_points() with a `loss` argument to visualize
residuals.

#### Usage

    VisualizerModel$add_points(
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
      loss = NULL,
      loss_params = list(),
      loss_fill = "auto",
      loss_alpha = NULL,
      loss_color = NA,
      loss_linetype = "solid"
    )

#### Arguments

- `points`:

  (`data.frame`\|`matrix`\|`list`) Points to add. For 1D these should
  contain columns `x` and `y` (observed y required for residuals).

- `color`:

  (`character(1)`) Color of the points or "auto". Default "auto".

- `size`:

  (`numeric(1)`\|`NULL`) Point size. If NULL uses theme default.

- `shape`:

  (`integer(1)`\|`character(1)`) Point shape. Default 19.

- `alpha`:

  (`numeric(1)`\|`NULL`) Alpha transparency for points.

- `annotations`:

  ([`character()`](https://rdrr.io/r/base/character.html)\|`NULL`)
  Optional text annotations for points.

- `annotation_size`:

  (`numeric(1)`\|`NULL`) Size of annotation text.

- `ordered`:

  (`logical(1)`) Whether points should be connected in order (arrows).
  Default FALSE.

- `arrow_color`:

  (`character(1)`\|`NULL`) Arrow color when ordered = TRUE.

- `arrow_size`:

  (`numeric(1)`) Arrow size when ordered = TRUE. Default 0.3.

- `loss`:

  (`character(1)`\|`NULL`) One of `"l2_se"` (aliases: `"l2"`, `"se"`),
  `"l1_ae"` (aliases: `"l1"`, `"abs"`, `"mae"`), or `NULL` (no
  geometry).

- `loss_params`:

  ([`list()`](https://rdrr.io/r/base/list.html)) Reserved for future
  loss-specific parameters (e.g. Huber delta).

- `loss_fill`:

  (`character(1)`) Fill color for L2 squares. "auto" draws from palette.

- `loss_alpha`:

  (`numeric(1)`\|`NULL`) Fill alpha for L2 squares (defaults to
  theme\$alpha \* 0.4).

- `loss_color`:

  (`character(1)`\|`NA`) Color for residual segment / square border. If
  `NA`, derived from fill.

- `loss_linetype`:

  (`character(1)`) Line type for residual segment. Default "solid".

#### Returns

Invisible self.

------------------------------------------------------------------------

### Method `add_training_data()`

Adds the training data to the plot.

#### Usage

    VisualizerModel$add_training_data(
      color = "auto",
      size = NULL,
      shape = 19,
      alpha = NULL,
      show_labels = FALSE,
      label_size = NULL
    )

#### Arguments

- `color`:

  (`character(1)` or named `character`)  
  Color of the points. For classification tasks:

  - `character(1)`: A single color for all points (e.g., `"blue"`) or
    `"auto"` for automatic color assignment.

  - `named character`: A vector mapping class labels to colors (e.g.,
    `c(pos = "red", neg = "blue")`). For regression tasks, only single
    colors are supported. Default is `"auto"`.

- `size`:

  (`numeric(1)`)  
  Size of the points. If NULL, uses theme\$point_size. Default is NULL.

- `shape`:

  (`numeric(1)` or named `numeric`)  
  Shape of the points. For classification tasks, can be a named vector
  mapping class labels to shapes. Default is 19 (filled circle).

- `alpha`:

  (`numeric(1)`)  
  Alpha transparency of the points. If NULL, uses theme\$alpha. Default
  is NULL.

- `show_labels`:

  (`logical(1)`)  
  Whether to show data point labels. Default is FALSE.

- `label_size`:

  (`numeric(1)`)  
  Size of data point labels. If NULL, defaults to smaller text.

------------------------------------------------------------------------

### Method `add_boundary()`

Adds boundary lines/contours to the plot.

#### Usage

    VisualizerModel$add_boundary(
      values = NULL,
      color = "black",
      linetype = "dashed",
      linewidth = NULL,
      alpha = NULL
    )

#### Arguments

- `values`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Values at which to draw boundaries. For 1D: horizontal lines
  (y-values). For 2D: contour lines (z-values). If NULL, uses sensible
  defaults based on prediction type.

- `color`:

  (`character(1)`)  
  Color of the boundary lines. Default is "black".

- `linetype`:

  (`character(1)`)  
  Line type for boundaries. For 1D: ggplot2 linetypes. For 2D: contour
  line types. Default is "dashed".

- `linewidth`:

  (`numeric(1)`)  
  Width of boundary lines. If NULL, uses theme\$line_width.

- `alpha`:

  (`numeric(1)`)  
  Alpha transparency of boundary lines. If NULL, uses theme\$alpha.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Create and return the ggplot2 plot with model-specific layers.

#### Usage

    VisualizerModel$plot(...)

#### Arguments

- `...`:

  Additional arguments passed to the parent plot method.

#### Returns

A ggplot2 object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VisualizerModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
