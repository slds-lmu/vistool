# Visualize model as interactive surface

This class is used to create interactive 3D surface visualizations of
learners and tasks for 2D input data using plotly.

## Super classes

[`vistool::Visualizer`](https://slds-lmu.github.io/vistool/reference/Visualizer.md)
-\>
[`vistool::VisualizerSurface`](https://slds-lmu.github.io/vistool/reference/VisualizerSurface.md)
-\> `VisualizerSurfaceModel`

## Public fields

- `task`:

  ([`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html))  
  Task used to train the model.

- `learner`:

  ([`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner used to train the model.

## Methods

### Public methods

- [`VisualizerSurfaceModel$new()`](#method-VisualizerSurfaceModel-new)

- [`VisualizerSurfaceModel$add_training_data()`](#method-VisualizerSurfaceModel-add_training_data)

- [`VisualizerSurfaceModel$add_boundary()`](#method-VisualizerSurfaceModel-add_boundary)

- [`VisualizerSurfaceModel$plot()`](#method-VisualizerSurfaceModel-plot)

- [`VisualizerSurfaceModel$clone()`](#method-VisualizerSurfaceModel-clone)

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

    VisualizerSurfaceModel$new(
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

- `hypothesis`:

  (`Hypothesis`\|`NULL`) Optional hypothesis used instead of a learner

- `domain`:

  (`list`\|`NULL`) Domain limits when visualizing a hypothesis without a
  task

- `retrain`:

  (`logical(1)`) Whether to retrain the learner if it has already been
  trained on the task. Default is `TRUE`.

------------------------------------------------------------------------

### Method `add_training_data()`

Adds the training data to the plot.

#### Usage

    VisualizerSurfaceModel$add_training_data(
      size = NULL,
      color = "auto",
      shape = 0,
      ...
    )

#### Arguments

- `size`:

  (`numeric(1)`)  
  Size of the points. If NULL, uses theme\$point_size. Default is NULL.

- `color`:

  (`character(1)` or named `character`)  
  Color of the points. For classification tasks:

  - `character(1)`: A single color for all points (e.g., `"blue"`) or
    `"auto"` for automatic color assignment.

  - `named character`: A vector mapping class labels to colors (e.g.,
    `c(pos = "red", neg = "blue")`). For regression tasks, only single
    colors are supported. Default is `"auto"`.

- `shape`:

  (`numeric(1)` or named `numeric`)  
  Shape/symbol of the points. For classification tasks:

  - `numeric(1)`: A single symbol for all points (e.g., `0` for circle).

  - `named numeric`: A vector mapping class labels to symbols (e.g.,
    `c(pos = 0, neg = 1)`). For regression tasks, only single symbols
    are supported. Default is 0 (circle).

- `...`:

  (`any`)  
  Further arguments passed to `add_trace(...)`.

------------------------------------------------------------------------

### Method `add_boundary()`

Adds boundary surface(s) to the plot at specified values.

#### Usage

    VisualizerSurfaceModel$add_boundary(values = NULL, color = NULL, ...)

#### Arguments

- `values`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of z-values where to draw boundary surfaces. For classification
  with probability predictions, defaults to 0.5. For regression or
  response predictions, defaults to the median of predictions.

- `color`:

  (`character(1)` or [`list()`](https://rdrr.io/r/base/list.html))  
  Color specification for boundary surfaces. Default uses a neutral
  colorscale.

- `...`:

  (`any`)  
  Further arguments passed to `add_trace(...)` or `add_surface(...)`.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Create and return the plotly plot with model-specific layers.

#### Usage

    VisualizerSurfaceModel$plot(...)

#### Arguments

- `...`:

  Additional arguments passed to the parent plot method.

#### Returns

A plotly object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VisualizerSurfaceModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
