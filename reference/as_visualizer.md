# Convert to visualizer

`as_visualizer()` inspects its input and instantiates the matching
visualizer class. All common arguments are validated first so that
incompatible combinations fail fast with informative messages before any
expensive rendering work is scheduled.

## Usage

``` r
as_visualizer(
  x,
  type = "auto",
  x1_limits = NULL,
  x2_limits = NULL,
  padding = 0,
  n_points = 100L,
  y_pred = NULL,
  y_true = NULL,
  input_type = "auto",
  y_curves = "both",
  learner = NULL,
  hypothesis = NULL,
  domain = NULL,
  retrain = TRUE
)

# S3 method for class 'Task'
as_visualizer(
  x,
  type = "auto",
  x1_limits = NULL,
  x2_limits = NULL,
  padding = 0,
  n_points = 100L,
  y_pred = NULL,
  y_true = NULL,
  input_type = "auto",
  y_curves = "both",
  learner = NULL,
  hypothesis = NULL,
  domain = NULL,
  retrain = TRUE
)

# S3 method for class 'Hypothesis'
as_visualizer(
  x,
  type = "auto",
  x1_limits = NULL,
  x2_limits = NULL,
  padding = 0,
  n_points = 100L,
  y_pred = NULL,
  y_true = NULL,
  input_type = "auto",
  y_curves = "both",
  learner = NULL,
  hypothesis = NULL,
  domain = NULL,
  retrain = TRUE
)

# S3 method for class 'Objective'
as_visualizer(
  x,
  type = "auto",
  x1_limits = NULL,
  x2_limits = NULL,
  padding = 0,
  n_points = 100L,
  y_pred = NULL,
  y_true = NULL,
  input_type = "auto",
  y_curves = "both",
  learner = NULL,
  hypothesis = NULL,
  domain = NULL,
  retrain = TRUE
)

# S3 method for class 'LossFunction'
as_visualizer(
  x,
  type = "auto",
  x1_limits = NULL,
  x2_limits = NULL,
  padding = 0,
  n_points = 1000L,
  y_pred = NULL,
  y_true = NULL,
  input_type = "auto",
  y_curves = "both",
  learner = NULL,
  hypothesis = NULL,
  domain = NULL,
  retrain = TRUE
)

# S3 method for class 'list'
as_visualizer(
  x,
  type = "auto",
  x1_limits = NULL,
  x2_limits = NULL,
  padding = 0,
  n_points = 1000L,
  y_pred = NULL,
  y_true = NULL,
  input_type = "auto",
  y_curves = "both",
  learner = NULL,
  hypothesis = NULL,
  domain = NULL,
  retrain = TRUE
)
```

## Arguments

- x:

  (`any`)  
  Object to convert to a visualizer.

- type:

  (`character(1)`)  
  The type of visualization: "auto" (default), "1d", "2d", or "surface".
  If "auto", automatically chooses between 1D and 2D ggplot2
  visualizations based on the number of features/dimensions. Use
  "surface" for interactive plotly surface plots (2D inputs only, Models
  and Objectives only).

- x1_limits:

  (`numeric(2)`)  
  The x1 limits.

- x2_limits:

  (`numeric(2)`)  
  The x2 limits.

- padding:

  (`numeric(1)`)  
  A margin that is added to x1limits and x2limits. The x1 margin is
  calculated by `max(x1lmits) - min(x1limits) * padding`.

- n_points:

  (`integer(1)`)  
  The number of generated point per dimension. Note that a grid of
  `npoints^2` values is generated and evaluated by `objective$eval(x)`
  to plot the surface.

- y_pred:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Predicted values
  (for loss function visualizations).

- y_true:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) True values (for
  loss function visualizations).

- input_type:

  (`character(1)`) One of `"auto"` (default), `"score"`, or
  `"probability"`. Passed through to the loss visualizer.

- y_curves:

  (`character(1)`) Which response curve(s) to draw when
  `input_type = "probability"`. One of `"both"`, `"y1"`, or `"y0"`.

- learner:

  ([`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html))  
  The learner to train the model with.

- hypothesis:

  (`Hypothesis`\|`NULL`) Optional hypothesis object that provides
  predictions instead of a learner. Supply either `learner` or
  `hypothesis`, but not both.

- domain:

  (`list`\|`NULL`) Named list giving axis limits per predictor when no
  Task data is available, e.g., `list(x = c(-1, 1))` for 1D or
  `list(x = c(-1,1), y = c(-1,1))` for 2D.

- retrain:

  (`logical(1)`) If a learner is supplied, whether it should be
  (re)trained on the task (`TRUE`, default) or reused as-is (`FALSE`).
  If `FALSE` but the learner is untrained, a warning is issued and
  training is performed.

## Value

An object inheriting from a Visualizer class (e.g. `VisualizerModel`,
`VisualizerObj`, or `VisualizerLossFuns`) depending on the input and
selected type.

## Details

By default (`type = "auto"`), `as_visualizer()` chooses a ggplot2
backend for 1D and 2D inputs. For 2D models or objectives you can opt
into interactive plotly surfaces via `type = "surface"`. All inputs are
checked by a shared validation helper that enforces mutual exclusivity
of `learner`/`hypothesis`, ensures limits are finite and ordered, and
rejects parameters that are irrelevant for the chosen input.

## Supported inputs

|     |     |     |     |
|-----|-----|-----|-----|
|     |     |     |     |

## Validation

Common argument validation is centralized so that:

- unused parameters (e.g. 2D limits supplied for 1D losses) raise an
  error,

- limits supplied manually are finite, strictly ordered, and match the
  dimensionality,

- high grid resolutions for interactive surfaces trigger explicit
  warnings, and

- probability inputs for loss visualizations are checked for consistency
  of `input_type`, `y_pred`, `y_true`, and `y_curves`.

## See also

[VisualizerModel](https://slds-lmu.github.io/vistool/reference/VisualizerModel.md),
[VisualizerSurfaceModel](https://slds-lmu.github.io/vistool/reference/VisualizerSurfaceModel.md),
[VisualizerObj](https://slds-lmu.github.io/vistool/reference/VisualizerObj.md),
[VisualizerSurfaceObj](https://slds-lmu.github.io/vistool/reference/VisualizerSurfaceObj.md),
[VisualizerLossFuns](https://slds-lmu.github.io/vistool/reference/VisualizerLossFuns.md)

## Examples

``` r
obj = obj("TF_branin")
vis = as_visualizer(obj, type = "2d")

# High resolutions on surfaces warn before plotting
tryCatch(
  as_visualizer(obj, type = "surface", n_points = 256L),
  warning = function(w) message("Warning: ", conditionMessage(w))
)
#> Warning: Requested surface grid with n_points = 256; values above 200 can cause slow interactive rendering.
```
