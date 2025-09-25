#' @title Convert to visualizer
#'
#' @description
#' `as_visualizer()` inspects its input and instantiates the matching visualizer class.
#' All common arguments are validated first so that incompatible combinations fail fast
#' with informative messages before any expensive rendering work is scheduled.
#'
#' @template param_x
#' @template param_type
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_hypothesis
#' @template param_domain
#' @template param_n_points
#'
#' @param y_pred (`numeric()`)
#'   Predicted values (for loss function visualizations).
#' @param y_true (`numeric()`)
#'   True values (for loss function visualizations).
#' @param input_type (`character(1)`)
#'   One of `"auto"` (default), `"score"`, or `"probability"`. Passed through to
#'   the loss visualizer.
#' @param y_curves (`character(1)`)
#'   Which response curve(s) to draw when `input_type = "probability"`.
#'   One of `"both"`, `"y1"`, or `"y0"`.
#' @template param_learner
#' @param retrain (`logical(1)`)
#'   If a learner is supplied, whether it should be (re)trained on the task (`TRUE`, default)
#'   or reused as-is (`FALSE`). If `FALSE` but the learner is untrained, a warning is issued
#'   and training is performed.
#'
#' @return An object inheriting from a Visualizer class (e.g. `VisualizerModel`,
#' `VisualizerObj`, or `VisualizerLossFuns`) depending on the input and selected type.
#'
#' @details
#' By default (`type = "auto"`), `as_visualizer()` chooses a ggplot2 backend for 1D and 2D
#' inputs. For 2D models or objectives you can opt into interactive plotly surfaces via
#' `type = "surface"`. All inputs are checked by a shared validation helper that enforces
#' mutual exclusivity of `learner`/`hypothesis`, ensures limits are finite and ordered, and
#' rejects parameters that are irrelevant for the chosen input.
#'
#' @section Supported inputs:
#' \tabular{llll}{
#'   Input & Auto `type` & Allowed overrides & Notes \\
#'   `Task` & `"1d"` (1 feature) or `"2d"` (2 features) & `type = "surface"` (2D only) & Exactly one of `learner`/`hypothesis`; `domain` is ignored and therefore rejected. \\
#'   `Hypothesis` & matches `input_dim` & `type = "surface"` (2D only) & Requires complete `domain`; `learner` not supported. \\
#'   `Objective` & matches `xdim` when available & `type = "surface"` (2D only) & `learner`/`hypothesis` not supported. \\
#'   `LossFunction` or list of losses & always `"1d"` & `n_points` & `input_type`, `y_pred`, `y_true`, `y_curves`, and `n_points` are forwarded to the loss visualizer. \\
#' }
#'
#' @section Validation:
#' Common argument validation is centralized so that:
#' * unused parameters (e.g. 2D limits supplied for 1D losses) raise an error,
#' * limits supplied manually are finite, strictly ordered, and match the dimensionality,
#' * high grid resolutions for interactive surfaces trigger explicit warnings, and
#' * probability inputs for loss visualizations are checked for consistency of
#'   `input_type`, `y_pred`, `y_true`, and `y_curves`.
#'
#' @seealso
#' [VisualizerModel], [VisualizerSurfaceModel], [VisualizerObj], [VisualizerSurfaceObj],
#' [VisualizerLossFuns]
#'
#' @examples
#' obj = obj("TF_branin")
#' vis = as_visualizer(obj, type = "2d")
#'
#' # High resolutions on surfaces warn before plotting
#' tryCatch(
#'   as_visualizer(obj, type = "surface", n_points = 256L),
#'   warning = function(w) message("Warning: ", conditionMessage(w))
#' )
#'
#' @export
as_visualizer = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                         padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                         input_type = "auto", y_curves = "both", learner = NULL,
                         hypothesis = NULL, domain = NULL, retrain = TRUE) {
  UseMethod("as_visualizer")
}

#' @rdname as_visualizer
#' @export
as_visualizer.Task = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                              padding = 0, n_points = 100L, learner = NULL,
                              hypothesis = NULL, retrain = TRUE, ...) {
  dots = list(...)
  payload = validate_visualizer_args("task", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = dots$y_pred,
    y_true = dots$y_true,
    input_type = dots$input_type,
    y_curves = dots$y_curves,
    learner = learner,
    hypothesis = hypothesis,
    domain = dots$domain,
    retrain = retrain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.Hypothesis = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                    padding = 0, n_points = 100L, domain = NULL,
                                    retrain = TRUE, ...) {
  dots = list(...)
  payload = validate_visualizer_args("hypothesis", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = dots$y_pred,
    y_true = dots$y_true,
    input_type = dots$input_type,
    y_curves = dots$y_curves,
    learner = dots$learner,
    hypothesis = dots$hypothesis,
    domain = domain,
    retrain = retrain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.Objective = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                   padding = 0, n_points = 100L, ...) {
  dots = list(...)
  payload = validate_visualizer_args("objective", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    padding = padding,
    n_points = n_points,
    y_pred = dots$y_pred,
    y_true = dots$y_true,
    input_type = dots$input_type,
    y_curves = dots$y_curves,
    learner = dots$learner,
    hypothesis = dots$hypothesis,
    domain = dots$domain,
    retrain = dots$retrain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.LossFunction = function(x, type = "auto", n_points = 1000L,
                                      y_pred = NULL, y_true = NULL,
                                      input_type = "auto", y_curves = "both", ...) {
  dots = list(...)
  payload = validate_visualizer_args("loss", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    n_points = n_points,
    y_pred = y_pred,
    y_true = y_true,
    input_type = input_type,
    y_curves = y_curves,
    learner = dots$learner,
    domain = dots$domain
  ))
  construct_visualizer(payload)
}

#' @rdname as_visualizer
#' @export
as_visualizer.list = function(x, type = "auto", n_points = 1000L,
                              y_pred = NULL, y_true = NULL,
                              input_type = "auto", y_curves = "both", ...) {
  dots = list(...)
  payload = validate_visualizer_args("loss_list", list(
    call = match.call(expand.dots = FALSE),
    x = x,
    type = type,
    n_points = n_points,
    y_pred = y_pred,
    y_true = y_true,
    input_type = input_type,
    y_curves = y_curves,
    learner = dots$learner,
    domain = dots$domain
  ))
  construct_visualizer(payload)
}
