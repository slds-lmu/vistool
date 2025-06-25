#' @title Convert to visualizer
#'
#' @description
#' This function converts to a visualizer.
#'
#' @param x (`any`)\cr
#'  Object to convert to a visualizer.
#' @param ... (`any`)\cr
#'  Additional arguments.
#'
#' @export
as_visualizer = function(x, ...) {
  UseMethod("as_visualizer")
}

#' @param learner (`mlr3::Learner`)\cr
#'  The learner to train the model with.
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.Task = function(x, learner, x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  n_features = length(x$feature_names)
  if (n_features == 1) {
    Visualizer1DModel$new(x, learner, xlim = x1_limits, n_points = n_points, ...)
  } else if (n_features == 2) {
    Visualizer2DModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...)
  } else {
    stop("Task has more than 2 features.")
  }
}

#' @rdname as_visualizer
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @export
as_visualizer.Objective = function(x, x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  if (x$xdim == 1) {
    Visualizer1DObjective$new(x, x1_limits = x1_limits, padding = padding, n_points = n_points, ...)
  } else if (x$xdim == 2) {
    Visualizer2DObjective$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...)
  } else {
    stop("Objective has more than 2 dimensions.")
  }
}

#' @rdname as_visualizer
#' @param y_pred (`numeric()`)\cr
#'   Predicted values.
#' @param y_true (`numeric(1)`)\cr
#'   True value.
#' @export
as_visualizer.LossFunction = function(x, y_pred, y_true, ...) {
  losses <- list(x)
  VisualizerLossFuns$new(losses)
}
