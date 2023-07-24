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
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.Task = function(x, learner, padding = 0, n_points = 100L, ...) {
  n_features = length(x$feature_names)
  if (n_features == 1) {
    Visualizer1DModel$new(x, learner, padding = padding, n_points = n_points, ...)
  } else if (n_features == 2) {
    Visualizer2DModel$new(x, learner, padding = padding, n_points = n_points, ...)
  } else {
    stop("Task has more than 2 features.")
  }
}

#' @rdname as_visualizer
#' @template param_padding
#' @template param_n_points
#' @export
as_visualizer.Objective = function(x, padding = 0, n_points = 100L, ...) {
  n_features = length(x$xdim)
  if (n_features == 1) {
    Visualizer1DObjective$new(x, padding = padding, n_points = n_points, ...)
  } else if (n_features == 2) {
    Visualizer2DObjective$new(x, padding = padding, n_points = n_points, ...)
  } else {
    stop("Task has more than 2 features.")
  }
}

#' @rdname as_visualizer
#' @param y_pred (`numeric()`)\cr
#'   Predicted values.
#' @param y_true (`numeric(1)`)\cr
#'   True value.
#' @export
as_visualizer.LossFunction = function(x, y_pred, y_true, ...) {
  VisualizerLossFunction$new(x, y_pred, y_true, ...)
}
