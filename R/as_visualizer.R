#' @title Convert to visualizer
#'
#' @description
#' This function converts to a visualizer. Automatically chooses between 1D, 2D, and 3D 
#' visualizations based on the number of features/dimensions, or allows explicit control
#' via the `type` parameter. 1D and 2D visualizations use ggplot2, while 3D visualizations use plotly.
#'
#' @param x (`any`)
#'  Object to convert to a visualizer.
#' @param ... (`any`)
#'  Additional arguments.
#'
#' @return An object inheriting from a Visualizer class (Visualizer1D, Visualizer2D, Visualizer3D, etc.)
#' depending on the input and selected type.
#'
#' @details
#' If `type = "auto"` (default), the function will inspect the input and select the appropriate visualizer:
#' - 1D: For objects with 1 feature/dimension (uses ggplot2)
#' - 2D: For objects with 2 features/dimensions (uses ggplot2)
#' - 3D: For objects with 3 or more features/dimensions (uses plotly)
#' You can override this by specifying `type = "1d"`, `type = "2d"`, or `type = "3d"`.
#'
#' @export
as_visualizer = function(x, ...) {
  UseMethod("as_visualizer")
}

#' @param learner (`mlr3::Learner`)
#'  The learner to train the model with.
#' @param type (`character(1)`)
#'  The type of visualization: "auto" (default), "1d", "2d", or "3d". 
#'  If "auto", automatically chooses based on the number of features.
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.Task = function(x, learner, type = "auto", x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "3d"))
  n_features = length(x$feature_names)
  # Determine visualization type
  if (type == "auto") {
    if (n_features == 1) {
      vis_type = "1d"
    } else if (n_features == 2) {
      vis_type = "2d"
    } else {
      vis_type = "3d"
    }
  } else {
    vis_type = type
  }
  # Validate type against features
  if (vis_type == "1d" && n_features != 1) {
    stop("1D visualization requires a task with exactly 1 feature.")
  }
  if (vis_type == "2d" && n_features != 2) {
    stop("2D visualization requires a task with exactly 2 features.")
  }
  # 3D: allow 3 or more features, but only use first two for grid, third for z
  if (vis_type == "1d") {
    return(Visualizer1DModel$new(x, learner, xlim = x1_limits, n_points = n_points, ...))
  } else if (vis_type == "2d") {
    return(Visualizer2DModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...))
  } else if (vis_type == "3d") {
    return(Visualizer3DModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...))
  } else {
    stop("Unknown visualization type.")
  }
}

#' @param type (`character(1)`)
#'  The type of visualization: "auto" (default), "1d", "2d", or "3d". 
#'  If "auto", automatically chooses based on the number of dimensions.
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.Objective = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "3d"))
  n_dim = x$xdim
  if (type == "auto") {
    if (n_dim == 1) {
      vis_type = "1d"
    } else if (n_dim == 2) {
      vis_type = "2d"
    } else {
      vis_type = "3d"
    }
  } else {
    vis_type = type
  }
  if (vis_type == "1d" && n_dim != 1) {
    stop("1D visualization requires an objective with exactly 1 dimension.")
  }
  if (vis_type == "2d" && n_dim != 2) {
    stop("2D visualization requires an objective with exactly 2 dimensions.")
  }
  if (vis_type == "1d") {
    return(Visualizer1DObj$new(x, xlim = x1_limits, n_points = n_points))
  } else if (vis_type == "2d") {
    return(Visualizer2DObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points))
  } else if (vis_type == "3d") {
    return(Visualizer3DObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points))
  } else {
    stop("Unknown visualization type.")
  }
}

#' @param type (`character(1)`)
#'  The type of visualization: "auto" (default), "1d", "2d", or "3d". 
#'  If "auto", automatically chooses based on the loss function's dimensionality.
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.LossFunction = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "3d"))
  # For loss functions, default to 1D visualizer
  if (type == "auto" || type == "1d") {
    return(VisualizerLossFuns$new(list(x)))  # VisualizerLossFuns expects a list of losses
  } else {
    stop("Only 1D visualization is currently supported for LossFunction.")
  }
}
