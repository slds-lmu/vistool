#' @title Convert to visualizer
#'
#' @description
#' This function converts to a visualizer. Automatically chooses between 1D and 2D
#' visualizations based on the number of features/dimensions using ggplot2 backend.
#' For 2D inputs, you can optionally use `type = "surface"` to get interactive plotly
#' surface plots (available for Models and Objectives only).
#'
#' @template param_x
#' @template param_dots
#'
#' @return An object inheriting from a Visualizer class (Visualizer1D, Visualizer2D, VisualizerSurface, etc.)
#' depending on the input and selected type.
#'
#' @details
#' If `type = "auto"` (default), the function will inspect the input and select the appropriate ggplot2 visualizer:
#' - 1D: For objects with 1 feature/dimension (uses ggplot2)
#' - 2D: For objects with 2 features/dimensions (uses ggplot2)
#' You can override this by specifying `type = "1d"`, `type = "2d"`, or for 2D inputs only: `type = "surface"` (uses plotly for interactive surfaces, Models and Objectives only).
#'
#' @export
as_visualizer <- function(x, ...) {
  UseMethod("as_visualizer")
}

#' @template param_learner
#' @template param_type
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.Task <- function(x, learner, type = "auto", x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "surface"))
  n_features <- length(x$feature_names)
  
  # Determine visualization type
  if (type == "auto") {
    if (n_features == 1) {
      vis_type <- "1d"
    } else if (n_features == 2) {
      vis_type <- "2d"  # Default to ggplot2 for 2D
    } else {
      stop("Auto visualization only supports 1D and 2D tasks. For higher dimensions, please specify type explicitly.")
    }
  } else {
    vis_type <- type
  }
  
  # Validate type against features
  if (vis_type == "1d" && n_features != 1) {
    stop("1D visualization requires a task with exactly 1 feature.")
  }
  if (vis_type %in% c("2d", "surface") && n_features != 2) {
    stop("2D and surface visualizations require a task with exactly 2 features.")
  }
  
  # Create appropriate visualizer
  if (vis_type == "1d") {
    return(Visualizer1DModel$new(x, learner, xlim = x1_limits, n_points = n_points, ...))
  } else if (vis_type == "2d") {
    return(Visualizer2DModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...))
  } else if (vis_type == "surface") {
    return(VisualizerSurfaceModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...))
  } else {
    stop("Unknown visualization type.")
  }
}

#' @template param_type
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.Objective <- function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "surface"))
  n_dim <- x$xdim
  
  # Determine visualization type
  if (type == "auto") {
    if (n_dim == 1) {
      vis_type <- "1d"
    } else if (n_dim == 2) {
      vis_type <- "2d"  # Default to ggplot2 for 2D
    } else {
      stop("Auto visualization only supports 1D and 2D objectives. For higher dimensions, please specify type explicitly.")
    }
  } else {
    vis_type <- type
  }
  
  # Validate type against dimensions
  if (vis_type == "1d" && n_dim != 1) {
    stop("1D visualization requires an objective with exactly 1 dimension.")
  }
  if (vis_type %in% c("2d", "surface") && n_dim != 2) {
    stop("2D and surface visualizations require an objective with exactly 2 dimensions.")
  }
  
  # Create appropriate visualizer
  if (vis_type == "1d") {
    return(Visualizer1DObj$new(x, xlim = x1_limits, n_points = n_points))
  } else if (vis_type == "2d") {
    return(Visualizer2DObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points))
  } else if (vis_type == "surface") {
    return(VisualizerSurfaceObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points))
  } else {
    stop("Unknown visualization type.")
  }
}

#' @template param_type
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @rdname as_visualizer
#' @export
as_visualizer.LossFunction <- function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d"))
  # For loss functions, only 1D visualization is supported
  if (type != "auto" && type != "1d") {
    stop("Only 1D visualization is currently supported for LossFunction.")
  }
  return(VisualizerLossFuns$new(list(x), ...)) # Pass additional arguments
}
