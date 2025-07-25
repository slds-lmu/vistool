#' @title Convert to visualizer
#'
#' @description
#' This function converts to a visualizer. Automatically chooses between 1D and 2D
#' visualizations based on the number of features/dimensions using ggplot2 backend.
#' For 2D inputs, you can optionally use `type = "surface"` to get interactive plotly
#' surface plots (available for Models and Objectives only).
#'
#' @template param_x
#' @template param_type
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @param y_pred (`numeric()`)\cr
#'   Predicted values (used for loss function visualizations).
#' @param y_true (`numeric()`)\cr
#'   True values (used for loss function visualizations).
#' @param input_type (`character(1)`)\cr
#'   `"auto"` (default), `"score"` or `"probability"`. Passed through to
#'   the loss visualiser.
#' @param y_curves (`character(1)`)\cr
#'   Which response curve(s) to draw when `input_type = "probability"`.
#'   One of `"both"`, `"y1"`, or `"y0"`.
#' @template param_learner
#' @template param_default_color_palette
#' @template param_default_text_size
#' @template param_default_theme
#' @param default_alpha (`numeric(1)`)\cr
#'   Default alpha transparency for visual elements. Default is 0.8.
#' @param default_line_width (`numeric(1)`)\cr
#'   Default line width for traces and boundaries. Default is 1.2.
#' @param default_point_size (`numeric(1)`)\cr
#'   Default point size for markers and training data. Default is 2.
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
as_visualizer <- function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, 
                         padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                         input_type = "auto", y_curves = "both", learner = NULL,
                         default_color_palette = "viridis", default_text_size = 11, 
                         default_theme = "minimal", default_alpha = 0.8, 
                         default_line_width = 1.2, default_point_size = 2, ...) {
  UseMethod("as_visualizer")
}

#' @rdname as_visualizer
#' @export
as_visualizer.Task <- function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, 
                              padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                              input_type = "auto", y_curves = "both", learner = NULL,
                              default_color_palette = "viridis", default_text_size = 11, 
                              default_theme = "minimal", default_alpha = 0.8, 
                              default_line_width = 1.2, default_point_size = 2, ...) {
  if (is.null(learner)) {
    stop("Argument 'learner' is required for Task visualizations")
  }
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
    vis <- Visualizer1DModel$new(x, learner, xlim = x1_limits, n_points = n_points, ...)
  } else if (vis_type == "2d") {
    vis <- Visualizer2DModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, 
                                padding = padding, n_points = n_points, ...)
  } else if (vis_type == "surface") {
    vis <- VisualizerSurfaceModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, 
                                     padding = padding, n_points = n_points, ...)
  } else {
    stop("Unknown visualization type.")
  }
  
  # Initialize defaults
  vis$initialize_defaults(default_color_palette, default_text_size, default_theme,
                         default_alpha, default_line_width, default_point_size)
  
  return(vis)
}

#' @rdname as_visualizer
#' @export
as_visualizer.Objective <- function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, 
                                   padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                                   input_type = "auto", y_curves = "both", learner = NULL,
                                   default_color_palette = "viridis", default_text_size = 11, 
                                   default_theme = "minimal", default_alpha = 0.8, 
                                   default_line_width = 1.2, default_point_size = 2, ...) {
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
    return(Visualizer1DObj$new(x, xlim = x1_limits, n_points = n_points, ...))
  } else if (vis_type == "2d") {
    return(Visualizer2DObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...))
  } else if (vis_type == "surface") {
    return(VisualizerSurfaceObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...))
  } else {
    stop("Unknown visualization type.")
  }
}

#' @rdname as_visualizer
#' @export
as_visualizer.LossFunction <- function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                       padding = 0, n_points = 1000L,
                                       y_pred = NULL, y_true = NULL,
                                       input_type = "auto", y_curves = "both", learner = NULL,
                                       default_color_palette = "viridis", default_text_size = 11, 
                                       default_theme = "minimal", default_alpha = 0.8, 
                                       default_line_width = 1.2, default_point_size = 2, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d"))
  checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
  if (type != "auto" && type != "1d") {
    stop("Only 1D visualization is currently supported for loss functions")
  }
  return(Visualizer1DLossFuns$new(losses = list(x), y_pred = y_pred, y_true = y_true,
           n_points = n_points, input_type = input_type, y_curves = y_curves, ...))
}

#' @rdname as_visualizer
#' @export
as_visualizer.list <- function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                padding = 0, n_points = 1000L,
                                y_pred = NULL, y_true = NULL,
                                input_type = "auto", y_curves = "both", learner = NULL,
                                default_color_palette = "viridis", default_text_size = 11, 
                                default_theme = "minimal", default_alpha = 0.8, 
                                default_line_width = 1.2, default_point_size = 2, ...) {
  # Check all elements are LossFunction objects
  invalid_indices <- which(!vapply(x, function(obj) inherits(obj, "LossFunction"), logical(1)))
  if (length(invalid_indices) > 0) {
    stop(sprintf("The following elements of the list are not LossFunction objects: %s", paste(invalid_indices, collapse = ", ")))
  }
  checkmate::assert_choice(type, choices = c("auto", "1d"))
  checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
  if (type != "auto" && type != "1d") {
    stop("Only 1D visualization is currently supported for loss functions")
  }
  return(Visualizer1DLossFuns$new(losses = x, y_pred = y_pred, y_true = y_true,
           n_points = n_points, input_type = input_type, y_curves = y_curves, ...))
}
