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
as_visualizer = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, 
                         padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                         input_type = "auto", y_curves = "both", learner = NULL, ...) {
  UseMethod("as_visualizer")
}

#' @rdname as_visualizer
#' @export
as_visualizer.Task = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, 
                              padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                              input_type = "auto", y_curves = "both", learner = NULL, ...) {
  if (is.null(learner)) {
    stop("Argument 'learner' is required for Task visualizations")
  }
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "surface"))
  n_features = length(x$feature_names)
  
  # Determine visualization type
  if (type == "auto") {
    if (n_features == 1) {
      vis_type = "1d"
    } else if (n_features == 2) {
      vis_type = "2d"  # Default to ggplot2 for 2D
    } else {
      stop("Auto visualization only supports 1D and 2D tasks. For higher dimensions, please specify type explicitly.")
    }
  } else {
    vis_type = type
  }
  
  # Validate type against features
  if (vis_type == "1d" && n_features != 1) {
    stop("1D visualization requires a task with exactly 1 feature.")
  }
  if (vis_type %in% c("2d", "surface") && n_features != 2) {
    stop("2D and surface visualizations require a task with exactly 2 features.")
  }
  
  # Create appropriate visualizer
  if (vis_type %in% c("1d", "2d")) {
    vis = VisualizerModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, 
                              padding = padding, n_points = n_points, ...)
  } else if (vis_type == "surface") {
    vis = VisualizerSurfaceModel$new(x, learner, x1_limits = x1_limits, x2_limits = x2_limits, 
                                     padding = padding, n_points = n_points, ...)
  } else {
    stop("Unknown visualization type.")
  }

  return(vis)
}

#' @rdname as_visualizer
#' @export
as_visualizer.Objective = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL, 
                                   padding = 0, n_points = 100L, y_pred = NULL, y_true = NULL,
                                   input_type = "auto", y_curves = "both", learner = NULL, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d", "2d", "surface"))
  n_dim = x$xdim
  
  # Determine visualization type
  if (type == "auto") {
    if (is.na(n_dim)) {
      stop("Auto visualization type detection not supported for variable-dimension objectives. Please specify type explicitly (1d, 2d, or surface).")
    } else if (n_dim == 1) {
      vis_type = "1d"
    } else if (n_dim == 2) {
      vis_type = "2d"  # Default to ggplot2 for 2D
    } else {
      stop("Auto visualization only supports 1D and 2D objectives. For higher dimensions, please specify type explicitly.")
    }
  } else {
    vis_type = type
  }
  
  # Validate type against dimensions 
  if (!is.na(n_dim)) {
    # For known dimensions, validate the type makes sense
    if (vis_type == "1d" && n_dim != 1) {
      stop("1D visualization requires an objective with exactly 1 dimension.")
    }
    if (vis_type %in% c("2d", "surface") && n_dim != 2) {
      stop("2D and surface visualizations require an objective with exactly 2 dimensions.")
    }
  }
  # For unknown dimensions (NA), trust the user's explicit type specification
  
  # Create appropriate visualizer
  if (vis_type %in% c("1d", "2d")) {
    return(VisualizerObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, 
                             padding = padding, n_points = n_points, type = vis_type, ...))
  } else if (vis_type == "surface") {
    return(VisualizerSurfaceObj$new(x, x1_limits = x1_limits, x2_limits = x2_limits, padding = padding, n_points = n_points, ...))
  } else {
    stop("Unknown visualization type.")
  }
}

#' @rdname as_visualizer
#' @export
as_visualizer.LossFunction = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                       padding = 0, n_points = 1000L,
                                       y_pred = NULL, y_true = NULL,
                                       input_type = "auto", y_curves = "both", learner = NULL, ...) {
  checkmate::assert_choice(type, choices = c("auto", "1d"))
  checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
  if (type != "auto" && type != "1d") {
    stop("Only 1D visualization is currently supported for loss functions")
  }
  return(VisualizerLossFuns$new(losses = list(x), y_pred = y_pred, y_true = y_true,
           n_points = n_points, input_type = input_type, y_curves = y_curves, ...))
}

#' @rdname as_visualizer
#' @export
as_visualizer.list = function(x, type = "auto", x1_limits = NULL, x2_limits = NULL,
                                padding = 0, n_points = 1000L,
                                y_pred = NULL, y_true = NULL,
                                input_type = "auto", y_curves = "both", learner = NULL, ...) {
  # Check all elements are LossFunction objects
  invalid_indices = which(!vapply(x, function(obj) inherits(obj, "LossFunction"), logical(1)))
  if (length(invalid_indices) > 0) {
    stop(sprintf("The following elements of the list are not LossFunction objects: %s", paste(invalid_indices, collapse = ", ")))
  }
  checkmate::assert_choice(type, choices = c("auto", "1d"))
  checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
  if (type != "auto" && type != "1d") {
    stop("Only 1D visualization is currently supported for loss functions")
  }
  return(VisualizerLossFuns$new(losses = x, y_pred = y_pred, y_true = y_true,
           n_points = n_points, input_type = input_type, y_curves = y_curves, ...))
}
