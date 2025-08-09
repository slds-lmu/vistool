#' @title Visualize Model as Interactive Surface
#'
#' @description
#' This class is used to create interactive 3D surface visualizations of learners and tasks
#' for 2D input data using plotly.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
VisualizerSurfaceModel = R6::R6Class("VisualizerSurfaceModel",
  inherit = VisualizerSurface,
  public = list(

    #' @field task (`mlr3::Task`)\cr
    #' Task used to train the model.
    task = NULL,

    #' @field learner (`mlr3::Learner`)\cr
    #' Learner used to train the model.
    learner = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task])\cr
    #'   The task to train the model on.
    #' @template param_learner
    #' @template param_x1_limits
    #' @template param_x2_limits
    #' @template param_padding
    #' @template param_n_points
    initialize = function(task, learner, x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_count(n_points)

      # Validate that task has exactly 2 features
      if (length(self$task$feature_names) != 2) {
        mlr3misc::stopf(
          "3D Model visualization requires a task with exactly 2 features, but got %d",
          length(self$task$feature_names)
        )
      }

      x1 = self$task$feature_names[1]
      x2 = self$task$feature_names[2]
      data = task$data()
      self$learner$train(task)

      x1_limits = range(data[, x1, with = FALSE])
      x2_limits = range(data[, x2, with = FALSE])

      grid = list(
        x1 = seq(x1_limits[1] - padding, x1_limits[2] + padding, length.out = n_points),
        x2 = seq(x2_limits[1] - padding, x2_limits[2] + padding, length.out = n_points)
      )

      newdata = CJ(grid$x1, grid$x2)
      setnames(newdata, self$task$feature_names)

      original_types = sapply(self$task$data()[, self$task$feature_names, with = FALSE], class)
      for (col in names(original_types)) {
        if (original_types[col] == "integer") {
          newdata[[col]] = as.integer(round(newdata[[col]]))
        }
      }
      z = self$learner$predict_newdata(newdata)[[self$learner$predict_type]]
      if (self$learner$predict_type == "prob") {
        pos_class = self$task$positive
        z = z[, pos_class]
      }
      zmat = matrix(z, nrow = n_points, ncol = n_points, byrow = TRUE)

      super$initialize(
        grid = grid,
        zmat = zmat,
        plot_lab = paste(self$learner$id, "on", self$task$id),
        x1_lab = self$task$feature_names[1],
        x2_lab = self$task$feature_names[2],
        z_lab = self$task$target_names
      )

      return(invisible(self))
    },

    #' @description
    #' Adds the training data to the plot.
    #'
    #' @param size (`numeric(1)`)\cr
    #'   Size of the points. Default is 5.
    #' @param color (`character(1)` or named `character`)\cr
    #'   Color of the points. For classification tasks:
    #'   - `character(1)`: A single color for all points (e.g., `"blue"`) or `"auto"` for automatic color assignment.
    #'   - `named character`: A vector mapping class labels to colors (e.g., `c(pos = "red", neg = "blue")`).
    #'   For regression tasks, only single colors are supported. Default is `"auto"`.
    #' @param shape (`numeric(1)` or named `numeric`)\cr
    #'   Shape/symbol of the points. For classification tasks:
    #'   - `numeric(1)`: A single symbol for all points (e.g., `0` for circle).
    #'   - `named numeric`: A vector mapping class labels to symbols (e.g., `c(pos = 0, neg = 1)`).
    #'   For regression tasks, only single symbols are supported. Default is 0 (circle).
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    add_training_data = function(size = 5, color = "auto", shape = 0, ...) {
      # Validate arguments - color can be string or named character vector
      checkmate::assert(
        checkmate::check_string(color),
        checkmate::check_character(color, names = "named", min.len = 1)
      )
      checkmate::assert_number(size, lower = 0)
      # Validate arguments - shape can be numeric or named numeric vector
      checkmate::assert(
        checkmate::check_number(shape),
        checkmate::check_numeric(shape, names = "named", min.len = 1)
      )

      if (is.null(private$.plot)) private$.init_default_plot()
      data = self$task$data()
      x1 = data[, self$task$feature_names[1], with = FALSE][[1]]
      x2 = data[, self$task$feature_names[2], with = FALSE][[1]]
      target = data[, self$task$target_names, with = FALSE][[1]]
      target_original = target # Store original for class mapping

      # For positioning (z-coordinate), use actual target values
      if (self$learner$predict_type == "prob" && is.factor(target)) {
        # For classification with probability predictions, convert factor to numeric for positioning
        z = as.integer(target) - 1 # 0-based for plotting
      } else if (is.factor(target)) {
        # For classification with response predictions
        z = as.integer(target) - 1
      } else {
        # For regression, use target values directly
        z = as.numeric(target)
      }

      # Store training data specification without resolving colors yet
      private$store_layer("training_data", list(
        x1 = x1,
        x2 = x2,
        z = z, # Actual target values for positioning
        target_original = target_original, # Original target for class mapping
        size = size,
        color = color, # Keep for later resolution
        shape = shape,
        args = list(...)
      ))

      return(invisible(self))
    },

    #' @description
    #' Adds boundary surface(s) to the plot at specified values.
    #'
    #' @param values (`numeric()`)\cr
    #'   Vector of z-values where to draw boundary surfaces. For classification with probability predictions,
    #'   defaults to 0.5. For regression or response predictions, defaults to the median of predictions.
    #' @param color (`character(1)` or `list()`)\cr
    #'   Color specification for boundary surfaces. Default uses a neutral colorscale.
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)` or `add_surface(...)`.
    add_boundary = function(values = NULL, color = NULL, ...) {
      checkmate::assert_numeric(values, null.ok = TRUE)

      if (is.null(private$.plot)) private$.init_default_plot()

      # Determine default values based on prediction type
      if (is.null(values)) {
        if (self$learner$predict_type == "prob") {
          values = 0.5
        } else {
          # For regression or response predictions, use median (single central tendency)
          values = median(self$zmat, na.rm = TRUE)
        }
      } else {
        # Validate that boundary values are within the prediction range
        z_range = range(self$zmat, na.rm = TRUE)
        invalid_values = values[values < z_range[1] | values > z_range[2]]
        if (length(invalid_values) > 0) {
          warning(sprintf(
            "Boundary values %s are outside the prediction range [%.3f, %.3f] and will not generate visible surfaces.",
            paste(round(invalid_values, 3), collapse = ", "),
            z_range[1], z_range[2]
          ))
        }
      }

      # Default color scheme
      if (is.null(color)) {
        color = list(c(0, "rgba(176,196,222,0.5)"), c(1, "rgba(160,82,45,0.5)"))
      }

      # Store boundary specification without resolving colors yet
      private$store_layer("boundary", list(
        values = values,
        color = color, # Keep for later resolution
        args = list(...)
      ))

      return(invisible(self))
    },

    #' @description
    #' Create and return the plotly plot with model-specific layers.
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A plotly object.
    plot = function(...) {
      # Call parent first to set up plot_settings and resolve colors
      super$plot(...)

      # render layers in the order they were added
      if (!is.null(private$.layers_to_add)) {
        for (layer in private$.layers_to_add) {
          if (layer$type == "training_data") {
            private$render_training_data_layer(layer$spec)
          } else if (layer$type == "boundary") {
            private$render_boundary_layer(layer$spec)
          }
        }
      }
      private$.last_plot = private$.plot
      return(private$.plot)
    }
  ),
  private = list(
    # Render stored training data layers
    render_training_data_layers = function() {
      training_layers = private$get_layers_by_type("training_data")

      if (length(training_layers) == 0) {
        return()
      }

      for (training_spec in training_layers) {
        private$render_training_data_layer(training_spec)
      }
    },

    # Render a single training data layer
    render_training_data_layer = function(layer_spec) {
      is_classification = self$task$task_type == "classif"

      # Helper function to map ggplot2 shapes to plotly symbols
      map_shape_to_plotly_symbol = function(shape_num) {
        # Map common ggplot2 shape numbers to plotly symbols
        symbol_map = c(
          "0" = "circle-open", # 0: circle open
          "1" = "circle", # 1: circle filled
          "2" = "triangle-up-open", # 2: triangle open
          "3" = "cross-thin", # 3: plus
          "4" = "x-thin", # 4: cross
          "5" = "diamond-open", # 5: diamond open
          "15" = "square", # 15: square filled
          "16" = "circle", # 16: circle filled
          "17" = "triangle-up", # 17: triangle filled
          "18" = "diamond", # 18: diamond filled
          "19" = "circle" # 19: circle filled (default)
        )

        symbol_name = as.character(shape_num)
        if (symbol_name %in% names(symbol_map)) {
          return(symbol_map[symbol_name])
        } else {
          return("circle") # Default fallback
        }
      }

      # Handle class-specific styling for classification tasks
      if (is_classification && (length(layer_spec$color) > 1 || length(layer_spec$shape) > 1 || layer_spec$color[1] == "auto")) {
        # Get unique classes and create separate traces for each
        class_labels = as.character(layer_spec$target_original)
        unique_classes = unique(class_labels)

        for (class_name in unique_classes) {
          class_indices = which(class_labels == class_name)

          # Get color for this class
          class_color = if (length(layer_spec$color) > 1) {
            layer_spec$color[class_name]
          } else if (layer_spec$color[1] == "auto") {
            # Use default plotly colors
            plotly_colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
            plotly_colors[((which(unique_classes == class_name) - 1) %% length(plotly_colors)) + 1]
          } else {
            layer_spec$color[1]
          }

          # Get shape for this class and map to plotly symbol
          class_shape_num = if (length(layer_spec$shape) > 1) {
            layer_spec$shape[class_name]
          } else {
            layer_spec$shape[1]
          }
          class_symbol = map_shape_to_plotly_symbol(class_shape_num)

          if (private$.layer_primary == "contour") {
            private$.plot = private$.plot %>%
              plotly::add_trace(
                x = layer_spec$x1[class_indices],
                y = layer_spec$x2[class_indices],
                type = "scatter",
                mode = "markers",
                marker = list(
                  size = 10,
                  color = class_color,
                  symbol = class_symbol,
                  line = list(color = "black", width = 1)
                ),
                name = paste("Class:", class_name),
                text = ~ paste("x1:", layer_spec$x1[class_indices], "\nx2:", layer_spec$x2[class_indices], "\nClass:", class_name),
                hoverinfo = "text"
              )
          } else {
            private$.plot = private$.plot %>%
              plotly::add_trace(
                x = layer_spec$x1[class_indices],
                y = layer_spec$x2[class_indices],
                z = layer_spec$z[class_indices],
                type = "scatter3d",
                mode = "markers",
                marker = list(
                  size = layer_spec$size,
                  color = class_color,
                  symbol = class_symbol
                ),
                name = paste("Class:", class_name)
              )
          }
        }
      } else {
        # Single color/shape for all points (regression or single-styled classification)
        resolved_color = if (layer_spec$color[1] == "auto") {
          private$get_auto_color_with_palette()
        } else {
          validate_color(layer_spec$color[1])
        }

        resolved_shape_num = if (length(layer_spec$shape) > 1) layer_spec$shape[1] else layer_spec$shape
        resolved_symbol = map_shape_to_plotly_symbol(resolved_shape_num)

        if (private$.layer_primary == "contour") {
          private$.plot = private$.plot %>%
            plotly::add_trace(
              x = layer_spec$x1,
              y = layer_spec$x2,
              type = "scatter",
              mode = "markers",
              marker = list(
                size = 10,
                color = layer_spec$z,
                cmin = min(self$zmat),
                cmax = max(self$zmat),
                colorscale = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")),
                line = list(color = "black", width = 2),
                showscale = FALSE,
                symbol = resolved_symbol
              ),
              text = ~ paste("x1:", layer_spec$x1, "\nx2:", layer_spec$x2, "\nz:", layer_spec$z),
              hoverinfo = "text"
            )
        } else {
          private$.plot = private$.plot %>%
            plotly::add_trace(
              x = layer_spec$x1,
              y = layer_spec$x2,
              z = layer_spec$z,
              type = "scatter3d",
              mode = "markers",
              marker = list(
                size = layer_spec$size,
                color = resolved_color,
                symbol = resolved_symbol
              )
            )
        }
      }
    },

    # Render stored boundary layers
    render_boundary_layers = function() {
      boundary_layers = private$get_layers_by_type("boundary")

      if (length(boundary_layers) == 0) {
        return()
      }

      for (boundary_spec in boundary_layers) {
        private$render_boundary_layer(boundary_spec)
      }
    },

    # Render a single boundary layer
    render_boundary_layer = function(layer_spec) {
      for (value in layer_spec$values) {
        z = matrix(value, nrow = nrow(self$zmat), ncol = ncol(self$zmat), byrow = TRUE)

        if (private$.layer_primary == "contour") {
          llp = list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
          private$.plot = private$.plot %>%
            plotly::add_trace(
              name = paste("boundary", value),
              autocontour = FALSE,
              showlegend = FALSE,
              showscale = FALSE,
              x = llp$x,
              y = llp$y,
              z = t(llp$z),
              type = "contour",
              colorscale = list(c(0, 1), c("rgb(0,0,0)", "rgb(0,0,0)")),
              ncontours = 1,
              contours = list(
                start = value,
                end = value,
                coloring = "lines"
              ),
              line = list(
                color = "black",
                width = 3
              )
            )
        } else {
          private$.plot = private$.plot %>%
            plotly::add_surface(
              x = self$grid$x1,
              y = self$grid$x2,
              z = z,
              colorscale = layer_spec$color,
              showscale = FALSE,
              name = paste("boundary", value)
            )
        }
      }
    }
  )
)
