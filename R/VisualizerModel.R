#' @title Visualize Model (Unified 1D/2D)
#'
#' @description
#' This class provides a unified interface for visualizing machine learning models
#' on both 1D and 2D tasks. It automatically detects the dimensionality and
#' creates appropriate visualizations using ggplot2.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#' @template param_hypothesis
#' @template param_domain
#' @param retrain (`logical(1)`)
#'   Whether to (re)train the supplied learner on the task (default `TRUE`).
#'   Set to `FALSE` to reuse an already trained learner. If set to `FALSE` but the
#'   learner has not yet been trained (`learner$model` is `NULL`), a warning is emitted
#'   and training is performed to ensure predictions are available.
#'
#' @export
VisualizerModel = R6::R6Class("VisualizerModel",
  inherit = Visualizer,
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
    #' @param x1_limits (`numeric(2)`)\cr
    #'   Limits for the first feature axis. For 1D tasks, this controls the x-axis range.
    #'   If NULL, will be determined from task data.
    #' @param x2_limits (`numeric(2)`)\cr
    #'   Limits for the second feature axis (2D tasks only). Ignored for 1D tasks.
    #'   If NULL, will be determined from task data.
    #' @template param_padding
    #' @template param_n_points
    initialize = function(task, learner, x1_limits = NULL, x2_limits = NULL,
                          padding = 0, n_points = 100L, hypothesis = NULL, domain = NULL,
                          retrain = TRUE) {
      # Validate inputs
      if (!is.null(learner) && !is.null(hypothesis)) stop("Provide only one of learner or hypothesis")
      if (!is.null(task)) self$task = mlr3::assert_task(task)
      if (!is.null(learner)) self$learner = mlr3::assert_learner(learner, task = self$task)
      if (!is.null(hypothesis)) assert_hypothesis(hypothesis)
      private$.hypothesis = hypothesis
      private$.domain = domain
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_number(padding, lower = 0)
      checkmate::assert_count(n_points)

      # Determine dimensionality
      n_features = if (!is.null(self$task)) length(self$task$feature_names) else hypothesis$input_dim
      private$.dimensionality = if (n_features == 1) {
        "1d"
      } else if (n_features == 2) {
        "2d"
      } else {
        stop("VisualizerModel supports only 1D and 2D tasks.")
      }

      # Optionally (re)train the learner
      if (!is.null(self$learner)) internal_maybe_train(self$learner, self$task, retrain)

      # Initialize appropriate data structure
      if (private$.dimensionality == "1d") {
        private$initialize_1d_data(x1_limits, n_points)
      } else {
        private$initialize_2d_data(x1_limits, x2_limits, padding, n_points)
      }
    },

    #' @description
    #' Add points with optional residual loss geometry (1D regression only for now).
    #' Extends base add_points() with a `loss` argument to visualize residuals.
    #' @param points (`data.frame`|`matrix`|`list`) Points to add. For 1D these should contain columns `x` and `y` (observed y required for residuals).
    #' @param color (`character(1)`) Color of the points or "auto". Default "auto".
    #' @param size (`numeric(1)`|`NULL`) Point size. If NULL uses theme default.
    #' @param shape (`integer(1)`|`character(1)`) Point shape. Default 19.
    #' @param alpha (`numeric(1)`|`NULL`) Alpha transparency for points.
    #' @param annotations (`character()`|`NULL`) Optional text annotations for points.
    #' @param annotation_size (`numeric(1)`|`NULL`) Size of annotation text.
    #' @param ordered (`logical(1)`) Whether points should be connected in order (arrows). Default FALSE.
    #' @param arrow_color (`character(1)`|`NULL`) Arrow color when ordered = TRUE.
    #' @param arrow_size (`numeric(1)`) Arrow size when ordered = TRUE. Default 0.3.
    #' @param loss (`character(1)`|`NULL`) One of `"l2_se"` (aliases: `"l2"`, `"se"`), `"l1_ae"` (aliases: `"l1"`, `"abs"`, `"mae"`), or `NULL` (no geometry).
    #' @param loss_params (`list()`) Reserved for future loss-specific parameters (e.g. Huber delta).
    #' @param loss_fill (`character(1)`) Fill color for L2 squares. "auto" draws from palette.
    #' @param loss_alpha (`numeric(1)`|`NULL`) Fill alpha for L2 squares (defaults to theme$alpha * 0.4).
    #' @param loss_color (`character(1)`|`NA`) Color for residual segment / square border. If `NA`, derived from fill.
    #' @param loss_linetype (`character(1)`) Line type for residual segment. Default "solid".
    #' @return Invisible self.
    add_points = function(points, color = "auto", size = NULL, shape = 19, alpha = NULL,
                          annotations = NULL, annotation_size = NULL, ordered = FALSE,
                          arrow_color = NULL, arrow_size = 0.3,
                          loss = NULL, loss_params = list(),
                          loss_fill = "auto", loss_alpha = NULL, loss_color = NA, loss_linetype = "solid") {
      # First store the regular points layer via base implementation
      super$add_points(points = points, color = color, size = size, shape = shape, alpha = alpha,
        annotations = annotations, annotation_size = annotation_size, ordered = ordered,
        arrow_color = arrow_color, arrow_size = arrow_size)

      if (is.null(loss)) return(invisible(self))
      loss_id = tolower(loss)
      if (loss_id %in% c("l2", "se")) loss_id = "l2_se"
      if (loss_id %in% c("l1", "abs", "mae")) loss_id = "l1_ae"
      if (!loss_id %in% c("l2_se", "l1_ae")) {
        warning(sprintf("Unknown loss '%s' - ignoring loss geometry.", loss))
        return(invisible(self))
      }

      # Only 1D regression currently supported
      is_classif = (!is.null(self$task) && self$task$task_type == "classif") || (!is.null(private$.hypothesis) && private$.hypothesis$type == "classif")
      if (private$.dimensionality != "1d" || is_classif) {
        message("Loss geometry currently only implemented for 1D regression - argument 'loss' ignored.")
        return(invisible(self))
      }

      # Prepare points (need observed y to compute residuals)
      pts = private$prepare_points_data(points, "1D")
      if (!"y" %in% names(pts) || all(is.na(pts$y))) {
        message("Observed y values missing - cannot compute residual geometry.")
        return(invisible(self))
      }

      # Interpolate prediction line at provided x's
      x_grid = private$.data_structure$coordinates$x1
      y_grid = private$.data_structure$coordinates$y
      preds = stats::approx(x = x_grid, y = y_grid, xout = pts$x, rule = 2)$y
      residuals = pts$y - preds

      # Minimal width to avoid zero-width rectangles
      x_range = diff(range(x_grid))
      min_width = x_range / 500

      data_list = list()
      if (loss_id == "l2_se") {
        rect_df = lapply(seq_along(residuals), function(i) {
          r = residuals[i]
          s = abs(r)
          w = max(s, min_width)
          y_pred_i = preds[i]
          y_obs_i = pts$y[i]
          data.frame(
            x = pts$x[i],
            xmin = pts$x[i],
            xmax = pts$x[i] + w, # extend to right
            ymin = min(y_pred_i, y_obs_i),
            ymax = max(y_pred_i, y_obs_i),
            y_pred = y_pred_i,
            y_obs = y_obs_i,
            residual = r,
            side_len = s
          )
        })
        rect_df = do.call(rbind, rect_df)
        seg_df = data.frame(
          x = pts$x,
          xend = pts$x,
          y = preds,
          yend = pts$y,
          residual = residuals
        )
        data_list$rect = rect_df
        data_list$segment = seg_df
      } else if (loss_id == "l1_ae") {
        seg_df = data.frame(
          x = pts$x,
          xend = pts$x,
          y = preds,
          yend = pts$y,
          residual = residuals
        )
        data_list$segment = seg_df
      }

      private$store_layer("loss_geom", list(
        loss_fun_id = loss_id,
        data = data_list,
        style = list(
          fill = loss_fill,
          alpha = loss_alpha,
          color = loss_color,
          linetype = loss_linetype
        )
      ))
      invisible(self)
    },

    #' @description
    #' Adds the training data to the plot.
    #'
    #' @param color (`character(1)` or named `character`)\cr
    #'   Color of the points. For classification tasks:
    #'   - `character(1)`: A single color for all points (e.g., `"blue"`) or `"auto"` for automatic color assignment.
    #'   - `named character`: A vector mapping class labels to colors (e.g., `c(pos = "red", neg = "blue")`).
    #'   For regression tasks, only single colors are supported. Default is `"auto"`.
    #' @param size (`numeric(1)`)\cr
    #'   Size of the points. If NULL, uses theme$point_size. Default is NULL.
    #' @param shape (`numeric(1)` or named `numeric`)\cr
    #'   Shape of the points. For classification tasks, can be a named vector mapping class labels to shapes.
    #'   Default is 19 (filled circle).
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of the points. If NULL, uses theme$alpha. Default is NULL.
    #' @param show_labels (`logical(1)`)\cr
    #'   Whether to show data point labels. Default is FALSE.
    #' @param label_size (`numeric(1)`)\cr
    #'   Size of data point labels. If NULL, defaults to smaller text.
    #' @template return_self_invisible
    add_training_data = function(color = "auto", size = NULL, shape = 19, alpha = NULL,
                                 show_labels = FALSE, label_size = NULL) {
      # Validate arguments
      checkmate::assert(
        checkmate::check_string(color),
        checkmate::check_character(color, names = "named", min.len = 1)
      )
      checkmate::assert_number(size, lower = 0, null.ok = TRUE)
      checkmate::assert(
        checkmate::check_number(shape),
        checkmate::check_numeric(shape, names = "named", min.len = 1)
      )
      checkmate::assert_number(alpha, lower = 0, upper = 1, null.ok = TRUE)
      checkmate::assert_flag(show_labels)
      checkmate::assert_number(label_size, lower = 0, null.ok = TRUE)

      # Training data can only be added when a Task is available
      if (is.null(self$task)) {
        stop("Training data not available without a Task")
      }

      # Get training data in dimension-appropriate format
      training_data = private$get_training_data()

      # Store training data specification without resolving colors yet
      private$store_layer("training_data", list(
        data = training_data,
        style = list(
          color = color,
          size = size,
          shape = shape,
          alpha = alpha,
          show_labels = show_labels,
          label_size = label_size
        )
      ))

      return(invisible(self))
    },

    #' @description
    #' Adds boundary lines/contours to the plot.
    #'
    #' @param values (`numeric()`)\cr
    #'   Values at which to draw boundaries. For 1D: horizontal lines (y-values).
    #'   For 2D: contour lines (z-values). If NULL, uses sensible defaults based on prediction type.
    #' @param color (`character(1)`)\cr
    #'   Color of the boundary lines. Default is "black".
    #' @param linetype (`character(1)`)\cr
    #'   Line type for boundaries. For 1D: ggplot2 linetypes. For 2D: contour line types.
    #'   Default is "dashed".
    #' @param linewidth (`numeric(1)`)\cr
    #'   Width of boundary lines. If NULL, uses theme$line_width.
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of boundary lines. If NULL, uses theme$alpha.
    #' @template return_self_invisible
    add_boundary = function(values = NULL, color = "black", linetype = "dashed",
                            linewidth = NULL, alpha = NULL) {
      checkmate::assert_numeric(values, null.ok = TRUE)
      checkmate::assert_string(color)
      checkmate::assert_string(linetype)
      checkmate::assert_number(linewidth, lower = 0, null.ok = TRUE)
      checkmate::assert_number(alpha, lower = 0, upper = 1, null.ok = TRUE)

      # Determine default values based on dimensionality and prediction type
      if (is.null(values)) {
        if (!is.null(self$learner) && self$learner$predict_type == "prob") {
          values = 0.5
        } else if (!is.null(private$.hypothesis) && private$.hypothesis$type == "classif") {
          values = 0.5
        } else {
          # For regression or response predictions, use median of predictions
          values = median(private$.data_structure$coordinates$y, na.rm = TRUE)
        }
      }

      # Validate boundary values are reasonable
      private$validate_boundary_values(values)

      # Store boundary specification
      private$store_layer("boundary", list(
        values = values,
        color = color,
        linetype = linetype,
        linewidth = linewidth,
        alpha = alpha
      ))

      return(invisible(self))
    },

    #' @description
    #' Create and return the ggplot2 plot with model-specific layers.
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(...) {
      # Call parent plot method for validation and settings storage
      super$plot(...)

      # Resolve layer colors before rendering
      self$resolve_layer_colors()

      # Initialize the base plot based on dimensionality
      if (private$.dimensionality == "1d") {
        private$init_1d_plot()
      } else {
        private$init_2d_plot()
      }

      # Render stored layers in the order they were added
      private$render_all_layers()

      private$.last_plot = private$.plot
      return(private$.plot)
    }
  ),
  private = list(
    .dimensionality = NULL, # "1d" or "2d"
    .data_structure = NULL, # Unified data container
    .plot = NULL, # The actual ggplot2 object
    .hypothesis = NULL,
    .domain = NULL,

    # Render all stored layers in the order they were added
    render_all_layers = function() {
      if (!is.null(private$.layers_to_add)) {
        for (layer in private$.layers_to_add) {
          if (layer$type == "training_data") {
            private$render_training_data_layer(layer$spec)
          } else if (layer$type == "boundary") {
            private$render_boundary_layer(layer$spec)
          } else if (layer$type == "points") {
            # Handle points layer using the base class method
            private$.plot = private$add_points_to_ggplot(private$.plot, private$.dimensionality)
          } else if (layer$type == "loss_geom") {
            private$render_loss_geom_layer(layer$spec)
          }
        }
      }

      ranges = list(
        x = private$.data_structure$coordinates$x1,
        y = if (private$.dimensionality == "1d") private$.data_structure$coordinates$y else private$.data_structure$coordinates$x2,
        z = if (private$.dimensionality == "2d") private$.data_structure$coordinates$y else NULL
      )
      private$.plot = private$add_annotations_to_ggplot(private$.plot, private$.dimensionality, ranges)
    },

    # Initialize 1D plot with function line
    init_1d_plot = function() {
      # Use helper method with model-specific geom layer
      private$gg_init_1d(private$.data_structure, function(plot_obj, plot_data, eff) {
        line_color = get_vistool_color(1, "discrete", base_palette = eff$palette)
        plot_obj + ggplot2::geom_line(color = line_color, linewidth = if (is.null(eff$line_width)) 1.2 else eff$line_width)
      })
    },

    # Initialize 2D plot with filled contour/raster
    init_2d_plot = function() {
      # Use helper method with model-specific geom layer
      private$gg_init_2d(private$.data_structure, function(plot_obj, plot_data, eff) {
        # Add filled contour or raster
        plot_obj = plot_obj + ggplot2::geom_raster(ggplot2::aes(fill = y), alpha = eff$alpha)
        # Apply color scale
        private$apply_ggplot_color_scale(plot_obj, eff$palette, "fill")
      })
    },

    # Render training data layer
    render_training_data_layer = function(layer_spec) {
      training_data = layer_spec$data
      style = layer_spec$style
      is_classification = (!is.null(self$task) && self$task$task_type == "classif") || (!is.null(private$.hypothesis) && private$.hypothesis$type == "classif")

      # Resolve style defaults from effective theme
      eff = private$.effective_theme
      resolved_size = if (is.null(style$size)) eff$point_size else style$size
      resolved_alpha = if (is.null(style$alpha)) eff$alpha else style$alpha

      if (private$.dimensionality == "1d") {
        points_data = data.frame(
          x = training_data$x,
          y = training_data$y
        )

        # Handle class-specific styling for classification
        if (is_classification && (length(style$color) > 1 || length(style$shape) > 1 || style$color[1] == "auto")) {
          points_data$class = as.character(training_data$y_original)

          use_color_aes = length(style$color) > 1 || style$color[1] == "auto"
          use_shape_aes = length(style$shape) > 1

          if (use_color_aes && use_shape_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x, y = y, color = class, shape = class),
              size = resolved_size, alpha = resolved_alpha, inherit.aes = FALSE
            )
          } else if (use_color_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x, y = y, color = class),
              size = resolved_size, shape = if (length(style$shape) == 1) style$shape else 19,
              alpha = resolved_alpha, inherit.aes = FALSE
            )
          } else if (use_shape_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x, y = y, shape = class),
              color = if (length(style$color) == 1 && style$color[1] != "auto") style$color[1] else "black",
              size = resolved_size, alpha = resolved_alpha, inherit.aes = FALSE
            )
          }

          # Add manual scales if needed
          if (length(style$color) > 1) {
            private$.plot = private$.plot + ggplot2::scale_color_manual(values = style$color, name = self$task$target_names)
          } else if (style$color[1] == "auto") {
            # Apply themed discrete color scale for automatic colors
            private$.plot = private$apply_ggplot_color_scale(private$.plot, eff$palette, "color", discrete = TRUE)
          }
          if (length(style$shape) > 1) {
            private$.plot = private$.plot + ggplot2::scale_shape_manual(values = style$shape, name = self$task$target_names)
          }
        } else {
          # Single styling - color should already be resolved by base class
          private$.plot = private$.plot + ggplot2::geom_point(
            data = points_data,
            ggplot2::aes(x = x, y = y),
            color = style$color[1], size = resolved_size,
            shape = if (length(style$shape) > 1) style$shape[1] else style$shape,
            alpha = resolved_alpha, inherit.aes = FALSE
          )
        }
      } else {
        # 2D case
        points_data = data.frame(
          x1 = training_data$x1,
          x2 = training_data$x2,
          y = training_data$y
        )

        if (is_classification && (length(style$color) > 1 || length(style$shape) > 1 || style$color[1] == "auto")) {
          points_data$class = as.character(training_data$y_original)

          use_color_aes = length(style$color) > 1 || style$color[1] == "auto"
          use_shape_aes = length(style$shape) > 1

          if (use_color_aes && use_shape_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x1, y = x2, color = class, shape = class),
              size = resolved_size, alpha = resolved_alpha, inherit.aes = FALSE
            )
          } else if (use_color_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x1, y = x2, color = class),
              size = resolved_size, shape = if (length(style$shape) == 1) style$shape else 19,
              alpha = resolved_alpha, inherit.aes = FALSE
            )
          } else if (use_shape_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x1, y = x2, shape = class),
              color = if (length(style$color) == 1 && style$color[1] != "auto") style$color[1] else "black",
              size = resolved_size, alpha = resolved_alpha, inherit.aes = FALSE
            )
          }

          # Add manual scales if needed
          if (length(style$color) > 1) {
            private$.plot = private$.plot + ggplot2::scale_color_manual(values = style$color, name = self$task$target_names)
          } else if (style$color[1] == "auto") {
            # Apply themed discrete color scale for automatic colors
            private$.plot = private$apply_ggplot_color_scale(private$.plot, eff$palette, "color", discrete = TRUE)
          }
          if (length(style$shape) > 1) {
            private$.plot = private$.plot + ggplot2::scale_shape_manual(values = style$shape, name = self$task$target_names)
          }
        } else {
          # Single styling - color should already be resolved by base class
          private$.plot = private$.plot + ggplot2::geom_point(
            data = points_data,
            ggplot2::aes(x = x1, y = x2),
            color = style$color[1], size = resolved_size,
            shape = if (length(style$shape) > 1) style$shape[1] else style$shape,
            alpha = resolved_alpha, inherit.aes = FALSE
          )
        }
      }

      # Add labels if requested
      if (style$show_labels) {
        label_size = if (!is.null(style$label_size)) style$label_size else 3
        if (private$.dimensionality == "1d") {
          private$.plot = private$.plot + ggplot2::geom_text(
            data = points_data,
            ggplot2::aes(x = x, y = y, label = rownames(points_data)),
            size = label_size, nudge_y = 0.02 * diff(range(points_data$y)),
            inherit.aes = FALSE
          )
        } else {
          private$.plot = private$.plot + ggplot2::geom_text(
            data = points_data,
            ggplot2::aes(x = x1, y = x2, label = rownames(points_data)),
            size = label_size, nudge_y = 0.02 * diff(range(points_data$x2)),
            inherit.aes = FALSE
          )
        }
      }
    },

    # Render boundary layer
    render_boundary_layer = function(layer_spec) {
      if (private$.dimensionality == "1d") {
        eff = private$.effective_theme
        resolved_linewidth = if (is.null(layer_spec$linewidth)) eff$line_width else layer_spec$linewidth
        resolved_alpha = if (is.null(layer_spec$alpha)) eff$alpha else layer_spec$alpha
        # Add horizontal lines for 1D
        for (value in layer_spec$values) {
          private$.plot = private$.plot + ggplot2::geom_hline(
            yintercept = value,
            color = layer_spec$color,
            linetype = layer_spec$linetype,
            linewidth = resolved_linewidth,
            alpha = resolved_alpha
          )
        }
      } else {
        eff = private$.effective_theme
        resolved_linewidth = if (is.null(layer_spec$linewidth)) eff$line_width else layer_spec$linewidth
        resolved_alpha = if (is.null(layer_spec$alpha)) eff$alpha else layer_spec$alpha
        # Add contour lines for 2D
        contour_data = data.frame(
          x1 = private$.data_structure$coordinates$x1,
          x2 = private$.data_structure$coordinates$x2,
          y = private$.data_structure$coordinates$y
        )

        private$.plot = private$.plot + ggplot2::geom_contour(
          data = contour_data,
          ggplot2::aes(x = x1, y = x2, z = y),
          breaks = layer_spec$values,
          color = layer_spec$color,
          linewidth = resolved_linewidth,
          linetype = layer_spec$linetype,
          alpha = resolved_alpha,
          inherit.aes = FALSE
        )
      }
    },

    # Initialize data structure for 1D tasks
    initialize_1d_data = function(x1_limits, n_points) {
      # Get feature and target information
      if (!is.null(self$task)) {
        feature_name = self$task$feature_names[1]
        target_name = self$task$target_names
      } else {
        feature_name = private$.hypothesis$predictors[1]
        target_name = if (private$.hypothesis$type == "regr") "y" else "p"
      }

      # Determine x limits
      if (is.null(x1_limits)) {
        if (!is.null(self$task)) {
          x1_limits = range(self$task$data()[[feature_name]])
        } else if (!is.null(private$.domain)) {
          x1_limits = private$.domain[[feature_name]]
        } else if (!is.null(private$.hypothesis$domain)) {
          x1_limits = private$.hypothesis$domain[[feature_name]]
        } else {
          stop("x1_limits or domain must be provided for hypothesis-only visualization")
        }
      }

      # Generate prediction grid
      x_pred = seq(x1_limits[1], x1_limits[2], length.out = n_points)
      newdata = as.data.table(x_pred)
      setnames(newdata, feature_name)

      # Handle integer features (only when task exists)
      if (!is.null(self$task)) {
        original_types = sapply(self$task$data()[, feature_name, with = FALSE], class)
        if (original_types[feature_name] == "integer") {
          newdata[[feature_name]] = as.integer(round(newdata[[feature_name]]))
        }
      }

      # Generate predictions
      if (!is.null(self$learner)) {
        y_pred = self$learner$predict_newdata(newdata)
        y_values = private$process_predictions(y_pred)
      } else {
        y_values = private$.hypothesis$predict(newdata)
      }

      # Store in unified data structure
      private$.data_structure = list(
        dimensionality = "1d",
        coordinates = list(
          x1 = x_pred,
          x2 = NULL,
          y = y_values
        ),
        labels = list(
          title = if (!is.null(self$learner)) sprintf("%s on %s", self$learner$id, self$task$id) else "Hypothesis",
          x1 = feature_name,
          x2 = NULL,
          y = target_name
        ),
        limits = list(
          x1 = x1_limits,
          x2 = NULL
        )
      )
    },

    # Initialize data structure for 2D tasks
    initialize_2d_data = function(x1_limits, x2_limits, padding, n_points) {
      # Get feature and target information
      if (!is.null(self$task)) {
        feature_names = self$task$feature_names
        target_name = self$task$target_names
        data = self$task$data()
      } else {
        feature_names = private$.hypothesis$predictors
        target_name = if (private$.hypothesis$type == "regr") "y" else "p"
        data = NULL
      }

      # Determine limits
      if (is.null(x1_limits)) {
        x1_limits = if (!is.null(data)) range(data[, feature_names[1], with = FALSE]) else private$.domain[[feature_names[1]]]
      }
      if (is.null(x2_limits)) {
        x2_limits = if (!is.null(data)) range(data[, feature_names[2], with = FALSE]) else private$.domain[[feature_names[2]]]
      }

      # Apply padding
      x1_pad = (x1_limits[2] - x1_limits[1]) * padding
      x2_pad = (x2_limits[2] - x2_limits[1]) * padding

      # Generate prediction grid
      x1 = seq(x1_limits[1] - x1_pad, x1_limits[2] + x1_pad, length.out = n_points)
      x2 = seq(x2_limits[1] - x2_pad, x2_limits[2] + x2_pad, length.out = n_points)
      newdata = CJ(x1, x2)
      setnames(newdata, feature_names)

      # Handle integer features when task exists
      if (!is.null(self$task)) {
        original_types = sapply(self$task$data()[, self$task$feature_names, with = FALSE], class)
        for (col in names(original_types)) {
          if (original_types[col] == "integer") {
            newdata[[col]] = as.integer(round(newdata[[col]]))
          }
        }
      }

      # Generate predictions
      if (!is.null(self$learner)) {
        y_pred = self$learner$predict_newdata(newdata)
        y_values = private$process_predictions(y_pred)
      } else {
        y_values = private$.hypothesis$predict(newdata)
      }

      # Store in unified data structure
      private$.data_structure = list(
        dimensionality = "2d",
        coordinates = list(
          x1 = newdata[, feature_names[1], with = FALSE][[1]],
          x2 = newdata[, feature_names[2], with = FALSE][[1]],
          y = y_values
        ),
        labels = list(
          title = if (!is.null(self$learner)) sprintf("%s on %s", self$learner$id, self$task$id) else "Hypothesis",
          x1 = feature_names[1],
          x2 = feature_names[2],
          y = target_name
        ),
        limits = list(
          x1 = x1_limits,
          x2 = x2_limits
        )
      )
    },

    # Process predictions to extract appropriate values for visualization
    process_predictions = function(y_pred) {
      if (!is.null(self$learner) && self$learner$predict_type == "prob") {
        # For probability predictions
        if (length(self$task$class_names) == 2 && !is.null(self$task$positive)) {
          # Binary classification - use positive class probability
          return(y_pred$prob[, self$task$positive])
        } else if (self$learner$task_type == "classif") {
          # Multi-class - use first class probability
          return(y_pred$prob[, 1])
        } else {
          return(y_pred$prob)
        }
      } else {
        # For response predictions
        y_values = y_pred$response
        if (is.factor(y_values)) {
          # Convert factor response to numeric for visualization
          return(as.numeric(y_values) - 1) # 0-based indexing
        }
        return(y_values)
      }
    },

    # Get training data in appropriate format for dimensionality
    get_training_data = function() {
      if (is.null(self$task)) stop("Training data not available without a Task")
      data = self$task$data()

      if (private$.dimensionality == "1d") {
        feature_name = self$task$feature_names[1]
        target_name = self$task$target_names

        training_x = data[[feature_name]]
        training_y = data[[target_name]]
        training_y_original = training_y

        # Convert factors to numeric for visualization
        if (!is.null(self$learner) && self$learner$predict_type == "prob" && is.factor(training_y)) {
          training_y = as.integer(training_y) - 1
        } else if (!is.null(self$learner) && self$learner$predict_type == "response" && is.factor(training_y)) {
          training_y = as.integer(training_y) - 1
        }

        return(list(x = training_x, y = training_y, y_original = training_y_original))
      } else {
        feature_names = self$task$feature_names
        target_name = self$task$target_names

        training_x1 = data[[feature_names[1]]]
        training_x2 = data[[feature_names[2]]]
        training_y = data[[target_name]]
        training_y_original = training_y

        # Convert factors to numeric for visualization
        if (!is.null(self$learner) && self$learner$predict_type == "prob" && is.factor(training_y)) {
          training_y = as.integer(training_y) - 1
        } else if (!is.null(self$learner) && self$learner$predict_type == "response" && is.factor(training_y)) {
          training_y = as.integer(training_y) - 1
        }

        return(list(x1 = training_x1, x2 = training_x2, y = training_y, y_original = training_y_original))
      }
    },

    # Validate boundary values are within reasonable range
    validate_boundary_values = function(values) {
      y_range = range(private$.data_structure$coordinates$y, na.rm = TRUE)

      if (private$.dimensionality == "1d") {
        # For 1D, check if boundary values are within the prediction range
        invalid_values = values[values < y_range[1] | values > y_range[2]]
        if (length(invalid_values) > 0) {
          warning(sprintf(
            "Boundary values %s are outside the prediction range [%.3f, %.3f] and will not be visible.",
            paste(round(invalid_values, 3), collapse = ", "),
            y_range[1], y_range[2]
          ))
        }
      }
      # For 2D, contour values outside range are handled by ggplot2 naturally
    },

    # Render residual loss geometry layer (1D regression)
    render_loss_geom_layer = function(layer_spec) {
      if (private$.dimensionality != "1d") return()
      eff = private$.effective_theme
      style = layer_spec$style
      loss_id = layer_spec$loss_fun_id
      # L2 squares
      if (loss_id == "l2_se" && !is.null(layer_spec$data$rect)) {
        rect_df = layer_spec$data$rect
        fill_col = if (identical(style$fill, NA)) NA else style$fill
        alpha_rect = if (is.null(style$alpha)) eff$alpha * 0.4 else style$alpha
        private$.plot = private$.plot + ggplot2::geom_rect(
          data = rect_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE,
          fill = fill_col,
          color = NA,
          alpha = alpha_rect
        )
      }
      # Segments for L1 or L2
      if (!is.null(layer_spec$data$segment)) {
        seg_df = layer_spec$data$segment
        base_col = if (is.na(style$color)) style$fill else style$color
        if (identical(base_col, "auto")) base_col = style$fill # after color resolution "auto" becomes hex
        if (is.null(base_col) || is.na(base_col)) base_col = get_vistool_color(1, "discrete", base_palette = eff$palette)
        alpha_seg = if (is.null(style$alpha)) eff$alpha * 0.9 else style$alpha
        private$.plot = private$.plot + ggplot2::geom_segment(
          data = seg_df,
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          inherit.aes = FALSE,
          color = base_col,
          alpha = alpha_seg,
          linetype = style$linetype,
          linewidth = if (is.null(eff$line_width)) 0.6 else eff$line_width * 0.7
        )
      }
    }
  )
)
