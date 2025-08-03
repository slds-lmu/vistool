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
                         padding = 0, n_points = 100L) {
      # Validate inputs
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_number(padding, lower = 0)
      checkmate::assert_count(n_points)
      
      # Determine dimensionality
      n_features = length(task$feature_names)
      private$.dimensionality = if (n_features == 1) "1d" else if (n_features == 2) "2d" else {
        stop("VisualizerModel supports only 1D and 2D tasks.")
      }
      
      # Train the learner
      self$learner$train(task)
      
      # Initialize appropriate data structure
      if (private$.dimensionality == "1d") {
        private$initialize_1d_data(x1_limits, n_points)
      } else {
        private$initialize_2d_data(x1_limits, x2_limits, padding, n_points)
      }
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
    #'   Size of the points. Default is 2.
    #' @param shape (`numeric(1)` or named `numeric`)\cr
    #'   Shape of the points. For classification tasks, can be a named vector mapping class labels to shapes.
    #'   Default is 19 (filled circle).
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of the points. Default is 1.
    #' @param show_labels (`logical(1)`)\cr
    #'   Whether to show data point labels. Default is FALSE.
    #' @param label_size (`numeric(1)`)\cr
    #'   Size of data point labels. If NULL, defaults to smaller text.
    #' @template return_self_invisible
    add_training_data = function(color = "auto", size = 2, shape = 19, alpha = 1, 
                                show_labels = FALSE, label_size = NULL) {
      # Validate arguments
      checkmate::assert(
        checkmate::check_string(color),
        checkmate::check_character(color, names = "named", min.len = 1)
      )
      checkmate::assert_number(size, lower = 0)
      checkmate::assert(
        checkmate::check_number(shape),
        checkmate::check_numeric(shape, names = "named", min.len = 1)
      )
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      checkmate::assert_flag(show_labels)
      checkmate::assert_number(label_size, lower = 0, null.ok = TRUE)
      
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
    #'   Width of boundary lines. Default is 1.
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of boundary lines. Default is 0.8.
    #' @template return_self_invisible
    add_boundary = function(values = NULL, color = "black", linetype = "dashed", 
                           linewidth = 1, alpha = 0.8) {
      checkmate::assert_numeric(values, null.ok = TRUE)
      checkmate::assert_string(color)
      checkmate::assert_string(linetype)
      checkmate::assert_number(linewidth, lower = 0)
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      
      # Determine default values based on dimensionality and prediction type
      if (is.null(values)) {
        if (self$learner$predict_type == "prob") {
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
    #' Create and return the plot with model-specific features.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param title_size (`numeric(1)`)\cr
    #'   Title text size. If NULL, defaults to text_size + 2.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param background (`character(1)`)\cr
    #'   Background color. Default is "white".
    #' @param color_palette (`character(1)`)\cr
    #'   Color palette for the fill scale. One of "viridis", "plasma", "grayscale". Default is "viridis".
    #' @param plot_title (`character(1)`)\cr
    #'   Custom plot title. If NULL, uses default title.
    #' @param plot_subtitle (`character(1)`)\cr
    #'   Plot subtitle. Default is NULL.
    #' @param show_title (`logical(1)`)\cr
    #'   Whether to show the plot title. Default is TRUE.
    #' @param x_lab (`character(1)`)\cr
    #'   Custom x-axis label. If NULL, uses feature name.
    #' @param y_lab (`character(1)`)\cr
    #'   Custom y-axis label. If NULL, uses target name.
    #' @param x_limits (`numeric(2)`)\cr
    #'   X-axis limits as c(min, max). If NULL, uses default limits.
    #' @param y_limits (`numeric(2)`)\cr
    #'   Y-axis limits as c(min, max). If NULL, uses default limits.
    #' @param show_grid (`logical(1)`)\cr
    #'   Whether to show grid lines. Default is TRUE.
    #' @param grid_color (`character(1)`)\cr
    #'   Grid line color. Default is "gray90".
    #' @param show_legend (`logical(1)`)\cr
    #'   Whether to show the legend. Default is TRUE.
    #' @param legend_position (`character(1)`)\cr
    #'   Legend position: "top", "right", "bottom", "left", "none". Default is "right".
    #' @param legend_title (`character(1)`)\cr
    #'   Custom legend title. If NULL, uses default.
    #' @param ... Additional arguments (currently unused).
    #' @return A ggplot2 object.
    plot = function(text_size = 11, title_size = NULL, theme = "minimal", background = "white",
                   color_palette = "viridis", plot_title = NULL, plot_subtitle = NULL, 
                   show_title = TRUE, x_lab = NULL, y_lab = NULL, x_limits = NULL, 
                   y_limits = NULL, show_grid = TRUE, grid_color = "gray90", 
                   show_legend = TRUE, legend_position = "right", legend_title = NULL, ...) {
      
      # Validate arguments
      checkmate::assert_number(text_size, lower = 0)
      checkmate::assert_number(title_size, lower = 0, null.ok = TRUE)
      checkmate::assert_choice(theme, c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      checkmate::assert_string(background)
      checkmate::assert_choice(color_palette, c("viridis", "plasma", "grayscale"))
      checkmate::assert_string(plot_title, null.ok = TRUE)
      checkmate::assert_string(plot_subtitle, null.ok = TRUE)
      checkmate::assert_flag(show_title)
      checkmate::assert_string(x_lab, null.ok = TRUE)
      checkmate::assert_string(y_lab, null.ok = TRUE)
      checkmate::assert_numeric(x_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(y_limits, len = 2, null.ok = TRUE)
      checkmate::assert_flag(show_grid)
      checkmate::assert_string(grid_color)
      checkmate::assert_flag(show_legend)
      checkmate::assert_choice(legend_position, c("top", "right", "bottom", "left", "none"))
      checkmate::assert_string(legend_title, null.ok = TRUE)
      
      # Set title size default
      if (is.null(title_size)) {
        title_size = text_size + 2
      }
      
      # Store plot settings for layer resolution
      private$.plot_settings = list(
        text_size = text_size,
        title_size = title_size,
        theme = theme,
        background = background,
        color_palette = color_palette,
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        show_title = show_title,
        x_lab = x_lab,
        y_lab = y_lab,
        x_limits = x_limits,
        y_limits = y_limits,
        show_grid = show_grid,
        grid_color = grid_color,
        show_legend = show_legend,
        legend_position = legend_position,
        legend_title = legend_title,
        ...
      )
      
      # Call parent plot method to store settings and resolve layer colors
      tryCatch({
        super$plot(text_size = text_size, title_size = title_size, theme = theme, background = background,
                   color_palette = color_palette, plot_title = plot_title, plot_subtitle = plot_subtitle,
                   x_lab = x_lab, y_lab = y_lab, x_limits = x_limits, y_limits = y_limits,
                   show_grid = show_grid, grid_color = grid_color, show_legend = show_legend,
                   legend_position = legend_position, legend_title = legend_title, show_title = show_title)
      }, error = function(e) {
        # Expected error from abstract method - we just wanted the setup
        if (!grepl("Abstract method", e$message)) {
          stop(e)
        }
      })
      
      # Initialize the base plot based on dimensionality
      if (private$.dimensionality == "1d") {
        private$init_1d_plot()
      } else {
        private$init_2d_plot()
      }
      
      # Render stored layers in order
      if (!is.null(private$.layers_to_add)) {
        for (layer in private$.layers_to_add) {
          if (layer$type == "training_data") {
            private$render_training_data_layer(layer$spec)
          } else if (layer$type == "boundary") {
            private$render_boundary_layer(layer$spec)
          }
        }
      }
      
      return(private$.plot)
    }
  ),
  
  private = list(
    .dimensionality = NULL,    # "1d" or "2d"
    .data_structure = NULL,    # Unified data container
    .plot = NULL,              # The actual ggplot2 object
    
    # Initialize 1D plot with function line
    init_1d_plot = function() {
      # Get plot settings
      settings = private$.plot_settings
      
      # Create base data for the function line
      plot_data = data.frame(
        x = private$.data_structure$coordinates$x1,
        y = private$.data_structure$coordinates$y
      )
      
      # Create base ggplot
      private$.plot = private$init_ggplot(plot_data, "x", "y")
      
      # Add the function line
      private$.plot = private$.plot + ggplot2::geom_line(color = "blue", linewidth = 1.2)
      
      # Apply theme and styling
      private$.plot = private$apply_ggplot_theme(private$.plot, settings$text_size, NULL, settings$theme)
      
      # Determine labels
      title_text = if (!is.null(settings$plot_title)) settings$plot_title else private$.data_structure$labels$title
      x_text = if (!is.null(settings$x_lab)) settings$x_lab else private$.data_structure$labels$x1
      y_text = if (!is.null(settings$y_lab)) settings$y_lab else private$.data_structure$labels$y
      
      # Add labels conditionally
      private$.plot = private$.plot + ggplot2::labs(
        title = if (settings$show_title) title_text else NULL,
        subtitle = settings$plot_subtitle,
        x = x_text,
        y = y_text
      )
      
      # Apply axis limits
      if (!is.null(settings$x_limits)) {
        private$.plot = private$.plot + ggplot2::xlim(settings$x_limits)
      }
      if (!is.null(settings$y_limits)) {
        private$.plot = private$.plot + ggplot2::ylim(settings$y_limits)
      }
      
      # Apply grid settings
      if (!settings$show_grid) {
        private$.plot = private$.plot + ggplot2::theme(panel.grid = ggplot2::element_blank())
      } else {
        private$.plot = private$.plot + ggplot2::theme(
          panel.grid = ggplot2::element_line(color = settings$grid_color)
        )
      }
      
      # Apply title size
      private$.plot = private$.plot + ggplot2::theme(
        plot.title = ggplot2::element_text(size = settings$title_size)
      )
      
      # Apply legend settings
      if (!settings$show_legend) {
        private$.plot = private$.plot + ggplot2::theme(legend.position = "none")
      } else if (settings$legend_position != "right") {
        private$.plot = private$.plot + ggplot2::theme(legend.position = settings$legend_position)
      }
    },
    
    # Initialize 2D plot with filled contour/raster
    init_2d_plot = function() {
      # Get plot settings
      settings = private$.plot_settings
      
      # Create base data for the filled contour
      plot_data = data.frame(
        x1 = private$.data_structure$coordinates$x1,
        x2 = private$.data_structure$coordinates$x2,
        y = private$.data_structure$coordinates$y
      )
      
      # Create base ggplot
      private$.plot = private$init_ggplot(plot_data, "x1", "x2")
      
      # Add filled contour or raster
      private$.plot = private$.plot + ggplot2::geom_raster(ggplot2::aes(fill = y), alpha = 0.8)
      
      # Apply color scale
      private$.plot = private$apply_ggplot_color_scale(private$.plot, settings$color_palette, "fill")
      
      # Apply theme and styling
      private$.plot = private$apply_ggplot_theme(private$.plot, settings$text_size, NULL, settings$theme)
      
      # Determine labels
      title_text = if (!is.null(settings$plot_title)) settings$plot_title else private$.data_structure$labels$title
      x_text = if (!is.null(settings$x_lab)) settings$x_lab else private$.data_structure$labels$x1
      y_text = if (!is.null(settings$y_lab)) settings$y_lab else private$.data_structure$labels$x2
      fill_text = if (!is.null(settings$legend_title)) settings$legend_title else private$.data_structure$labels$y
      
      # Add labels conditionally
      private$.plot = private$.plot + ggplot2::labs(
        title = if (settings$show_title) title_text else NULL,
        subtitle = settings$plot_subtitle,
        x = x_text,
        y = y_text,
        fill = fill_text
      )
      
      # Apply axis limits
      if (!is.null(settings$x_limits)) {
        private$.plot = private$.plot + ggplot2::xlim(settings$x_limits)
      }
      if (!is.null(settings$y_limits)) {
        private$.plot = private$.plot + ggplot2::ylim(settings$y_limits)
      }
      
      # Apply grid settings
      if (!settings$show_grid) {
        private$.plot = private$.plot + ggplot2::theme(panel.grid = ggplot2::element_blank())
      } else {
        private$.plot = private$.plot + ggplot2::theme(
          panel.grid = ggplot2::element_line(color = settings$grid_color)
        )
      }
      
      # Apply title size
      private$.plot = private$.plot + ggplot2::theme(
        plot.title = ggplot2::element_text(size = settings$title_size)
      )
      
      # Apply legend settings
      if (!settings$show_legend) {
        private$.plot = private$.plot + ggplot2::theme(legend.position = "none")
      } else if (settings$legend_position != "right") {
        private$.plot = private$.plot + ggplot2::theme(legend.position = settings$legend_position)
      }
    },
    
    # Render training data layer
    render_training_data_layer = function(layer_spec) {
      training_data = layer_spec$data
      style = layer_spec$style
      is_classification = self$task$task_type == "classif"
      
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
              size = style$size, alpha = style$alpha, inherit.aes = FALSE
            )
          } else if (use_color_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x, y = y, color = class),
              size = style$size, shape = if (length(style$shape) == 1) style$shape else 19,
              alpha = style$alpha, inherit.aes = FALSE
            )
          } else if (use_shape_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x, y = y, shape = class),
              color = if (length(style$color) == 1 && style$color[1] != "auto") style$color[1] else "black",
              size = style$size, alpha = style$alpha, inherit.aes = FALSE
            )
          }
          
          # Add manual scales if needed
          if (length(style$color) > 1) {
            private$.plot = private$.plot + ggplot2::scale_color_manual(values = style$color, name = self$task$target_names)
          }
          if (length(style$shape) > 1) {
            private$.plot = private$.plot + ggplot2::scale_shape_manual(values = style$shape, name = self$task$target_names)
          }
        } else {
          # Single styling - color should already be resolved by base class
          private$.plot = private$.plot + ggplot2::geom_point(
            data = points_data,
            ggplot2::aes(x = x, y = y),
            color = style$color[1], size = style$size,
            shape = if (length(style$shape) > 1) style$shape[1] else style$shape,
            alpha = style$alpha, inherit.aes = FALSE
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
              size = style$size, alpha = style$alpha, inherit.aes = FALSE
            )
          } else if (use_color_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x1, y = x2, color = class),
              size = style$size, shape = if (length(style$shape) == 1) style$shape else 19,
              alpha = style$alpha, inherit.aes = FALSE
            )
          } else if (use_shape_aes) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x1, y = x2, shape = class),
              color = if (length(style$color) == 1 && style$color[1] != "auto") style$color[1] else "black",
              size = style$size, alpha = style$alpha, inherit.aes = FALSE
            )
          }
          
          # Add manual scales if needed  
          if (length(style$color) > 1) {
            private$.plot = private$.plot + ggplot2::scale_color_manual(values = style$color, name = self$task$target_names)
          }
          if (length(style$shape) > 1) {
            private$.plot = private$.plot + ggplot2::scale_shape_manual(values = style$shape, name = self$task$target_names)
          }
        } else {
          # Single styling - color should already be resolved by base class
          private$.plot = private$.plot + ggplot2::geom_point(
            data = points_data,
            ggplot2::aes(x = x1, y = x2),
            color = style$color[1], size = style$size,
            shape = if (length(style$shape) > 1) style$shape[1] else style$shape,
            alpha = style$alpha, inherit.aes = FALSE
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
        # Add horizontal lines for 1D
        for (value in layer_spec$values) {
          private$.plot = private$.plot + ggplot2::geom_hline(
            yintercept = value,
            color = layer_spec$color,
            linetype = layer_spec$linetype,
            linewidth = layer_spec$linewidth,
            alpha = layer_spec$alpha
          )
        }
      } else {
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
          linewidth = layer_spec$linewidth,
          linetype = layer_spec$linetype,
          alpha = layer_spec$alpha,
          inherit.aes = FALSE
        )
      }
    },
    
    # Initialize data structure for 1D tasks
    initialize_1d_data = function(x1_limits, n_points) {
      # Get feature and target information
      feature_name = self$task$feature_names[1]
      target_name = self$task$target_names
      
      # Determine x limits
      if (is.null(x1_limits)) {
        x1_limits = range(self$task$data()[[feature_name]])
      }
      
      # Generate prediction grid
      x_pred = seq(x1_limits[1], x1_limits[2], length.out = n_points)
      newdata = as.data.table(x_pred)
      setnames(newdata, feature_name)
      
      # Handle integer features
      original_types = sapply(self$task$data()[, feature_name, with = FALSE], class)
      if (original_types[feature_name] == "integer") {
        newdata[[feature_name]] = as.integer(round(newdata[[feature_name]]))
      }
      
      # Generate predictions
      y_pred = self$learner$predict_newdata(newdata)
      y_values = private$process_predictions(y_pred)
      
      # Store in unified data structure
      private$.data_structure = list(
        dimensionality = "1d",
        coordinates = list(
          x1 = x_pred,
          x2 = NULL,
          y = y_values
        ),
        labels = list(
          title = sprintf("%s on %s", self$learner$id, self$task$id),
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
      feature_names = self$task$feature_names
      target_name = self$task$target_names
      data = self$task$data()
      
      # Determine limits
      if (is.null(x1_limits)) {
        x1_limits = range(data[, feature_names[1], with = FALSE])
      }
      if (is.null(x2_limits)) {
        x2_limits = range(data[, feature_names[2], with = FALSE])
      }
      
      # Apply padding
      x1_pad = (x1_limits[2] - x1_limits[1]) * padding
      x2_pad = (x2_limits[2] - x2_limits[1]) * padding
      
      # Generate prediction grid
      x1 = seq(x1_limits[1] - x1_pad, x1_limits[2] + x1_pad, length.out = n_points)
      x2 = seq(x2_limits[1] - x2_pad, x2_limits[2] + x2_pad, length.out = n_points)
      newdata = CJ(x1, x2)
      setnames(newdata, feature_names)
      
      # Handle integer features
      original_types = sapply(self$task$data()[, feature_names, with = FALSE], class)
      for (col in names(original_types)) {
        if (original_types[col] == "integer") {
          newdata[[col]] = as.integer(round(newdata[[col]]))
        }
      }
      
      # Generate predictions
      y_pred = self$learner$predict_newdata(newdata)
      y_values = private$process_predictions(y_pred)
      
      # Store in unified data structure
      private$.data_structure = list(
        dimensionality = "2d",
        coordinates = list(
          x1 = newdata[, feature_names[1], with = FALSE][[1]],
          x2 = newdata[, feature_names[2], with = FALSE][[1]],
          y = y_values
        ),
        labels = list(
          title = sprintf("%s on %s", self$learner$id, self$task$id),
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
      if (self$learner$predict_type == "prob") {
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
          return(as.numeric(y_values) - 1)  # 0-based indexing
        }
        return(y_values)
      }
    },
    
    # Get training data in appropriate format for dimensionality
    get_training_data = function() {
      data = self$task$data()
      
      if (private$.dimensionality == "1d") {
        feature_name = self$task$feature_names[1]
        target_name = self$task$target_names
        
        training_x = data[[feature_name]]
        training_y = data[[target_name]]
        training_y_original = training_y
        
        # Convert factors to numeric for visualization
        if (self$learner$predict_type == "prob" && is.factor(training_y)) {
          training_y = as.integer(training_y) - 1
        } else if (self$learner$predict_type == "response" && is.factor(training_y)) {
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
        if (self$learner$predict_type == "prob" && is.factor(training_y)) {
          training_y = as.integer(training_y) - 1
        } else if (self$learner$predict_type == "response" && is.factor(training_y)) {
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
    }
  )
)
