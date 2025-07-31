#' @title Visualize Model
#'
#' @description
#' This class is used to create 2D visualizations of learners and tasks.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer2DModel = R6::R6Class("Visualizer2DModel",
  inherit = Visualizer2D,
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
    initialize = function(task,
                          learner,
                          x1_limits = NULL,
                          x2_limits = NULL,
                          padding = 0,
                          n_points = 100L) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_count(n_points)
      lab_x1 = self$task$feature_names[1]
      lab_x2 = self$task$feature_names[2]
      data = task$data()
      self$learner$train(task)

      x1_limits = range(data[, lab_x1, with = FALSE])
      x2_limits = range(data[, lab_x2, with = FALSE])

      x1 = seq(x1_limits[1] - padding, x1_limits[2] + padding, length.out = n_points)
      x2 = seq(x2_limits[1] - padding, x2_limits[2] + padding, length.out = n_points)

      newdata = CJ(x1, x2)
      setnames(newdata, self$task$feature_names)

      original_types = sapply(self$task$data()[, self$task$feature_names, with = FALSE], class)
      for (col in names(original_types)) {
        if (original_types[col] == "integer") {
          newdata[[col]] = as.integer(round(newdata[[col]]))
        }
      }
      y = self$learner$predict_newdata(newdata)[[self$learner$predict_type]]
      if (self$learner$predict_type == "prob") {
        # for binary classification, use the positive class
        # for multi-class, use the first class
        if (length(task$class_names) == 2 && !is.null(task$positive)) {
          y = y[, task$positive]
        } else {
          y = y[, 1]
        }
      } else if (self$learner$predict_type == "response" && is.factor(y)) {
        # Convert factor response to numeric for visualization
        y = as.numeric(y) - 1 # Convert to 0-based indexing
      }


      super$initialize(
        fun_x1 = newdata[, lab_x1, with = FALSE][[1]],
        fun_x2 = newdata[, lab_x2, with = FALSE][[1]],
        fun_y = y,
        title = sprintf("%s on %s", self$learner$id, self$task$id),
        lab_x1 = lab_x1,
        lab_x2 = lab_x2,
        lab_y = task$target_names
      )

      return(invisible(self))
    },

    #' @description
    #' Adds the training data to the plot.
    #'
    #' @param color (`character(1)`)\cr
    #'   Color of the points. Use "auto" for automatic color assignment. Default is "auto".
    #' @param size (`numeric(1)`)\cr
    #'   Size of the points. Default is 2.
    #' @param shape (`integer(1)`)\cr
    #'   Shape of the points. Default is 19.
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of the points. Default is 1.
    #' @param show_labels (`logical(1)`)\cr
    #'   Whether to show data point labels. Default is FALSE.
    #' @param label_size (`numeric(1)`)\cr
    #'   Size of data point labels. If NULL, defaults to smaller text.
    add_training_data = function(color = "auto", size = 2, shape = 19, alpha = 1, 
                                show_labels = FALSE, label_size = NULL) {
      checkmate::assert_string(color)
      checkmate::assert_number(size, lower = 0)
      checkmate::assert_integerish(shape, len = 1)
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      checkmate::assert_flag(show_labels)
      checkmate::assert_number(label_size, lower = 0, null.ok = TRUE)
      
      data = self$task$data()
      training_x1 = data[[self$task$feature_names[1]]]
      training_x2 = data[[self$task$feature_names[2]]]
      training_y = data[[self$task$target_names]]

      # Convert factors to numeric for visualization
      if (self$learner$predict_type == "prob" && is.factor(training_y)) {
        training_y = as.integer(training_y) - 1
      } else if (self$learner$predict_type == "response" && is.factor(training_y)) {
        training_y = as.integer(training_y) - 1
      }
      
      # Store training data specification without resolving colors yet
      private$store_layer("training_data", list(
        data = list(x1 = training_x1, x2 = training_x2, y = training_y),
        style = list(
          color = color,  # Keep as "auto" for later resolution
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
    #' Adds boundary line(s) to the plot at specified values.
    #'
    #' @param values (`numeric()`)\cr
    #'   Vector of values where to draw boundary contours. For classification with probability predictions,
    #'   defaults to 0.5. For regression or response predictions, defaults to the median of predictions.
    #' @param color (`character(1)`)\cr
    #'   Color of the boundary lines. Default is "black".
    #' @param line_width (`numeric(1)`)\cr
    #'   Width of boundary lines. Default is 1.5.
    #' @param line_type (`character(1)`)\cr
    #'   Type of boundary lines. One of "solid", "dashed", "dotted". Default is "solid".
    #' @param alpha (`numeric(1)`)\cr
    #'   Transparency of boundary lines. Default is 0.8.
    add_boundary = function(values = NULL, color = "black", line_width = 1.5, line_type = "solid", alpha = 0.8) {
      checkmate::assert_numeric(values, null.ok = TRUE)
      checkmate::assert_string(color)
      checkmate::assert_number(line_width, lower = 0)
      checkmate::assert_choice(line_type, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      
      # Determine default values based on prediction type
      if (is.null(values)) {
        if (self$learner$predict_type == "prob") {
          values = 0.5
        } else {
          # For regression or response predictions, use median (single central tendency)
          values = median(self$fun_y, na.rm = TRUE)
        }
      } else {
        # Validate that boundary values are within the prediction range
        y_range = range(self$fun_y, na.rm = TRUE)
        invalid_values = values[values < y_range[1] | values > y_range[2]]
        if (length(invalid_values) > 0) {
          warning(sprintf(
            "Boundary values %s are outside the prediction range [%.3f, %.3f] and will not generate visible contours.",
            paste(round(invalid_values, 3), collapse = ", "),
            y_range[1], y_range[2]
          ))
        }
      }
      
      # Store boundary specification without resolving colors yet
      private$store_layer("boundary", list(
        values = values,
        color = color,  # Keep for later resolution if "auto"
        line_width = line_width,
        line_type = line_type,
        alpha = alpha
      ))

      return(invisible(self))
    },

    #' @description
    #' Create and return the ggplot2 plot with model-specific features.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param title_size (`numeric(1)`)\cr
    #'   Title text size. If NULL, defaults to text_size + 2.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param background (`character(1)`)\cr
    #'   Background color for the plot. Default is "white".
    #' @param color_palette (`character(1)`)\cr
    #'   Color palette for the fill scale. One of "viridis", "plasma", "grayscale". Default is "viridis".
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(text_size = 11, title_size = NULL, theme = "minimal", background = "white", color_palette = "viridis", ...) {
      # Call parent first to set up plot_settings and base plot
      p = super$plot(text_size = text_size, title_size = title_size, theme = theme, 
                      background = background, color_palette = color_palette, ...)
      
      # Render class-specific layers
      p = private$render_boundary_layers(p, color_palette)
      p = private$render_training_data_layers(p, color_palette, text_size)
      
      return(p)
    }
  ),
  private = list(
    # Render stored boundary layers
    render_boundary_layers = function(plot_obj, color_palette) {
      boundary_layers = private$get_layers_by_type("boundary")
      
      if (length(boundary_layers) == 0) {
        return(plot_obj)
      }
      
      for (boundary_spec in boundary_layers) {
        plot_obj = private$render_boundary_layer(plot_obj, boundary_spec)
      }
      
      return(plot_obj)
    },
    
    # Render a single boundary layer
    render_boundary_layer = function(plot_obj, layer_spec) {
      data = data.table(
        fun_x1 = self$fun_x1,
        fun_x2 = self$fun_x2,
        fun_y = self$fun_y
      )
      
      # Map line_type to ggplot2 linetype
      gg_linetype = switch(layer_spec$line_type,
        "solid" = "solid",
        "dashed" = "dashed", 
        "dotted" = "dotted",
        "dotdash" = "dotdash",
        "longdash" = "longdash",
        "twodash" = "twodash",
        "solid"  # default fallback
      )
      
      plot_obj = plot_obj + ggplot2::geom_contour(
        data = data,
        aes(x = fun_x1, y = fun_x2, z = fun_y),
        breaks = layer_spec$values,
        color = layer_spec$color, 
        linewidth = layer_spec$line_width,
        linetype = gg_linetype,
        alpha = layer_spec$alpha,
        inherit.aes = FALSE
      )
      
      return(plot_obj)
    },
    
    # Render stored training data layers
    render_training_data_layers = function(plot_obj, color_palette, text_size) {
      training_layers = private$get_layers_by_type("training_data")
      
      if (length(training_layers) == 0) {
        return(plot_obj)
      }
      
      for (training_spec in training_layers) {
        plot_obj = private$render_training_data_layer(plot_obj, training_spec, color_palette, text_size)
      }
      
      return(plot_obj)
    },
    
    # Render a single training data layer
    render_training_data_layer = function(plot_obj, layer_spec, color_palette, text_size) {
      points_data = data.table(
        points_x1 = layer_spec$data$x1,
        points_x2 = layer_spec$data$x2,
        points_y = layer_spec$data$y
      )
      
      style = layer_spec$style
      
      # Add styled training data points with color mapping
      if (self$learner$predict_type == "prob" || is.numeric(layer_spec$data$y)) {
        # Use color aesthetic for continuous/probability data
        color_limits = c(min(self$fun_y), max(self$fun_y))
        
        plot_obj = plot_obj + geom_point(
          data = points_data,
          aes(x = points_x1, y = points_x2, color = points_y),
          size = style$size,
          shape = style$shape,
          alpha = style$alpha,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
        
        # Apply matching color scale for points based on the selected palette
        if (color_palette == "viridis") {
          plot_obj = plot_obj + scale_color_viridis_c(name = self$lab_y, limits = color_limits)
        } else if (color_palette == "plasma") {
          plot_obj = plot_obj + scale_color_viridis_c(name = self$lab_y, option = "plasma", limits = color_limits)
        } else if (color_palette == "grayscale") {
          plot_obj = plot_obj + scale_color_gradient(name = self$lab_y, low = "black", high = "white", limits = color_limits)
        }
      } else {
        # Use fixed color for discrete/categorical data
        plot_obj = plot_obj + geom_point(
          data = points_data,
          aes(x = points_x1, y = points_x2),
          color = style$color,
          size = style$size,
          shape = style$shape,
          alpha = style$alpha,
          inherit.aes = FALSE
        )
      }
      
      # Add labels if requested
      if (style$show_labels) {
        label_size = if (!is.null(style$label_size)) style$label_size else text_size * 0.8 / ggplot2::.pt
        
        plot_obj = plot_obj + geom_text(
          data = points_data,
          aes(x = points_x1, y = points_x2, label = seq_len(nrow(points_data))),
          color = if (self$learner$predict_type == "prob" || is.numeric(layer_spec$data$y)) "black" else style$color,
          size = label_size,
          vjust = -0.5,
          inherit.aes = FALSE
        )
      }
      
      return(plot_obj)
    }
  )
)
