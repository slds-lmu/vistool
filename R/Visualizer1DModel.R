# FIXME: check that this works with all classif output types

#' @title Visualize Model
#'
#' @description
#' This class is used to create visualizations of tasks and learners.
#'
#' @template param_x1_limits
#' @template param_n_points
#'
#' @export
Visualizer1DModel <- R6::R6Class("Visualizer1DModel",
  inherit = Visualizer1D,
  public = list(

    #' @field task (`mlr3::Task`)\cr
    #' Task used to train the model.
    task = NULL,

    #' @field learner (`mlr3::Learner`)\cr
    #' Learner used to train the model.
    learner = NULL,

    #' @field n_points (`integer(1)`)\cr
    #' Number of points to use for visualization.
    n_points = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task])\cr
    #'   The task to train the model on.
    #' @template param_learner
    #' @param xlim (`numeric(2)`)\cr
    #'   Limits for the x-axis. If NULL, will be determined from task data.
    #' @template param_n_points
    initialize = function(task,
                          learner,
                          xlim = NULL,
                          n_points = 100) {
      # FIXME: doc complete class, not all args are doced here
      self$task <- mlr3::assert_task(task)
      fnames <- task$feature_names
      if (length(fnames) != 1) {
        stop("Task must have exactly 1 feature")
      }
      self$learner <- mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_count(n_points)

      # train learner on task
      self$learner$train(task)

      x_train <- task$data()[[fnames[1]]]
      y_train <- task$truth()

      if (is.null(xlim)) {
        xlim <- range(x_train)
      }
      x_pred <- seq(xlim[1], xlim[2], length.out = n_points)
      newdata <- as.data.table(x_pred)
      setnames(newdata, self$task$feature_names)

      original_types <- sapply(self$task$data()[, self$task$feature_names, with = FALSE], class)
      for (col in names(original_types)) {
        if (original_types[col] == "integer") {
          newdata[[col]] <- as.integer(round(newdata[[col]]))
        }
      }
      y_pred <- self$learner$predict_newdata(newdata)
      if (self$learner$predict_type == "prob") {
        # For binary classification, use the positive class probability
        # For multi-class, use the first class probability  
        if (length(self$task$class_names) == 2 && !is.null(self$task$positive)) {
          y_pred <- y_pred$prob[, self$task$positive]
        } else if (self$learner$task_type == "classif") {
          y_pred <- y_pred$prob[, 1]
        } else {
          y_pred <- y_pred$prob
        }
      } else {
        y_pred <- y_pred$response
        # Convert factor response to numeric for visualization
        if (is.factor(y_pred)) {
          y_pred <- as.numeric(y_pred) - 1 # Convert to 0-based indexing
        }
      }
      title <- sprintf("%s on %s", self$learner$id, self$task$id)


      super$initialize(
        fun_x = x_pred,
        fun_y = y_pred,
        title = title,
        lab_x = fnames[1],
        lab_y = task$target_names
      )
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
      
      data <- self$task$data()
      training_x <- data[[self$task$feature_names[1]]]
      training_y <- data[[self$task$target_names]]

      # Convert factors to numeric for visualization
      if (self$learner$predict_type == "prob" && is.factor(training_y)) {
        training_y <- as.integer(training_y) - 1
      } else if (self$learner$predict_type == "response" && is.factor(training_y)) {
        training_y <- as.integer(training_y) - 1
      }
      
      # Store training data specification without resolving colors yet
      private$store_layer("training_data", list(
        data = list(x = training_x, y = training_y),
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
    #' Adds boundary line(s) to the plot at specified y-values.
    #'
    #' @param values (`numeric()`)\cr
    #'   Vector of y-values where to draw boundary lines. For classification with probability predictions,
    #'   defaults to 0.5. For regression or response predictions, defaults to the median of predictions.
    #' @param color (`character(1)`)\cr
    #'   Color of the boundary lines. Default is "black".
    #' @param linetype (`character(1)`)\cr
    #'   Line type for boundary lines. Default is "dashed".
    #' @param linewidth (`numeric(1)`)\cr
    #'   Width of boundary lines. Default is 1.
    #' @param alpha (`numeric(1)`)\cr
    #'   Transparency of boundary lines. Default is 0.8.
    add_boundary = function(values = NULL, color = "black", linetype = "dashed", linewidth = 1, alpha = 0.8) {
      checkmate::assert_numeric(values, null.ok = TRUE)
      checkmate::assert_string(color)
      checkmate::assert_string(linetype)
      checkmate::assert_number(linewidth, lower = 0)
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      
      # Determine default values based on prediction type
      if (is.null(values)) {
        if (self$learner$predict_type == "prob") {
          values <- 0.5
        } else {
          # For regression or response predictions, use median of predictions
          values <- median(self$fun_y, na.rm = TRUE)
        }
      } else {
        # Validate that boundary values are within the prediction range
        y_range <- range(self$fun_y, na.rm = TRUE)
        invalid_values <- values[values < y_range[1] | values > y_range[2]]
        if (length(invalid_values) > 0) {
          warning(sprintf(
            "Boundary values %s are outside the prediction range [%.3f, %.3f] and will not be visible.",
            paste(round(invalid_values, 3), collapse = ", "),
            y_range[1], y_range[2]
          ))
        }
      }
      
      # Store boundary specification without resolving colors yet
      private$store_layer("boundary", list(
        values = values,
        color = color,  # Keep for later resolution
        linetype = linetype,
        linewidth = linewidth,
        alpha = alpha
      ))

      return(invisible(self))
    },

    #' @description
    #' Create and return the ggplot2 plot with model-specific features.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param color_palette (`character(1)`)\cr
    #'   Color palette for the fill scale. One of "viridis", "plasma", "grayscale". Default is "viridis".
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(text_size = 11, theme = "minimal", color_palette = "viridis", ...) {
      # Call parent first to set up plot_settings and base plot
      p <- super$plot(text_size = text_size, theme = theme, color_palette = color_palette, ...)
      
      # Render class-specific layers
      p <- private$render_boundary_layers(p)
      p <- private$render_training_data_layers(p, color_palette, text_size)
      
      return(p)
    }
  ),
  private = list(
    # Render stored boundary layers
    render_boundary_layers = function(plot_obj) {
      boundary_layers <- private$get_layers_by_type("boundary")
      
      if (length(boundary_layers) == 0) {
        return(plot_obj)
      }
      
      for (boundary_spec in boundary_layers) {
        plot_obj <- private$render_boundary_layer(plot_obj, boundary_spec)
      }
      
      return(plot_obj)
    },
    
    # Render a single boundary layer
    render_boundary_layer = function(plot_obj, layer_spec) {
      for (value in layer_spec$values) {
        plot_obj <- plot_obj + ggplot2::geom_hline(
          yintercept = value,
          color = layer_spec$color,
          linetype = layer_spec$linetype,
          linewidth = layer_spec$linewidth,
          alpha = layer_spec$alpha
        )
      }
      return(plot_obj)
    },
    
    # Render stored training data layers
    render_training_data_layers = function(plot_obj, color_palette, text_size) {
      training_layers <- private$get_layers_by_type("training_data")
      
      if (length(training_layers) == 0) {
        return(plot_obj)
      }
      
      for (training_spec in training_layers) {
        plot_obj <- private$render_training_data_layer(plot_obj, training_spec, color_palette, text_size)
      }
      
      return(plot_obj)
    },
    
    # Render a single training data layer
    render_training_data_layer = function(plot_obj, layer_spec, color_palette, text_size) {
      points_data <- data.frame(
        points_x = layer_spec$data$x,
        points_y = layer_spec$data$y
      )
      
      style <- layer_spec$style
      
      # Resolve auto colors if needed
      resolved_color <- if (style$color == "auto") {
        private$get_auto_color_with_palette()
      } else {
        style$color
      }
      
      # Add styled training data points
      plot_obj <- plot_obj + ggplot2::geom_point(
        data = points_data,
        aes(x = points_x, y = points_y),
        color = resolved_color,
        size = style$size,
        shape = style$shape,
        alpha = style$alpha,
        inherit.aes = FALSE
      )
      
      # Add labels if requested
      if (style$show_labels) {
        label_size <- if (!is.null(style$label_size)) style$label_size else text_size * 0.8 / ggplot2::.pt
        
        plot_obj <- plot_obj + ggplot2::geom_text(
          data = points_data,
          aes(x = points_x, y = points_y, label = seq_len(nrow(points_data))),
          color = resolved_color,
          size = label_size,
          vjust = -0.5,
          inherit.aes = FALSE
        )
      }
      
      return(plot_obj)
    }
  )
)
