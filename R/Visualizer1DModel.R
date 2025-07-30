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
    #' Add training data points to the plot.
    #' @return Returns the visualizer object invisibly for method chaining.
    add_training_data = function() {
      data <- self$task$data()
      points_data <- data.frame(
        x = data[[self$lab_x]],
        y = data[[self$task$target_names]]
      )
      self$add_points(points_data)
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
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(text_size = 11, theme = "minimal", ...) {
      # Call parent first to set up plot_settings and base plot
      p <- super$plot(text_size = text_size, theme = theme, ...)
      
      # Render class-specific layers
      p <- private$render_boundary_layers(p)
      
      return(p)
    }
  ),
  private = list(
    # Render stored boundary layers
    render_boundary_layers = function(plot_obj) {
      boundary_layer <- private$get_layer("boundary")
      if (!is.null(boundary_layer)) {
        for (value in boundary_layer$values) {
          plot_obj <- plot_obj + ggplot2::geom_hline(
            yintercept = value,
            color = boundary_layer$color,
            linetype = boundary_layer$linetype,
            linewidth = boundary_layer$linewidth,
            alpha = boundary_layer$alpha
          )
        }
      }
      return(plot_obj)
    }
  )
)
