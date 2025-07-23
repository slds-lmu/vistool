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
Visualizer2DModel <- R6::R6Class("Visualizer2DModel",
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
      self$task <- mlr3::assert_task(task)
      self$learner <- mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_count(n_points)
      lab_x1 <- self$task$feature_names[1]
      lab_x2 <- self$task$feature_names[2]
      data <- task$data()
      self$learner$train(task)

      x1_limits <- range(data[, lab_x1, with = FALSE])
      x2_limits <- range(data[, lab_x2, with = FALSE])

      x1 <- seq(x1_limits[1] - padding, x1_limits[2] + padding, length.out = n_points)
      x2 <- seq(x2_limits[1] - padding, x2_limits[2] + padding, length.out = n_points)

      newdata <- CJ(x1, x2)
      setnames(newdata, self$task$feature_names)

      original_types <- sapply(self$task$data()[, self$task$feature_names, with = FALSE], class)
      for (col in names(original_types)) {
        if (original_types[col] == "integer") {
          newdata[[col]] <- as.integer(round(newdata[[col]]))
        }
      }
      y <- self$learner$predict_newdata(newdata)[[self$learner$predict_type]]
      if (self$learner$predict_type == "prob") {
        # for binary classification, use the positive class
        # for multi-class, use the first class
        if (length(task$class_names) == 2 && !is.null(task$positive)) {
          y <- y[, task$positive]
        } else {
          y <- y[, 1]
        }
      } else if (self$learner$predict_type == "response" && is.factor(y)) {
        # Convert factor response to numeric for visualization
        y <- as.numeric(y) - 1 # Convert to 0-based indexing
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
    #' @param size (`numeric(1)`)\cr
    #'  Size of the points.
    #' @param color (`character(1)`)\cr
    #' Color of the points.
    add_training_data = function(size = 2, color = NULL) {
      data <- self$task$data()
      self$points_x1 <- data[[self$task$feature_names[1]]]
      self$points_x2 <- data[[self$task$feature_names[2]]]
      self$points_y <- data[[self$task$target_names]]

      # Convert factors to numeric for visualization
      if (self$learner$predict_type == "prob" && is.factor(self$points_y)) {
        self$points_y <- as.integer(self$points_y) - 1
      } else if (self$learner$predict_type == "response" && is.factor(self$points_y)) {
        self$points_y <- as.integer(self$points_y) - 1
      }

      return(invisible(self))
    },

    #' @description
    #' Adds boundary line(s) to the plot at specified values.
    #'
    #' @param values (`numeric()`)\cr
    #'   Vector of values where to draw boundary contours. For classification with probability predictions,
    #'   defaults to 0.5. For regression or response predictions, defaults to quantiles of predictions.
    #' @param color (`character(1)`)\cr
    #'   Color of the boundary lines. Default is "black".
    #' @param linewidth (`numeric(1)`)\cr
    #'   Width of boundary lines. Default is 1.5.
    #' @param alpha (`numeric(1)`)\cr
    #'   Transparency of boundary lines. Default is 0.8.
    add_boundary = function(values = NULL, color = "black", linewidth = 1.5, alpha = 0.8) {
      checkmate::assert_numeric(values, null.ok = TRUE)
      checkmate::assert_string(color)
      checkmate::assert_number(linewidth, lower = 0)
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      
      # Determine default values based on prediction type
      if (is.null(values)) {
        if (self$learner$predict_type == "prob") {
          values <- 0.5
        } else {
          # For regression or response predictions, use quantiles
          values <- quantile(self$fun_y, c(0.25, 0.5, 0.75), na.rm = TRUE)
        }
      }
      
      # Store boundary information for plotting
      private$.boundary_values <- values
      private$.boundary_color <- color
      private$.boundary_linewidth <- linewidth
      private$.boundary_alpha <- alpha

      return(invisible(self))
    },

    #' @description
    #' Create and return the ggplot2 plot with model-specific features.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @return A ggplot2 object.
    plot = function(text_size = 11, theme = "minimal") {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      
      # Call parent plot method
      p <- super$plot(text_size = text_size, theme = theme)
      
      # Add boundary lines if available
      if (!is.null(private$.boundary_values)) {
        data <- data.table(
          fun_x1 = self$fun_x1,
          fun_x2 = self$fun_x2,
          fun_y = self$fun_y
        )
        
        p <- p + ggplot2::geom_contour(
          data = data,
          aes(x = fun_x1, y = fun_x2, z = fun_y),
          breaks = private$.boundary_values,
          color = private$.boundary_color, 
          linewidth = private$.boundary_linewidth, 
          alpha = private$.boundary_alpha,
          inherit.aes = FALSE
        )
      }
      
      return(p)
    }
  ),
  private = list(
    .boundary_values = NULL,
    .boundary_color = NULL,
    .boundary_linewidth = NULL,
    .boundary_alpha = NULL
  )
)
