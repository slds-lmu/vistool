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
    #' @param training_points (`logical(1)`)\cr
    #'   Whether to show training points on the plot.
    initialize = function(task,
                          learner,
                          x1_limits = NULL,
                          x2_limits = NULL,
                          padding = 0,
                          n_points = 100L,
                          training_points = FALSE) {
      self$task <- mlr3::assert_task(task)
      self$learner <- mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_count(n_points)
      checkmate::assert_flag(training_points)
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

      if (training_points) {
        data <- task$data()
        self$points_x1 <- data[[lab_x1]]
        self$points_x2 <- data[[lab_x2]]
        self$points_y <- data[[task$target_names]]
      }

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
    #' Adds the decision boundary to the plot.
    #'
    #' @param threshold (`numeric(1)`)\cr
    #'  Threshold for the decision boundary.
    add_decision_boundary = function(threshold = 0.5) {
      if (self$learner$predict_type != "prob") {
        stop("Decision boundary can only be added for probability predictions")
      }

      # Store the threshold for use in plotting
      private$.decision_threshold <- threshold

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
      
      # Add decision boundary if available (for classification)
      if (!is.null(private$.decision_threshold)) {
        data <- data.table(
          fun_x1 = self$fun_x1,
          fun_x2 = self$fun_x2,
          fun_y = self$fun_y
        )
        
        p <- p + geom_contour(
          data = data,
          aes(x = fun_x1, y = fun_x2, z = fun_y),
          breaks = private$.decision_threshold,
          color = "black", 
          linewidth = 1.5, 
          alpha = 0.8,
          inherit.aes = FALSE
        )
      }
      
      return(p)
    }
  ),
  private = list(
    .decision_threshold = NULL
  )
)
