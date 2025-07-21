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
    #' @param training_points (`logical(1)`)\cr
    #'   Whether to show training points on the plot.
    initialize = function(task,
                          learner,
                          xlim = NULL,
                          n_points = 100,
                          training_points = FALSE) {
      # FIXME: doc complete class, not all args are doced here
      self$task <- mlr3::assert_task(task)
      fnames <- task$feature_names
      if (length(fnames) != 1) {
        stop("Task must have exactly 1 feature")
      }
      self$learner <- mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_count(n_points)
      checkmate::assert_flag(training_points)

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

      if (training_points) {
        data <- task$data()
        self$points_x <- data[[self$lab_x]]
        self$points_y <- data[[task$target_names]]
      }
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
      }
      
      # Store boundary information for plotting
      private$.boundary_values <- values
      private$.boundary_color <- color
      private$.boundary_linetype <- linetype
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
        for (value in private$.boundary_values) {
          p <- p + ggplot2::geom_hline(
            yintercept = value,
            color = private$.boundary_color,
            linetype = private$.boundary_linetype,
            linewidth = private$.boundary_linewidth,
            alpha = private$.boundary_alpha
          )
        }
      }
      
      return(p)
    }
  ),
  private = list(
    .boundary_values = NULL,
    .boundary_color = NULL,
    .boundary_linetype = NULL,
    .boundary_linewidth = NULL,
    .boundary_alpha = NULL
  )
)
