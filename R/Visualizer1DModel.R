
#' @title Visualize Model
#'
#' @description
#' This class is used to create visualizations of tasks and learners.
#'
#' @template param_x_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer1DModel = R6::R6Class("Visualizer1DModel",
  inherit = Visualizer1D,
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
    #' @param learner ([mlr3::Learner])\cr
    #'   The learner to train the model with.
    #' @param x_limits (`numeric(2)`)\cr
    #'  The limits of the x axis.
    initialize = function(task, learner, x_limits = NULL, padding = 0, n_points = 100L) {
      self$task = assert_task(task)
      self$learner = assert_learner(learner, task = self$task)
      assert_numeric(x_limits, len = 2, null.ok = TRUE)
      assert_count(n_points)
      x_lab = self$task$feature_names[1]
      data = task$data()
      self$learner$train(task)

      xlimits = range(data[, x_lab, with = FALSE])
      x = seq(xlimits[1] - padding, xlimits[2] + padding, length.out = n_points)

      newdata = set_names(as.data.table(x), self$task$feature_names)
      y = self$learner$predict_newdata(newdata)$response

      super$initialize(
        x = x,
        y = y,
        plot_lab = sprintf("%s on %s", self$learner$id, self$task$id),
        x_lab = x_lab,
        y_lab = task$target_names
      )

      return(invisible(self))
    }
  )
)
