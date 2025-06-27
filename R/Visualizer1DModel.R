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
Visualizer1DModel = R6::R6Class("Visualizer1DModel", inherit = Visualizer1D,
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
    #' @param learner ([mlr3::Learner])\cr
    #'   The learner to train the model with.
    #' @param xlim (`numeric(2)`)\cr
    #'   Limits for the x-axis. If NULL, will be determined from task data.
    #' @param n_points (`integer(1)`)\cr
    #'   Number of points to use for visualization.
    #' @param training_points (`logical(1)`)\cr
    #'   Whether to show training points on the plot.
    initialize = function(
      task,
      learner,
      xlim = NULL,
      n_points = 100,
      training_points = FALSE
      ) {
      # FIXME: doc complete class, not all args are doced here
      self$task = mlr3::assert_task(task)
      fnames = task$feature_names
      if (length(fnames) != 1)
        stop("Task must have exactly 1 feature")
      self$learner = mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_count(n_points)
      checkmate::assert_flag(training_points)

      # train learner on task
      self$learner$train(task)

      x_train = task$data()[[fnames[1]]]
      y_train = task$truth()

      if (is.null(xlim))
        xlim = range(x_train)
      x_pred = seq(xlim[1], xlim[2], length.out = n_points)
      newdata = as.data.table(x_pred)
      setnames(newdata, self$task$feature_names)

      original_types = sapply(self$task$data()[, self$task$feature_names, with = FALSE], class)
      for (col in names(original_types)) {
        if (original_types[col] == "integer") {
          newdata[[col]] = as.integer(round(newdata[[col]]))
        }
      }
      y_pred = self$learner$predict_newdata(newdata)$response
      title = sprintf("%s on %s", self$learner$id, self$task$id)


      super$initialize(
        fun_x = x_pred,
        fun_y = y_pred,
        title = title,
        lab_x = fnames[1],
        lab_y = task$target_names)

      if (training_points) {
        data = task$data()
        self$points_x = data[[self$lab_x]]
        self$points_y = data[[task$target_names]]
      }
    }
  )
)
