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

    n_points = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task])\cr
    #'   The task to train the model on.
    #' @param learner ([mlr3::Learner])\cr
    #'   The learner to train the model with.
    initialize = function(
      task,
      learner,
      xlim = NULL,
      n_points = 100,
      training_points = FALSE
      ) {
      # FIXME: doc complete class, not all args are doced here
      self$task = assert_task(task)
      fnames = task$feature_names
      if (length(fnames) != 1)
        stop("Task must have exactly 1 feature")
      self$learner = assert_learner(learner, task = self$task)
      assert_count(n_points)
      assert_flag(training_points)

      # train learner on task
      self$learner$train(task)

      x_train = task$data()[[fnames[1]]]
      y_train = task$truth()

      if (is.null(xlim))
        xlim = range(x_train)
      x_pred = seq(xlim[1], xlim[2], length.out = n_points)
      newdata = set_names(as.data.table(x_pred), self$task$feature_names)
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