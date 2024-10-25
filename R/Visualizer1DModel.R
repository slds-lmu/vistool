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

    # FIXME: add that we can plot data
    plot_data = NULL,

    n_points = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task])\cr
    #'   The task to train the model on.
    #' @param learner ([mlr3::Learner])\cr
    #'   The learner to train the model with.
    initialize = function(task, learner, plot_data = FALSE, xlim = NULL, n_points = 100) {
      # FIXME: doc complete class, not all args are doced here
      self$task = assert_task(task)
      fnames = task$feature_names
      if (length(fnames) != 1)
        stop("Task must have exactly 1 feature")
      self$learner = assert_learner(learner, task = self$task)
      assert_flag(plot_data)
      assert_count(n_points)

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

      points_x = NULL; points_y = NULL
      if (plot_data) {
        points_x = x_train
        points_y = y_train
      }

      super$initialize(fun_x = x_pred, fun_y = y_pred,
        title = title, lab_x = fnames[1], lab_y = task$target_names,
        points_x = points_x, points_y = points_y)
    }
  )
)
