
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
    #' @param learner ([mlr3::Learner])\cr
    #'   The learner to train the model with.
    initialize = function(
      task,
      learner,
      x1_limits = NULL,
      x2_limits = NULL,
      padding = 0,
      n_points = 100L
      training_data = FALSE
      ) {

      self$task = assert_task(task)
      self$learner = assert_learner(learner, task = self$task)
      assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      assert_count(n_points)
      lab_x1 = self$task$feature_names[1]
      lab_x2 = self$task$feature_names[2]
      data = task$data()
      self$learner$train(task)

      x1_limits = range(data[, lab_x1, with = FALSE])
      x2_limits = range(data[, lab_x2, with = FALSE])

      x1 = seq(x1_limits[1] - padding, x1_limits[2] + padding, length.out = n_points)
      x2 = seq(x2_limits[1] - padding, x2_limits[2] + padding, length.out = n_points)

      newdata = set_names(CJ(x1, x2), self$task$feature_names)
      y = self$learner$predict_newdata(newdata)[[self$learner$predict_type]]
      if (self$learner$predict_type == "prob") y = y[, task$positive]

      super$initialize(
        fun_x1 = newdata[, lab_x1, with = FALSE][[1]],
        fun_x2 = newdata[, lab_x2, with = FALSE][[1]],
        fun_y = y,
        title = sprintf("%s on %s", self$learner$id, self$task$id),
        lab_x1 = lab_x1,
        lab_x2 = lab_x2,
        lab_y = task$target_names
      )

      if (training_data) {

      }
    },

    plot = function() {
      if (training_data) {
        data = task$data()
        self$points_x1 = data[[lab_x1]]
        self$points_x2 = data[[lab_x2]]
        self$points_y = data[[task$target_names]]
      }


      if (training_data) {
        data = task$data()
        # if (self$learner$predict_type == "prob") points_y = as.integer(points_y) - 1
        browser()
        super$plot() +
          geom_point(aes(
            x = .data[[self$task$feature_names[1]]],
            y = .data[[self$task$feature_names[2]]],
            color = .data[[self$task$target_names]]),
            data = data, size = 5, inherit.aes = FALSE) +
          scale_color_viridis_c()
      } else {
        super$plot()
      }
    }
  )
)
