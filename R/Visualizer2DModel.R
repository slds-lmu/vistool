
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
    initialize = function(task, learner, x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L) {
      self$task = assert_task(task)
      self$learner = assert_learner(learner, task = self$task)
      assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      assert_count(n_points)
      x1 = self$task$feature_names[1]
      x2 = self$task$feature_names[2]
      data = task$data()
      self$learner$train(task)

      x1_limits = range(data[, x1, with = FALSE])
      x2_limits = range(data[, x2, with = FALSE])

      grid = list(
        x1 = seq(x1_limits[1] - padding, x1_limits[2] + padding, length.out = n_points),
        x2 = seq(x2_limits[1] - padding, x2_limits[2] + padding, length.out = n_points)
      )

      newdata = set_names(CJ(grid$x1, grid$x2), self$task$feature_names)
      z = self$learner$predict_newdata(newdata)[[self$learner$predict_type]]
      if (self$learner$predict_type == "prob") z = z[, "pos"]
      zmat = matrix(z, nrow = n_points, ncol = n_points, byrow = TRUE)

      super$initialize(
        grid = grid,
        zmat = zmat,
        plot_lab = sprintf("%s on %s", self$learner$id, self$task$id),
        x1_lab = x1,
        x2_lab = x2,
        z_lab = task$target_names
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
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    add_training_data = function(size = 5, color = "grey",...) {
      if (is.null(private$.plot)) self$init_layer_surface()
      data = self$task$data()
      x1 = data[, self$task$feature_names[1], with = FALSE][[1]]
      x2 = data[, self$task$feature_names[2], with = FALSE][[1]]
      z = data[, self$task$target_names, with = FALSE][[1]]
      if (self$learner$predict_type == "prob") z = as.integer(z) - 1

      private$.plot = private$.plot %>%
        add_trace(
          x = x1,
          y = x2,
          z = z,
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 5, color = grey),
          ...
        )

      return(invisible(self))
    },

    #' @description
    #' Adds the decision boundary to the plot.
    #'
    #' @param threshold (`numeric(1)`)\cr
    #'  Threshold for the decision boundary.
    #' @param surfacecolor (`list()`)\cr
    #'  The coloring of the surface.
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    add_decision_boundary = function(threshold = 0.5, surfacecolor = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")), ...) {
       if (is.null(private$.plot)) self$init_layer_surface()
      z = matrix(threshold, nrow = nrow(self$zmat), ncol = ncol(self$zmat), byrow = TRUE)

      private$.plot = private$.plot %>%
        add_surface(
          x = self$grid$x1,
          y = self$grid$x2,
          z = z,
          colorscale  = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")),
          showscale = FALSE,
          ...)
    }
  )
)
