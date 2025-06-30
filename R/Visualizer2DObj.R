#' @title Visualize Objective
#'
#' @description
#' This class is used to create visualizations and animations of optimization traces.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer2DObj = R6::R6Class("Visualizer2DObj",
  inherit = Visualizer2D,
  public = list(

    #' @template field_objective
    objective = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_objective
    initialize = function(
      objective,
      x1_limits = NULL,
      x2_limits = NULL,
      padding = 0,
      n_points = 100L
      ) {
      self$objective = checkmate::assert_r6(objective, "Objective")
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(padding)
      checkmate::assert_count(n_points)

      if (objective$xdim != 2) {
        mlr3misc::stopf("`Visualizer2D` requires 2-dimensional inputs, but `objective$xdim = %s`", objective$xdim)
      }

      x1_limits = x1_limits %??% c(objective$lower[1], objective$upper[1])
      x2_limits = x2_limits %??% c(objective$lower[2], objective$upper[2])

      if (any(is.na(x1_limits)) || any(is.na(x2_limits))) {
        stop("Limits could not be extracted from the objective. Please use `x_limits`.")
      }

      x1_pad = (x1_limits[2] - x1_limits[1]) * padding
      x2_pad = (x2_limits[2] - x2_limits[1]) * padding

      x1 = unique(seq(x1_limits[1] - x1_pad, x1_limits[2] + x1_pad, length.out = n_points))
      x2 = unique(seq(x2_limits[1] - x2_pad, x2_limits[2] + x2_pad, length.out = n_points))
      grid = CJ(x1, x2)

      y = apply(grid, 1, function(row) {
         self$objective$eval(c(row[1], row[2]))
      })

      super$initialize(
        fun_x1 = grid[, "x1", with = FALSE][[1]],
        fun_x2 = grid[, "x2", with = FALSE][[1]],
        fun_y = y,
        title = self$objective$label,
        lab_x1 = "x1",
        lab_x2 = "x2",
        lab_y = "y"
      )
    }
  )
)
