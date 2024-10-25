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

    #' @field objective (`Objective`)\cr
    #' The objective which was optimized.
    #' This object is used to generate the surface/contour lines.
    objective = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param objective (`Objective`)\cr
    #'   The objective which was optimized.
    #'   This object is used to generate the surface/contour lines.
    initialize = function(
      objective,
      x1_limits = NULL,
      x2_limits = NULL,
      padding = 0,
      n_points = 100L
      ) {
      self$objective = assert_r6(objective, "Objective")
      assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      assert_numeric(padding)
      assert_count(n_points)

      if (objective$xdim != 2) {
        stopf("`Visualizer2D` requires 2-dimensional inputs, but `objective$xdim = %s`", objective$xdim)
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

      y = pmap_dbl(grid, function(x1, x2) {
         self$objective$eval(c(x1, x2))
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
