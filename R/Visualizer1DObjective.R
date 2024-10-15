#' @title Visualize Objective
#'
#' @description
#' This class is used to create visualizations of optimization traces.
#'
#' @template param_x1_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer1DObjective = R6::R6Class("Visualizer1DObjective",
  inherit = Visualizer1D,
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
    initialize = function(objective, x1_limits = NULL, padding = 0, n_points = 100L) {
      self$objective = assert_r6(objective, "Objective")
      assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      assert_numeric(padding)
      assert_count(n_points)

      if (objective$xdim != 1) {
        stopf("`Visualizer1D` requires 1-dimensional inputs, but `objective$xdim = %s`", objective$xdim)
      }

      x1_limits = x1_limits %??% c(objective$limits_lower, objective$limits_upper)

      if (any(is.na(x1_limits))) {
        stop("Limits could not be extracted from the objective. Please use `x1_limits`.")
      }

      x_pad = (x1_limits[2] - x1_limits[1]) * padding
      x = seq(x1_limits[1] - x_pad, x1_limits[2] + x_pad, length.out = n_points)
      y = map_dbl(x, function(x) objective$eval(x))

      super$initialize(
        x = x,
        y = y,
        plot_lab = self$objective$label,
        x_lab = "x",
        y_lab = "y"
      )

      return(invisible(self))
    },

    #' @description
    #' Add optimization trace to the plot.
    #'
    #' @param optimizer (`Optimizer`)\cr
    #'  The optimizer to add to the plot.
    add_optimization_trace = function(optimizer) {
      assert_r6(optimizer, "Optimizer")

      archive = optimizer$archive

      private$.plot = private$.plot %>%
        add_trace(
          x = archive$x_in,
          y = archive$fval_in,
          color = archive$step,
          type = "scatter",
          mode = "markers"
        )
    }
  )
)
