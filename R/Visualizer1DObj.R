#FIXME: run rcmd check
# FIXME: think about plotting extra points
# FIXME: can we add optimizer traces? maybe use bbotk?

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
Visualizer1DObj = R6::R6Class("Visualizer1DObj", inherit = Visualizer1D,
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
    initialize = function(objective, xlim = NULL, n_points = 100L) {
      self$objective = checkmate::assert_r6(objective, "Objective")
      if (objective$xdim != 1) {
        mlr3misc::stopf("`Visualizer1D` requires 1-dimensional inputs, but `objective$xdim = %s`", objective$xdim)
      }
      xlim = xlim %??% c(objective$lower, objective$upper)
      if (any(is.na(xlim)))
        stop("Limits could not be extracted from the objective. Please use `xlim`.")
      checkmate::assert_numeric(xlim, len = 2)
      checkmate::assert_count(n_points)
      x = seq(xlim[1], xlim[2], length.out = n_points)
      y = sapply(x, function(x) objective$eval(x))

      super$initialize(fun_x = x, fun_y = y, title = self$objective$label, lab_x = "x", lab_y = "y")
    },

    #' @description
    #' Add optimization trace to the plot.
    #'
    #' @param optimizer (`Optimizer`)\cr
    #'  The optimizer to add to the plot.
    add_optimization_trace = function(optimizer) {
      checkmate::assert_r6(optimizer, "Optimizer")

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
