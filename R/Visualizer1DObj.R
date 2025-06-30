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

    #' @template field_objective
    objective = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_objective
    #' @param xlim (`numeric(2)`)\cr
    #'   Limits for the x-axis. If NULL, will be determined from objective bounds.
    #' @template param_n_points
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
    #' @template return_self_invisible
    add_optimization_trace = function(optimizer) {
      checkmate::assert_r6(optimizer, "Optimizer")

      archive = optimizer$archive
      
      # Extract x and y values from archive for plotting
      if (nrow(archive) > 0) {
        # For 1D objectives, x_in is a list of vectors, we need the first element
        x_vals = sapply(archive$x_in, function(x) x[1])
        y_vals = archive$fval_in
        
        # Add points to the visualizer
        self$points_x = x_vals
        self$points_y = y_vals
        
        # Set some default styling if not set
        if (is.null(self$points_col)) self$points_col = "red"
        if (is.null(self$points_size)) self$points_size = 2
        if (is.null(self$points_shape)) self$points_shape = 16
        if (is.null(self$points_alpha)) self$points_alpha = 0.8
      }
      
      invisible(self)
    }
  )
)
