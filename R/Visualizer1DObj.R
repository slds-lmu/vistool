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
Visualizer1DObj <- R6::R6Class("Visualizer1DObj",
  inherit = Visualizer1D,
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
      self$objective <- checkmate::assert_r6(objective, "Objective")
      if (objective$xdim != 1) {
        mlr3misc::stopf("`Visualizer1D` requires 1-dimensional inputs, but `objective$xdim = %s`", objective$xdim)
      }
      xlim <- xlim %??% c(objective$lower, objective$upper)
      if (any(is.na(xlim))) {
        stop("Limits could not be extracted from the objective. Please use `xlim`.")
      }
      checkmate::assert_numeric(xlim, len = 2)
      checkmate::assert_count(n_points)
      x <- seq(xlim[1], xlim[2], length.out = n_points)
      y <- sapply(x, function(x) objective$eval(x))

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

      archive <- optimizer$archive

      # Extract x and y values from archive for plotting
      if (nrow(archive) > 0) {
        # For 1D objectives, x_in is a list of vectors, we need the first element
        x_vals <- sapply(archive$x_in, function(x) x[1])
        y_vals <- archive$fval_in

        # Store optimization trace specification without resolving colors yet
        private$store_layer("optimization_trace", list(
          x_vals = x_vals,
          y_vals = y_vals,
          color = "auto",  # Will be resolved at plot time
          size = 2,
          shape = 16,
          alpha = 0.8
        ))
      }

      invisible(self)
    },

    #' @description
    #' Create and return the ggplot2 plot with optimization traces.
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(...) {
      # Call parent plot method first
      p <- super$plot(...)
      
      # Render optimization traces if any exist
      private$render_optimization_trace_layers(p)
    }
  ),
  private = list(
    # Render stored optimization trace layers
    render_optimization_trace_layers = function(plot_obj) {
      # Get all stored optimization trace layers
      trace_layers <- private$get_layers_by_type("optimization_trace")
      
      if (length(trace_layers) == 0) {
        return(plot_obj)
      }
      
      for (trace_spec in trace_layers) {
        plot_obj <- private$render_optimization_trace_layer(plot_obj, trace_spec)
      }
      
      return(plot_obj)
    },
    
    # Render a single optimization trace layer
    render_optimization_trace_layer = function(plot_obj, layer_spec) {
      dd_trace <- data.frame(x = layer_spec$x_vals, y = layer_spec$y_vals)
      
      plot_obj <- plot_obj + ggplot2::geom_point(
        data = dd_trace, 
        size = layer_spec$size, 
        color = layer_spec$color,
        shape = layer_spec$shape, 
        alpha = layer_spec$alpha
      )
      
      return(plot_obj)
    }
  )
)
