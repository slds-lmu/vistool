#' @title Visualize 2D Functions
#'
#' @description
#' Visualizes a two-dimensional function \eqn{f: \mathbb{R}^2 \to \mathbb{R}}.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer2D = R6::R6Class("Visualizer2D",
  inherit = Visualizer,
  public = list(

    #' @field fun_x1 (`numeric(n)`)
    fun_x1 = NULL,

    #' @field fun_x2 (`numeric(n)`)
    fun_x2 = NULL,

    #' @field fun_y (`numeric(n)`)
    fun_y = NULL,

    #' @field title (`character(1)`)
    title = NULL,

    #' @field lab_x1 (`character(1)`)
    lab_x1 = NULL,

    #' @field lab_x2 (`character(1)`)
    lab_x2 = NULL,

    #' @field lab_y (`character(1)`)
    lab_y = NULL,

    #' @field points_x1 (`numeric()`)\cr
    points_x1 = NULL,

    #' @field points_x2 (`numeric()`)\cr
    points_x2 = NULL,

    #' @field points_y (`numeric()`)\cr
    #' y-values of points.
    points_y = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param fun_x1 (`numeric()`)
    #'  x-values of function.
    #' @param fun_x2 (`numeric()`)
    #' x-values of function.
    #' @param fun_y (`numeric()`)
    #' y-values of function.
    #' @param title (`character(1)`)
    #' Title of plot.
    #' @param lab_x1 (`character(1)`)
    #' Label of x-axis.
    #' @param lab_x2 (`character(1)`)
    #' Label of x-axis.
    #' @param lab_y (`character(1)`)
    #' Label of y-axis.
    initialize = function(
      fun_x1,
      fun_x2,
      fun_y,
      title = NULL,
      lab_x1 = "x1",
      lab_x2 = "x2",
      lab_y = "y"
      ) {
      self$fun_x1 = checkmate::assert_numeric(fun_x1)
      self$fun_x2 = checkmate::assert_numeric(fun_x2)
      self$fun_y = checkmate::assert_numeric(fun_y)
      self$title = checkmate::assert_character(title, null.ok = TRUE)
      self$lab_x1 = checkmate::assert_character(lab_x1)
      self$lab_x2 = checkmate::assert_character(lab_x2)
      self$lab_y = checkmate::assert_character(lab_y)
    },

    #' @description
    #' Create and return the ggplot2 plot.
    #' @return A ggplot2 object.
    plot = function() {
      data = data.table(
        fun_x1 = self$fun_x1,
        fun_x2 = self$fun_x2,
        fun_y = self$fun_y)

      # adjust y breaks
      min_y = min(self$fun_y)
      max_y = max(self$fun_y)
      breaks = if (!is.null(self$points_y)) {
        pretty(c(min_y, max_y), n = 10, min.n = 6L)
      } else {
        pretty(c(min_y, max_y), n = 10, min.n = 6L)
      }

      p = ggplot(data, aes(x = fun_x1, y = fun_x2, z = fun_y)) +
        geom_contour_filled(breaks = breaks, show.legend = TRUE) +
        geom_contour(color = "white", alpha = 0.3) +
        labs(title = self$title, x = self$lab_x1, y = self$lab_x2) +
        scale_fill_viridis_d(name = self$lab_y, drop = FALSE) +
        theme_minimal()

      # add decision boundary if available (for classification)
      if (!is.null(private$.decision_threshold)) {
        p = p + geom_contour(aes(z = fun_y), breaks = private$.decision_threshold, 
                           color = "black", linewidth = 1.5, alpha = 0.8)
      }

      # add training points if available
      if (!is.null(self$points_x1) && !is.null(self$points_x2) && !is.null(self$points_y)) {
        points_data = data.table(
          points_x1 = self$points_x1,
          points_x2 = self$points_x2,
          points_y = self$points_y)

        # determine color scale limits based on function values
        color_limits = c(min(self$fun_y), max(self$fun_y))
        
        p = p + geom_point(aes(x = points_x1, y = points_x2, color = points_y),
          data = points_data,
          size = 2,
          inherit.aes = FALSE,
          show.legend = FALSE) +
          scale_color_viridis_c(name = self$lab_y, limits = color_limits)
      }

      return(p)
    },
    
    #' @description
    #' Initialize contour layer (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    init_layer_contour = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },
    
    #' @description
    #' Initialize surface layer (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    init_layer_surface = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },
    
    #' @description
    #' Add optimization trace (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    add_optimization_trace = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },
    
    #' @description
    #' Set layout (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    set_layout = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },
    
    #' @description
    #' Set scene (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    set_scene = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },
    
    #' @description
    #' Add layer taylor (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    add_layer_taylor = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },
    
    #' @description
    #' Add layer hessian (compatibility method, no-op for ggplot2 version).
    #' @param ... Additional arguments (ignored for compatibility).
    add_layer_hessian = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    }
  ),
  
  private = list(
    .decision_threshold = NULL
  )
)

