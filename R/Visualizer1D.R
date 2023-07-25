#' @title Visualize Base Class
#'
#' @description
#' This class is used to create 1D visualizations.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer1D = R6::R6Class("Visualizer1D",
  public = list(

    #' @field x (`vector()`)\cr
    #' x-values.
    x = NULL,

    #' @field y (`vector()`)\cr
    #' y-values
    y = NULL,

    #' @field plot_lab (character(1)\cr
    #' Label of the plot.
    plot_lab = NULL,

    #' @field x_lab (character(1)\cr
    #' Label of the x axis.
    x_lab = NULL,

    #' @field y_lab (character(1)\cr
    #' Label of the y axis.
    y_lab = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param x (`numeric()`)\cr
    #'   x-values.
    #' @param y (`numeric()`)\cr
    #'   y-values.
    #' @param plot_lab (character(1)\cr
    #'   Label of the plot.
    #' @param x_lab (character(1)\cr
    #'   Label of the x axis.
    #' @param y_lab (character(1)\cr
    #'   Label of the y axis.
    initialize = function(x, y, plot_lab = NULL, x_lab = "x", y_lab = "y") {
      self$x = assert_numeric(x)
      self$y = assert_numeric(y)
      self$plot_lab = assert_character(plot_lab, null.ok = TRUE)
      self$x_lab = assert_character(x_lab)
      self$y_lab = assert_character(y_lab)
      return(invisible(self))
    },

    #' @description
    #' Initialize the plot with a line plot.
    #'
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    init_layer_lines = function(...) {
       private$.plot = plot_ly() %>%
        add_trace(
          name = self$plot_lab,
          showlegend = FALSE,
          x = self$x,
          y = self$y,
          type = "scatter",
          mode = "lines",
          ...
        ) %>%
        layout(
          title = self$plot_lab,
          xaxis = list(title = self$x_lab),
          yaxis = list(title = self$y_lab))

      return(invisible(self))
    },

    #' @description
    #' Set the layout of the plotly plot.
    #'
    #' @param ... (`any`)\cr
    #'   Layout options directly passed to `layout(...)`.
    setLayout = function(...) {
      private$p_layout = list(...)
      private$p_plot = private$p_plot %>% layout(...)

      return(invisible(self))
    },

    #' @description
    #' Return the plot and hence plot it or do further processing.
    plot = function() {
       if (is.null(private$.plot)) self$init_layer_lines()
      return(private$.plot)
    },

    #' @description
    #' Save the plot by using plotlys `orca()` function.
    #'
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `orca()`.
    save = function(...) {
      orca(private$.plot, ...)
    }
  ),

  private = list(
    .plot = NULL
  )
)
