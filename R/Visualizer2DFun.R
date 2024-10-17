#FIXME: where is z_lab used?


#' @title Visualize Base Class
#'
#' @description
#' This class is used to create 2D visualizations.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer2DFun = R6::R6Class("Visualizer2DFun",
  public = list(

    #' @field grid (`list()`)\cr
    #' List with the `x1` and `x2` grid.
    grid = NULL,

    #' @field zmat (`matrix()`)\cr
    #' The result of evaluation at each element of the cross product of `grid$x1` and `grid$x2`.
    zmat = NULL,

    #' @field title (character(1)\cr
    #' Label of the plot.
    title = NULL,

    #' @field lab_x1 (character(1)\cr
    #' Label of the x1 axis.
    lab_x1 = NULL,

    #' @field lab_x2 (character(1)\cr
    #' Label of the x2 axis.
    lab_x2 = NULL,

    #' @field z_lab (character(1)\cr
    #' Label of the z axis.
    z_lab = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param grid (`list()`)\cr
    #'   List with the `x1` and `x2` grid.
    #' @param zmat (`matrix()`)\cr
    #'   The result of evaluation at each element of the cross product of `grid$x1` and `grid$x2`.
    #' @param title (`character(1)`)\cr
    #'  Label of the plot.
    #' @param lab_x1 (`character(1)`)\cr
    #'  Label of the x1 axis.
    #' @param lab_x2 (`character(1)`)\cr
    #' Label of the x2 axis.
    #' @param z_lab (`character(1)`)\cr
    #' Label of the z axis.
    initialize = function(fun_x1, fun_x2, fun, title = NULL, lab_x1 = "x1", lab_x2 = "x2", z_lab = "z") {
      self$fun_x1 = assert_numeric(fun_x1)
      self$fun_x2 = assert_numeric(fun_x2)
      self$fun = assert_function(fun)
      self$title = assert_character(title, null.ok = TRUE)
      self$lab_x1 = assert_character(lab_x1)
      self$lab_x2 = assert_character(lab_x2)
      self$z_lab = assert_character(z_lab)
      self$fun_data = expand.grid(self$fun_1, self$fun_x2)
      return(invisible(self))
    },

    plot = function() {
      pl = ggplot(data = self$fun_data, aes(x = x1, y = x2, z = y))
      pl = pl + geom_contour_filled(aes(fill = ..level..), show.legend = FALSE)
      pl = pl + geom_contour(color = "white")
      pl = pl + scale_fill_viridis_c()
      pl = pl + labs(title = self$title, x = self$lab_x, y = self$lab_y)
    }
  )
)

#' Randomly generate colors
#' @description Helper function to generate RGB colors.
#' @param alpha (`numeric(1)`) The alpha value. If `!is.null` the used prefix is 'rgba' instead of 'rgb'.
#' @return A character of length one containing the RGB color.
#' @import plotly
#' @import colorspace
#' @export
colSampler = function(alpha = NULL) {
# FIXME: do we really need this?
  checkmate::assertNumber(alpha, lower = 0, upper = 1, null.ok = TRUE)
  r = sample(seq(0, 255), 1)
  g = sample(seq(0, 255), 1)
  b = sample(seq(0, 255), 1)

  if (is.null(alpha)) {
    rgb = "rgb"
  } else {
    rgb = "rgba"
  }
  clr = sprintf("%s(%s)", rgb, paste(c(r, g, b, alpha), collapse = ", "))
  return(clr)
}
