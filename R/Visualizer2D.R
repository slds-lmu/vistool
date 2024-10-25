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
Visualizer2D = R6::R6Class("Visualizer2D",
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
    initialize = function(fun_x1, fun_x2, fun_y, title = NULL, lab_x1 = "x1", lab_x2 = "x2", lab_y = "y") {
      self$fun_x1 = assert_numeric(fun_x1)
      self$fun_x2 = assert_numeric(fun_x2)
      self$fun_y = assert_numeric(fun_y)
      self$title = assert_character(title, null.ok = TRUE)
      self$lab_x1 = assert_character(lab_x1)
      self$lab_x2 = assert_character(lab_x2)
      self$lab_y = assert_character(lab_y)
    },

    plot = function() {
      data = data.table(fun_x1 = self$fun_x1, fun_x2 = self$fun_x2, fun_y = self$fun_y)

      p = ggplot(data, aes(x = fun_x1, y = fun_x2, z = fun_y)) +
        geom_contour_filled() +
        geom_contour(color = "white") +
        labs(title = self$title, x = self$lab_x1, y = self$lab_x2)

      if (!is.null(self$points_x1)) {
        data = data.table(points_x1 = self$points_x1, points_x2 = self$points_x2, points_y = self$points_y)
        p = p + geom_point(data = data, aes(x = points_x1, y = points_x2, color = self$points_y), size = 5)
      }

      return(p)
    }
  )
)

