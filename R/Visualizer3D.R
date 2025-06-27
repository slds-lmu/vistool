#' @title Visualize Base Class
#'
#' @description
#' This class is used to create 3D visualizations.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer3D = R6::R6Class("Visualizer3D",
  public = list(

    #' @field grid (`list()`)\cr
    #' List with the `x1` and `x2` grid.
    grid = NULL,

    #' @field zmat (`matrix()`)\cr
    #' The result of evaluation at each element of the cross product of `grid$x1` and `grid$x2`.
    zmat = NULL,

    #' @field plot_lab (character(1)\cr
    #' Label of the plot.
    plot_lab = NULL,

    #' @field x1_lab (character(1)\cr
    #' Label of the x1 axis.
    x1_lab = NULL,

    #' @field x2_lab (character(1)\cr
    #' Label of the x2 axis.
    x2_lab = NULL,

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
    #' @param plot_lab (`character(1)`)\cr
    #'  Label of the plot.
    #' @param x1_lab (`character(1)`)\cr
    #'  Label of the x1 axis.
    #' @param x2_lab (`character(1)`)\cr
    #' Label of the x2 axis.
    #' @param z_lab (`character(1)`)\cr
    #' Label of the z axis.
    initialize = function(grid, zmat, plot_lab = NULL, x1_lab = "x1", x2_lab = "x2", z_lab = "z") {
      self$grid = checkmate::assert_list(grid)
      self$zmat = checkmate::assert_matrix(zmat)
      self$plot_lab = checkmate::assert_character(plot_lab, null.ok = TRUE)
      self$x1_lab = checkmate::assert_character(x1_lab)
      self$x2_lab = checkmate::assert_character(x2_lab)
      self$z_lab = checkmate::assert_character(z_lab)
      return(invisible(self))
    },

    #' @description
    #' Initialize the plot with contour lines.
    #'
    #' @param opacity (`numeric(1)`)\cr
    #'   Opacity of the layer.
    #' @param colorscale (`list()`)\cr
    #'   The coloring of the contour.
    #' @param show_title (`logical(1)`)\cr
    #'   Indicator whether to show the title of the plot.
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    init_layer_contour = function(opacity = 0.8, colorscale = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")), show_title = TRUE, ...) {
      checkmate::assert_number(opacity, lower = 0, upper = 1)
      checkmate::assert_list(colorscale)
      checkmate::assert_flag(show_title)

      private$.vbase = c(as.list(environment()), list(...))
      private$.layer_primary = "contour"

      llp = list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      private$.plot = plot_ly() %>%
        add_trace(
          name = self$plot_lab,
          showlegend = TRUE,
          showscale = TRUE,
          x = llp$x,
          y = llp$y,
          z = t(llp$z),
          type = "contour",
          opacity = opacity,
          colorscale = colorscale,
          ...
        ) %>%
        layout(
          title = if (show_title) self$plot_lab else NULL,
          xaxis = list(title = self$x1_lab),
          yaxis = list(title = self$x2_lab))

      if (! private$.freeze_plot) {    # Used in animate to not overwrite the
        private$.opts = list()         # plot over and over again when calling
        private$.layer_arrow = list()  # `$initLayerXXX`.
      }

      return(invisible(self))
    },

    #' @description
    #' Initialize the plot as 3D surface.
    #'
    #' @param opacity (`numeric(1)`)\cr
    #'   Opacity of the layer.
    #' @param colorscale (`list()`)\cr
    #'   The coloring of the surface.
    #' @param show_title (`logical(1)`)\cr
    #'   Indicator whether to show the title of the plot.
    #' @param show_contours (`logical(1)`)\cr
    #'  Indicator whether to show the contours of the surface.
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    init_layer_surface = function(opacity = 0.8, colorscale = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")), show_contours = FALSE, show_title = TRUE, ...) {
      checkmate::assert_number(opacity, lower = 0, upper = 1)
      checkmate::assert_list(colorscale)
      checkmate::assert_flag(show_title)

      private$.vbase = c(as.list(environment()), list(...))
      private$.layer_primary = "surface"

      contours = if (show_contours) {
        list(
          z = list(
            show = TRUE,
            project = list(z = TRUE),
            usecolormap = TRUE)
        )
      } else NULL

      llp = list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      private$.plot = plot_ly() %>%
        add_trace(
          name = self$plot_lab,
          showlegend = FALSE,
          showscale = FALSE,
          x = llp$x,
          y = llp$y,
          z = t(llp$z),
          type = "surface",
          opacity = opacity,
          colorscale = colorscale,
          contours = contours,
          ...
        ) %>%
        layout(
          title = if (show_title) self$plot_lab else NULL,
          scene = list(
            xaxis = list(title = self$x1_lab),
            yaxis = list(title = self$x2_lab),
            zaxis = list(title = self$z_lab)
          )
        )

      if (! private$.freeze_plot) { # Used in animate to not overwrite the plot over and over again.
        private$.opts = list()
        private$.layer_arrow = list()
      }

      return(invisible(self))
    },

    #' @description Set the layout of the plotly plot.
    #' @param ... Layout options directly passed to `layout(...)`.
    set_layout = function(...) {
      private$.layout = list(...)
      private$.plot = private$.plot %>% layout(...)

      return(invisible(self))
    },

    #' @description Set the view for a 3D plot.
    #' @param x (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param y (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param z (`numeric(1)`) The view from which the "camera looks down" to the plot.
    set_scene = function(x, y, z) {
      if (is.null(private$.plot)) self$init_layer_surface()
      checkmate::assert_number(x)
      checkmate::assert_number(y)
      checkmate::assert_number(z)

      if (private$.layer_primary != "surface") {
        stop("Scene can only be set for `surface` plots")
      }

      private$.plot = private$.plot %>%
        layout(scene = list(camera = list(eye = list(x = x, y = y, z = z))))

      return(invisible(self))
    },

    #' @description Return the plot and hence plot it or do further processing.
    plot = function() {
      if (is.null(private$.plot)) self$init_layer_surface()
      return(private$.plot)
    },


    #' @description Save the plot by using plotlys `save_image()` function.
    #' @param ... Further arguments passed to `save_image()`.
    save = function(...) {
      if (is.null(private$.plot)) self$init_layer_surface()
      save_image(private$.plot, ...)
    }
  ),
  private = list(
    # @field .layer_primary (`character(1)`) The id of the primary layer. Used to determine
    # the trace setup.
    .layer_primary = NULL,

    # @field .layer_arrow (`list()`) Arguments passed to `$addLayerArrow()` to reconstruct the plot for animations.
    .layer_arrow = list(),

    # @field .plot (`plot_ly()`) The plot.
    .plot = NULL,

    # @field .opts (`list(Optimizer)`) List of optimizers used to add traces. Each `$initLayerXXX()`
    # resets this list. An optimizer is added after each call to `$addLayerOptimizationTrace()`.
    # this private field is exclusively used to create animations with `$animate()`.
    .opts = list(),

    .vbase = list(),

    .layout = list(),

    # @field .freeze_plot (`logical(1)`) Indicator whether to freeze saving the plot elements.
    .freeze_plot = FALSE,

    checkInit = function() {
      if (is.null(private$.plot)) {
        stop("Initialize plot with `initLayer*`")
      }
      return(invisible(TRUE))
    },
    checkInput = function(x) {
      if (private$.layer_primary == "surface") {
        return(checkmate::assertNumeric(x, len = 3L))
      }
      if (private$.layer_primary == "contour") {
        return(checkmate::assertNumeric(x, len = 3L))
      }
      stop("Error in `$checkInput()`")
    }
  )
)

#' Randomly generate colors
#' @description Helper function to generate RGB colors.
#' @param alpha (`numeric(1)`) The alpha value. If `!is.null` the used prefix is 'rgba' instead of 'rgb'.
#' @return A character of length one containing the RGB color.
#' @importFrom plotly plot_ly add_trace add_surface layout save_image schema
#' @import colorspace
#' @export
colSampler = function(alpha = NULL) {
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
