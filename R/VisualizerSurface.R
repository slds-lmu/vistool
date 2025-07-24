#' @title Visualize 2D Functions as Interactive Surfaces
#'
#' @description
#' Visualizes a two-dimensional function \eqn{f: \mathbb{R}^2 \to \mathbb{R}} via interactive plotly renderings.
#' Creates 3D surface and contour plots for better visualization of 2D functions.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
VisualizerSurface <- R6::R6Class("VisualizerSurface",
  inherit = Visualizer,
  public = list(

    #' @template field_grid_x1x2
    grid = NULL,

    #' @template field_zmat
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

    #' @field opacity (`numeric(1)`)\cr
    #' Opacity of the surface plot.
    opacity = 0.8,

    #' @field colorscale (`list()`)\cr
    #' Color scale for the surface plot.
    colorscale = NULL,

    #' @field show_contours (`logical(1)`)\cr
    #' Whether to show contours on the surface plot.
    show_contours = FALSE,

    #' @field contours (`list()`)\cr
    #' Custom contour configuration for the surface plot.
    contours = NULL,

    #' @field show_title (`logical(1)`)\cr
    #' Whether to show the plot title.
    show_title = TRUE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param grid (`list()`)\cr
    #'   List with the `x1` and `x2` grid.
    #' @param zmat (`matrix()`)\cr
    #'   The result of evaluation at each element of the cross product of `grid$x1` and `grid$x2`.
    #' @template param_plot_lab
    #' @template param_x1_lab
    #' @template param_x2_lab
    #' @template param_z_lab
    #' @template param_opacity
    #' @template param_colorscale
    #' @param show_contours (`logical(1)`)\cr
    #'   Whether to show contours on the surface plot. Default is FALSE.
    #' @param contours (`list()`)\cr
    #'   Custom contour configuration for the surface plot. If provided, this takes precedence over `show_contours`.
    #' @template param_show_title
    initialize = function(grid, zmat, plot_lab = NULL, x1_lab = "x1", x2_lab = "x2", z_lab = "z", 
                          opacity = 0.8, colorscale = list(
                            c(0, "#440154"), c(0.25, "#3b528b"), c(0.5, "#21908c"), 
                            c(0.75, "#5dc863"), c(1, "#fde725")
                          ), show_contours = FALSE, contours = NULL, show_title = TRUE) {
      self$grid <- checkmate::assert_list(grid)
      self$zmat <- checkmate::assert_matrix(zmat)
      self$plot_lab <- checkmate::assert_character(plot_lab, null.ok = TRUE)
      self$x1_lab <- checkmate::assert_character(x1_lab)
      self$x2_lab <- checkmate::assert_character(x2_lab)
      self$z_lab <- checkmate::assert_character(z_lab)
      self$opacity <- checkmate::assert_number(opacity, lower = 0, upper = 1)
      self$colorscale <- checkmate::assert_list(colorscale)
      self$show_contours <- checkmate::assert_flag(show_contours)
      self$contours <- checkmate::assert_list(contours, null.ok = TRUE)
      self$show_title <- checkmate::assert_flag(show_title)
      return(invisible(self))
    },

    #' @description
    #' Initialize the plot as 2D contour.
    #' This method is called automatically by plot() and typically doesn't need to be called directly.
    #'
    #' @template param_opacity
    #' @template param_colorscale
    #' @template param_show_title
    #' @template param_dots_trace
    init_layer_contour = function(opacity = self$opacity, colorscale = self$colorscale, show_title = self$show_title, ...) {
      checkmate::assert_number(opacity, lower = 0, upper = 1)
      checkmate::assert_list(colorscale)
      checkmate::assert_flag(show_title)

      private$.vbase <- c(as.list(environment()), list(...))
      private$.layer_primary <- "contour"

      llp <- list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      private$.plot <- plot_ly() %>%
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
          yaxis = list(title = self$x2_lab)
        )

      if (!private$.freeze_plot) { # Used in animate to not overwrite the
        private$.opts <- list() # plot over and over again when calling
        private$.layer_arrow <- list() # `$initLayerXXX`.
      }

      return(invisible(self))
    },

    #' @description
    #' Initialize the plot as 3D surface.
    #' This method is called automatically by plot() and typically doesn't need to be called directly.
    #'
    #' @template param_opacity
    #' @template param_colorscale
    #' @template param_show_title
    #' @param show_contours (`logical(1)`)\cr
    #'  Indicator whether to show the contours of the surface.
    #' @param contours (`list()`)\cr
    #'  Custom contour configuration for the surface. If provided, this takes precedence over `show_contours`.
    #'  Can specify x, y, and z contours with custom properties like start, end, size, and color.
    #'  See plotly documentation for detailed contour options.
    #' @template param_dots_trace
    init_layer_surface = function(opacity = self$opacity, colorscale = self$colorscale, 
                                  show_contours = self$show_contours, contours = self$contours, 
                                  show_title = self$show_title, ...) {
      checkmate::assert_number(opacity, lower = 0, upper = 1)
      checkmate::assert_list(colorscale)
      checkmate::assert_flag(show_contours)
      checkmate::assert_list(contours, null.ok = TRUE)
      checkmate::assert_flag(show_title)

      private$.vbase <- c(as.list(environment()), list(...))
      private$.layer_primary <- "surface"

      # Handle contours: custom contours take precedence over show_contours
      if (!is.null(contours)) {
        # Use provided custom contours
        contours_final <- contours
      } else if (show_contours) {
        # Use default z-projected contours
        contours_final <- list(
          z = list(
            show = TRUE,
            project = list(z = TRUE),
            usecolormap = TRUE
          )
        )
      } else {
        contours_final <- NULL
      }

      llp <- list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      private$.plot <- plot_ly() %>%
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
          contours = contours_final,
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

      if (!private$.freeze_plot) { # Used in animate to not overwrite the plot over and over again.
        private$.opts <- list()
        private$.layer_arrow <- list()
      }

      return(invisible(self))
    },

    #' @description Set the layout of the plotly plot.
    #' @param ... Layout options directly passed to `layout(...)`.
    set_layout = function(...) {
      private$.layout <- list(...)
      private$.plot <- private$.plot %>% layout(...)

      return(invisible(self))
    },

    #' @description Set the view for a 3D plot.
    #' @param x (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param y (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param z (`numeric(1)`) The view from which the "camera looks down" to the plot.
    set_scene = function(x, y, z) {
      if (is.null(private$.plot)) private$.init_default_plot()
      checkmate::assert_number(x)
      checkmate::assert_number(y)
      checkmate::assert_number(z)

      if (private$.layer_primary != "surface") {
        stop("Scene can only be set for `surface` plots")
      }

      private$.plot <- private$.plot %>%
        layout(scene = list(camera = list(eye = list(x = x, y = y, z = z))))

      return(invisible(self))
    },

    #' @description Return the plot and hence plot it or do further processing.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 12. For 3D plotly calls, this controls axis labels and title sizes.
    #' @param theme (`character(1)`)\cr
    #'   Theme parameter for compatibility with ggplot2-based visualizers. Ignored for 3D plotly plots.
    #' @param flatten (`logical(1)`)\cr
    #'   If TRUE, display as 2D contour plot. If FALSE (default), display as 3D surface plot.
    #' @param layout (`list()`)\cr
    #'   Layout options passed directly to `plotly::layout()`. If NULL, no additional layout modifications are applied.
    #' @param scene (`list()`)\cr
    #'   Scene options for 3D surface plots. Should contain camera settings like `list(camera = list(eye = list(x = 1.1, y = 1.2, z = 1.3)))`. 
    #'   Only applies to surface plots, ignored for contour plots. If NULL, no scene modifications are applied.
    #' @return A plotly object.
    plot = function(text_size = 12, theme = "minimal", flatten = FALSE, layout = NULL, scene = NULL) {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      checkmate::assert_flag(flatten)
      checkmate::assert_list(layout, null.ok = TRUE)
      checkmate::assert_list(scene, null.ok = TRUE)
      
      # Validate scene parameter is only used for surface plots
      if (!is.null(scene) && flatten) {
        stop("Scene parameter can only be used for surface plots (flatten = FALSE)")
      }
      
      # Initialize appropriate plot type based on flatten parameter
      if (is.null(private$.plot) || 
          (flatten && private$.layer_primary != "contour") ||
          (!flatten && private$.layer_primary != "surface")) {
        if (flatten) {
          self$init_layer_contour()
        } else {
          private$.init_default_plot()  # This calls init_layer_surface
        }
      }
      
      # apply text size to plotly layout
      if (!is.null(private$.plot)) {
        if (private$.layer_primary == "surface") {
          # For 3D surface plots
          private$.plot <- private$.plot %>%
            layout(
              title = list(text = self$plot_lab, font = list(size = text_size + 2)),
              scene = list(
                xaxis = list(title = list(text = self$x1_lab, font = list(size = text_size))),
                yaxis = list(title = list(text = self$x2_lab, font = list(size = text_size))),
                zaxis = list(title = list(text = self$z_lab, font = list(size = text_size)))
              )
            )
        } else {
          # For 2D contour plots
          private$.plot <- private$.plot %>%
            layout(
              title = list(text = self$plot_lab, font = list(size = text_size + 2)),
              xaxis = list(title = list(text = self$x1_lab, font = list(size = text_size))),
              yaxis = list(title = list(text = self$x2_lab, font = list(size = text_size)))
            )
        }
      }
      
      # Apply additional layout options if provided
      if (!is.null(layout)) {
        private$.plot <- do.call(plotly::layout, c(list(private$.plot), layout))
      }
      
      # Apply scene options if provided (only for surface plots)
      if (!is.null(scene) && private$.layer_primary == "surface") {
        private$.plot <- private$.plot %>% plotly::layout(scene = scene)
      }
      
      return(private$.plot)
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
    
    # Initialize default surface plot (called automatically by plot())
    .init_default_plot = function() {
      self$init_layer_surface()
    },
    
    checkInit = function() {
      if (is.null(private$.plot)) {
        private$.init_default_plot()
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
#' @export
colSampler <- function(alpha = NULL) {
  checkmate::assertNumber(alpha, lower = 0, upper = 1, null.ok = TRUE)
  r <- sample(seq(0, 255), 1)
  g <- sample(seq(0, 255), 1)
  b <- sample(seq(0, 255), 1)

  if (is.null(alpha)) {
    rgb <- "rgb"
  } else {
    rgb <- "rgba"
  }
  clr <- sprintf("%s(%s)", rgb, paste(c(r, g, b, alpha), collapse = ", "))
  return(clr)
}

#' Get consistent color palette
#' 
#' @description
#' Returns a consistent color palette that matches the ggplot2 implementation.
#' This ensures visual consistency between ggplot2 and plotly visualizations.
#' 
#' @param index (`integer(1)`)\cr
#'   Index of the color to retrieve from the palette.
#' @return A character string containing the color in hex format.
get_consistent_color <- function(index) {
  # Same color palette as used in Visualizer2DObj
  colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  color_index <- ((index - 1) %% length(colors)) + 1
  return(colors[color_index])
}
