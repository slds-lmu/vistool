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
    #' @template param_show_title
    initialize = function(grid, zmat, plot_lab = NULL, x1_lab = "x1", x2_lab = "x2", z_lab = "z", 
                          opacity = 0.8, colorscale = list(
                            c(0, "#440154"), c(0.25, "#3b528b"), c(0.5, "#21908c"), 
                            c(0.75, "#5dc863"), c(1, "#fde725")
                          ), show_title = TRUE) {
      self$grid <- checkmate::assert_list(grid)
      self$zmat <- checkmate::assert_matrix(zmat)
      self$plot_lab <- checkmate::assert_character(plot_lab, null.ok = TRUE)
      self$x1_lab <- checkmate::assert_character(x1_lab)
      self$x2_lab <- checkmate::assert_character(x2_lab)
      self$z_lab <- checkmate::assert_character(z_lab)
      self$opacity <- checkmate::assert_number(opacity, lower = 0, upper = 1)
      self$colorscale <- checkmate::assert_list(colorscale)
      self$show_title <- checkmate::assert_flag(show_title)
      return(invisible(self))
    },

    #' @description
    #' Initialize the plot as 2D contour.
    #' This method is called automatically by plot() and should not be called directly.
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
    #' This method is called automatically by plot() and should not be called directly.
    #'
    #' @template param_opacity
    #' @template param_colorscale
    #' @template param_show_title
    #' @template param_dots_trace
    init_layer_surface = function(opacity = self$opacity, colorscale = self$colorscale, 
                                  show_title = self$show_title, ...) {
      checkmate::assert_number(opacity, lower = 0, upper = 1)
      checkmate::assert_list(colorscale)
      checkmate::assert_flag(show_title)

      private$.vbase <- c(as.list(environment()), list(...))
      private$.layer_primary <- "surface"

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
    #' This method is used internally by plot(layout = ...) and should not be called directly.
    #' @param ... Layout options directly passed to `layout(...)`.
    set_layout = function(...) {
      private$.layout <- list(...)
      private$.plot <- private$.plot %>% layout(...)

      return(invisible(self))
    },

    #' @description Set the view for a 3D plot.
    #' This method is used internally by plot(scene = ...) and should not be called directly.
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

    #' @description
    #' Add contours to the surface plot.
    #'
    #' @param contours (`list()` or `NULL`)\cr
    #'   Custom contour configuration. If `NULL` (default), adds default z-projected contours.
    #'   Can specify x, y, and z contours with custom properties like start, end, size, and color.
    #'   See plotly documentation for detailed contour options.
    #' @param ... Additional arguments passed to the contour trace.
    #' @return Self (invisibly) for method chaining.
    add_contours = function(contours = NULL, ...) {
      if (is.null(private$.plot)) private$.init_default_plot()
      checkmate::assert_list(contours, null.ok = TRUE)
      
      if (private$.layer_primary != "surface") {
        stop("Contours can only be added to surface plots")
      }
      
      # Handle contours: custom contours or default z-projected contours
      if (is.null(contours)) {
        # Use default z-projected contours
        contours_final <- list(
          z = list(
            show = TRUE,
            project = list(z = TRUE),
            usecolormap = TRUE
          )
        )
      } else {
        # Use provided custom contours
        contours_final <- contours
      }
      
      # Update the existing surface trace to include contours
      # We need to reconstruct the surface with contours
      llp <- list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
      
      # Get the current opacity and colorscale from the existing plot or defaults
      current_opacity <- if(!is.null(private$.vbase$opacity)) private$.vbase$opacity else self$opacity
      current_colorscale <- if(!is.null(private$.vbase$colorscale)) private$.vbase$colorscale else self$colorscale
      current_show_title <- if(!is.null(private$.vbase$show_title)) private$.vbase$show_title else self$show_title
      
      # Recreate the plot with contours
      private$.plot <- plot_ly() %>%
        add_trace(
          name = self$plot_lab,
          showlegend = FALSE,
          showscale = FALSE,
          x = llp$x,
          y = llp$y,
          z = t(llp$z),
          type = "surface",
          opacity = current_opacity,
          colorscale = current_colorscale,
          contours = contours_final,
          ...
        ) %>%
        layout(
          title = if (current_show_title) self$plot_lab else NULL,
          scene = list(
            xaxis = list(title = self$x1_lab),
            yaxis = list(title = self$x2_lab),
            zaxis = list(title = self$z_lab)
          )
        )

      return(invisible(self))
    },

    #' @description Return the plot and hence plot it or do further processing.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 12. For 3D plotly calls, this controls axis labels and title sizes.
    #' @param title_size (`numeric(1)`)\cr
    #'   Title text size. If NULL, defaults to text_size + 2.
    #' @param theme (`character(1)`)\cr
    #'   Theme parameter for compatibility with ggplot2-based visualizers. Ignored for 3D plotly plots.
    #' @param background (`character(1)`)\cr
    #'   Background color parameter for compatibility. Ignored for 3D plotly plots.
    #' @param color_palette (`character(1)`)\cr
    #'   Color palette for the surface. One of "viridis", "plasma", "grayscale". Default is "viridis".
    #' @param flatten (`logical(1)`)\cr
    #'   If TRUE, display as 2D contour plot. If FALSE (default), display as 3D surface plot.
    #' @param layout (`list()`)\cr
    #'   Layout options passed directly to `plotly::layout()`. If NULL, no additional layout modifications are applied.
    #' @param scene (`list()`)\cr
    #'   Scene options for 3D surface plots. Should contain camera settings like `list(camera = list(eye = list(x = 1.1, y = 1.2, z = 1.3)))`. 
    #'   Only applies to surface plots, ignored for contour plots. If NULL, no scene modifications are applied.
    #' @template param_plot_title
    #' @template param_plot_subtitle
    #' @template param_x_lab
    #' @template param_y_lab
    #' @template param_z_lab_custom
    #' @template param_x_limits
    #' @template param_y_limits
    #' @template param_z_limits
    #' @template param_show_legend
    #' @template param_legend_title
    #' @template param_show_title
    #' @return A plotly object.
    plot = function(text_size = 12, title_size = NULL, theme = "minimal", background = "white", 
                    color_palette = "viridis", flatten = FALSE, layout = NULL, scene = NULL,
                    plot_title = NULL, plot_subtitle = NULL, x_lab = NULL, y_lab = NULL, z_lab = NULL,
                    x_limits = NULL, y_limits = NULL, z_limits = NULL, show_legend = TRUE, legend_title = NULL, show_title = TRUE) {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_number(title_size, lower = 1, null.ok = TRUE)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      checkmate::assert_string(background)
      checkmate::assert_choice(color_palette, choices = c("viridis", "plasma", "grayscale"))
      checkmate::assert_flag(flatten)
      checkmate::assert_list(layout, null.ok = TRUE)
      checkmate::assert_list(scene, null.ok = TRUE)
      checkmate::assert_string(plot_title, null.ok = TRUE)
      checkmate::assert_string(plot_subtitle, null.ok = TRUE)
      checkmate::assert_string(x_lab, null.ok = TRUE)
      checkmate::assert_string(y_lab, null.ok = TRUE)
      checkmate::assert_string(z_lab, null.ok = TRUE)
      checkmate::assert_numeric(x_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(y_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(z_limits, len = 2, null.ok = TRUE)
      checkmate::assert_flag(show_legend)
      checkmate::assert_string(legend_title, null.ok = TRUE)
      checkmate::assert_flag(show_title)
      checkmate::assert_flag(flatten)
      checkmate::assert_list(layout, null.ok = TRUE)
      checkmate::assert_list(scene, null.ok = TRUE)
      
      # Set default title size
      if (is.null(title_size)) title_size <- text_size + 2
      
      # Validate scene parameter is only used for surface plots
      if (!is.null(scene) && flatten) {
        stop("Scene parameter can only be used for surface plots (flatten = FALSE)")
      }
      
      # Get the appropriate colorscale based on palette choice
      plot_colorscale <- get_vistool_color(1, color_palette)
      
      # Initialize appropriate plot type based on flatten parameter
      if (is.null(private$.plot) || 
          (flatten && private$.layer_primary != "contour") ||
          (!flatten && private$.layer_primary != "surface")) {
        if (flatten) {
          self$init_layer_contour(colorscale = plot_colorscale)
        } else {
          self$init_layer_surface(colorscale = plot_colorscale)
        }
      }
      
      # apply text size to plotly layout
      if (!is.null(private$.plot)) {
        # Determine final labels
        final_title <- if (show_title) {
          if (!is.null(plot_title)) plot_title else self$plot_lab
        } else {
          ""  # Empty string instead of NULL for plotly
        }
        final_x_lab <- if (!is.null(x_lab)) x_lab else self$x1_lab
        final_y_lab <- if (!is.null(y_lab)) y_lab else self$x2_lab
        final_z_lab <- if (!is.null(z_lab)) z_lab else self$z_lab
        
        if (private$.layer_primary == "surface") {
          # For 3D surface plots
          private$.plot <- private$.plot %>%
            plotly::layout(
              title = list(text = final_title, font = list(size = title_size)),
              scene = list(
                xaxis = list(
                  title = list(text = final_x_lab, font = list(size = text_size)),
                  range = x_limits
                ),
                yaxis = list(
                  title = list(text = final_y_lab, font = list(size = text_size)),
                  range = y_limits
                ),
                zaxis = list(
                  title = list(text = final_z_lab, font = list(size = text_size)),
                  range = z_limits
                )
              ),
              showlegend = show_legend
            )
        } else {
          # For 2D contour plots
          private$.plot <- private$.plot %>%
            plotly::layout(
              title = list(text = final_title, font = list(size = title_size)),
              xaxis = list(
                title = list(text = final_x_lab, font = list(size = text_size)),
                range = x_limits
              ),
              yaxis = list(
                title = list(text = final_y_lab, font = list(size = text_size)),
                range = y_limits
              ),
              showlegend = show_legend
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
      
      # Add points from add_points() method
      if (private$.layer_primary == "surface") {
        private$.plot <- private$add_points_to_plotly(private$.plot, "surface")
      } else {
        private$.plot <- private$add_points_to_plotly(private$.plot, "contour")
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
    },
    
    # Override infer_z_values to use the surface's zmat
    infer_z_values = function(points_data) {
      # Use bilinear interpolation to estimate z values from the surface
      x_vals <- points_data$x
      y_vals <- points_data$y
      
      # Get grid ranges
      x1_range <- self$grid$x1
      x2_range <- self$grid$x2
      
      z_vals <- numeric(length(x_vals))
      
      for (i in seq_along(x_vals)) {
        x <- x_vals[i]
        y <- y_vals[i]
        
        # Find closest grid points
        x1_idx <- which.min(abs(x1_range - x))
        x2_idx <- which.min(abs(x2_range - y))
        
        # Clamp to grid boundaries
        x1_idx <- max(1, min(length(x1_range), x1_idx))
        x2_idx <- max(1, min(length(x2_range), x2_idx))
        
        # Use the closest grid point value
        z_vals[i] <- self$zmat[x1_idx, x2_idx]
      }
      
      return(z_vals)
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
