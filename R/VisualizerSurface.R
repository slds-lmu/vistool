#' @title Visualize 2D functions as interactive surfaces
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
VisualizerSurface = R6::R6Class("VisualizerSurface",
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

    # theme-driven styling is resolved at plot(); keep fields for labels only

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
    initialize = function(grid, zmat, plot_lab = NULL, x1_lab = "x1", x2_lab = "x2", z_lab = "y") {
      self$grid = checkmate::assert_list(grid)
      self$zmat = checkmate::assert_matrix(zmat)
      self$plot_lab = checkmate::assert_character(plot_lab, null.ok = TRUE)
      self$x1_lab = checkmate::assert_character(x1_lab)
      self$x2_lab = checkmate::assert_character(x2_lab)
      self$z_lab = checkmate::assert_character(z_lab)
      return(invisible(self))
    },

    #' @description
    #' Initialize the plot as 2D contour.
    #' This method is called automatically by plot() and should not be called directly.
    #'
    #' @param opacity (`numeric(1)`)\cr
    #'   Opacity of the contour plot (0-1). If NULL, uses theme default.
    #' @param colorscale (`list`)\cr
    #'   Color scale for the contour plot. If NULL or "auto", uses theme palette.
    #' @param show_title (`logical(1)`)\cr
    #'   Whether to show the plot title. Default is TRUE.
    #' @template param_dots_trace
    init_layer_contour = function(opacity = NULL, colorscale = NULL, show_title = TRUE, ...) {
      checkmate::assert_number(opacity, lower = 0, upper = 1, null.ok = TRUE)
      # Resolve colorscale if it's "auto"
      if (is.null(colorscale) || (is.character(colorscale) && colorscale == "auto")) {
        eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme
        colorscale = get_continuous_colorscale(eff$palette)
      }
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
          opacity = if (is.null(opacity)) (if (is.null(private$.effective_theme)) get_pkg_theme_default()$alpha else private$.effective_theme$alpha) else opacity,
          colorscale = colorscale,
          ...
        ) %>%
        plotly::layout(
          title = if (show_title) self$plot_lab else NULL,
          xaxis = list(title = self$x1_lab),
          yaxis = list(title = self$x2_lab)
        )

      if (!private$.freeze_plot) { # Used in animate to not overwrite the
        private$.opts = list() # plot over and over again when calling
        private$.layer_arrow = list() # `$init_layerXXX`.
      }

      return(invisible(self))
    },

    #' @description
    #' Initialize the plot as 3D surface.
    #' This method is called automatically by plot() and should not be called directly.
    #'
    #' @param opacity (`numeric(1)`)\cr
    #'   Opacity of the surface plot (0-1). If NULL, uses theme default.
    #' @param colorscale (`list`)\cr
    #'   Color scale for the surface plot. If NULL or "auto", uses theme palette.
    #' @param show_title (`logical(1)`)\cr
    #'   Whether to show the plot title. Default is TRUE.
    #' @template param_dots_trace
    init_layer_surface = function(opacity = NULL, colorscale = NULL, show_title = TRUE, ...) {
      checkmate::assert_number(opacity, lower = 0, upper = 1, null.ok = TRUE)
      # Resolve colorscale if it's "auto"
      if (is.null(colorscale) || (is.character(colorscale) && colorscale == "auto")) {
        eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme
        colorscale = get_continuous_colorscale(eff$palette)
      }
      checkmate::assert_list(colorscale)
      checkmate::assert_flag(show_title)

      private$.vbase = c(as.list(environment()), list(...))
      private$.layer_primary = "surface"

      llp = list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)

      # Check if contours layer is stored and include it in the base surface
      contours_layer = private$get_layer("contours")

      # Build trace arguments
      trace_args = list(
        name = self$plot_lab,
        showlegend = FALSE,
        showscale = FALSE,
        x = llp$x,
        y = llp$y,
        z = t(llp$z),
        type = "surface",
        opacity = if (is.null(opacity)) (if (is.null(private$.effective_theme)) get_pkg_theme_default()$alpha else private$.effective_theme$alpha) else opacity,
        colorscale = colorscale
      )

      # Add contours if available
      if (!is.null(contours_layer)) {
        trace_args$contours = contours_layer$contours
        # Add contours args to the base trace, allowing args to override base values
        if (!is.null(contours_layer$args)) {
          trace_args = utils::modifyList(trace_args, contours_layer$args)
        }
      }

      # Add additional arguments
      trace_args = c(trace_args, list(...))

      plot_obj = plot_ly()
      private$.plot = do.call(add_trace, c(list(plot_obj), trace_args)) %>%
        plotly::layout(
          title = if (show_title) self$plot_lab else NULL,
          scene = list(
            xaxis = list(title = self$x1_lab),
            yaxis = list(title = self$x2_lab),
            zaxis = list(title = self$z_lab)
          )
        )

      if (!private$.freeze_plot) { # Used in animate to not overwrite the plot over and over again.
        private$.opts = list()
        private$.layer_arrow = list()
      }

      return(invisible(self))
    },

    #' @description Set the layout of the plotly plot.
    #' This method is used internally by plot(layout = ...) and should not be called directly.
    #' @param ... Layout options directly passed to `layout(...)`.
    set_layout = function(...) {
      # persist provided layout; apply immediately only if a plot exists
      private$.layout = list(...)
      if (!is.null(private$.plot)) {
        private$.plot = do.call(plotly::layout, c(list(private$.plot), private$.layout))
      }

      return(invisible(self))
    },

    #' @description Set the view for a 3D plot.
    #' This method is used internally by plot(scene = ...) and should not be called directly.
    #' @param x (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param y (`numeric(1)`) The view from which the "camera looks down" to the plot.
    #' @param z (`numeric(1)`) The view from which the "camera looks down" to the plot.
    set_scene = function(x, y, z) {
      checkmate::assert_number(x)
      checkmate::assert_number(y)
      checkmate::assert_number(z)

      # If a plot exists and is not a surface, disallow. If no plot yet, just persist.
      if (!is.null(private$.layer_primary) && private$.layer_primary != "surface") {
        stop("Scene can only be set for `surface` plots")
      }

      # persist camera eye for re-application in plot()
      private$.scene_eye = list(x = x, y = y, z = z)

      # if a plot already exists, update it immediately; otherwise it will be applied in plot()
      if (!is.null(private$.plot)) {
        private$.plot = private$.plot %>%
          plotly::layout(scene = list(camera = list(eye = private$.scene_eye)))
      }

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
      checkmate::assert_list(contours, null.ok = TRUE)

      # Handle contours: custom contours or default z-projected contours
      if (is.null(contours)) {
        # Use default z-projected contours
        contours_final = list(
          z = list(
            show = TRUE,
            project = list(z = TRUE),
            usecolormap = TRUE
          )
        )
      } else {
        # Use provided custom contours
        contours_final = contours
      }

      # Store contours specification for deferred rendering
      private$store_layer("contours", list(
        contours = contours_final,
        args = list(...)
      ))

      return(invisible(self))
    },

    #' @description
    #' Create and return the plotly plot with model-specific layers.
    #' @param flatten (`logical(1)`)\cr
    #'   If TRUE, display as 2D contour plot. If FALSE, display as 3D surface plot.
    #' @param layout (`list()`)\cr
    #'   Layout options passed to `plotly::layout()`. Ignored by other visualizer types. Default is NULL.
    #' @param scene (`list()`)\cr
    #'   Scene options for 3D plots. Ignored by other visualizer types. Default is NULL.
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A plotly object.
    plot = function(flatten = FALSE, layout = NULL, scene = NULL, ...) {
      checkmate::assert_flag(flatten)
      checkmate::assert_list(layout, null.ok = TRUE)
      checkmate::assert_list(scene, null.ok = TRUE)

      # Validate scene parameter is only used for surface plots
      if (!is.null(scene) && flatten) {
        stop("Scene parameter can only be used for surface plots (flatten = FALSE)")
      }

      # Store VisualizerSurface-specific parameters before calling super$plot()
      private$.surface_plot_settings = list(
        flatten = flatten,
        layout = layout,
        scene = scene
      )

      # Call parent method for common parameter validation and setup
      super$plot(...)

      # Effective theme and params
      eff = private$.effective_theme
      rp = private$.render_params
      settings = c(list(), private$.surface_plot_settings)
      self$resolve_layer_colors()

      plot_colorscale = get_continuous_colorscale(eff$palette)

      # Always reinitialize the base plot to prevent accumulating traces
      # This ensures clean plots on repeated plot() calls unless in freeze_plot mode
      if (!private$.freeze_plot) {
        if (settings$flatten) {
          self$init_layer_contour(colorscale = plot_colorscale, opacity = eff$alpha, show_title = rp$show_title)
        } else {
          self$init_layer_surface(colorscale = plot_colorscale, opacity = eff$alpha, show_title = rp$show_title)
        }
      }

      # apply text size to plotly layout
      if (!is.null(private$.plot)) {
        # Determine final labels
        final_title = if (rp$show_title) {
          if (!is.null(rp$plot_title)) rp$plot_title else self$plot_lab
        } else {
          "" # Empty string instead of NULL for plotly
        }
        final_x_lab = if (!is.null(rp$x_lab)) rp$x_lab else self$x1_lab
        final_y_lab = if (!is.null(rp$y_lab)) rp$y_lab else self$x2_lab
        final_z_lab = if (!is.null(rp$z_lab)) rp$z_lab else self$z_lab

        title_res = private$format_label(final_title, "title", "plotly")
        x_res = private$format_label(final_x_lab, "x", "plotly")
        y_res = private$format_label(final_y_lab, "y", "plotly")
        z_res = private$format_label(final_z_lab, "z", "plotly")

        legend_layout = NULL
        if (!is.null(rp$legend_title)) {
          legend_res = private$format_label(rp$legend_title, "legend", "plotly")
          legend_layout = list(title = list(text = legend_res$values, font = list(size = eff$text_size)))
        }

        # Determine legend visibility from theme and render params
        legend_pos = if (is.null(eff$legend_position)) "right" else eff$legend_position
        legend_on = isTRUE(rp$show_legend) && !identical(legend_pos, "none")

        if (private$.layer_primary == "surface") {
          # For 3D surface plots
          private$.plot = private$.plot %>%
            plotly::layout(
              title = list(text = title_res$values, font = list(size = eff$title_size)),
              scene = list(
                xaxis = list(
                  title = list(text = x_res$values, font = list(size = eff$text_size)),
                  range = rp$x_limits
                ),
                yaxis = list(
                  title = list(text = y_res$values, font = list(size = eff$text_size)),
                  range = rp$y_limits
                ),
                zaxis = list(
                  title = list(text = z_res$values, font = list(size = eff$text_size)),
                  range = rp$z_limits
                )
              ),
              showlegend = legend_on,
              legend = if (!is.null(legend_layout) && legend_on) legend_layout else NULL
            )
        } else {
          # For 2D contour plots
          private$.plot = private$.plot %>%
            plotly::layout(
              title = list(text = title_res$values, font = list(size = eff$title_size)),
              xaxis = list(
                title = list(text = x_res$values, font = list(size = eff$text_size)),
                range = rp$x_limits
              ),
              yaxis = list(
                title = list(text = y_res$values, font = list(size = eff$text_size)),
                range = rp$y_limits
              ),
              showlegend = legend_on,
              legend = if (!is.null(legend_layout) && legend_on) legend_layout else NULL
            )
        }
      }

      # Apply additional layout options if provided; otherwise re-apply any
      # persisted layout set previously via set_layout().
      if (!is.null(settings$layout)) {
        private$.plot = do.call(plotly::layout, c(list(private$.plot), settings$layout))
      } else if (length(private$.layout)) {
        private$.plot = do.call(plotly::layout, c(list(private$.plot), private$.layout))
      }

      # Apply scene options if provided (only for surface plots). If not provided,
      # use any camera set previously via set_scene().
      if (private$.layer_primary == "surface") {
        if (!is.null(settings$scene)) {
          private$.plot = private$.plot %>% plotly::layout(scene = settings$scene)
        } else if (!is.null(private$.scene_eye)) {
          private$.plot = private$.plot %>% plotly::layout(scene = list(camera = list(eye = private$.scene_eye)))
        }
      }

      # Add points from add_points() method
      if (private$.layer_primary == "surface") {
        private$.plot = private$add_points_to_plotly(private$.plot, "surface")
      } else {
        private$.plot = private$add_points_to_plotly(private$.plot, "contour")
      }

      annotation_ranges = list(
        x = self$grid$x1,
        y = self$grid$x2,
        z = as.numeric(self$zmat)
      )
      dim_label = if (private$.layer_primary == "surface") "3d" else "2d"
      private$.plot = private$add_annotations_to_plotly(private$.plot, dim_label, annotation_ranges)

      private$.plot = private$ensure_mathjax_dependency(private$.plot)

      private$.last_plot = private$.plot
      return(private$.plot)
    }
  ),
  private = list(
    # @field .layer_primary (`character(1)`) The id of the primary layer. Used to determine
    # the trace setup.
    .layer_primary = NULL,

    # @field .layer_arrow (`list()`) Arguments passed to `$add_arrow_layer()` to reconstruct the plot for animations.
    .layer_arrow = list(),

    # @field .plot (`plot_ly()`) The plot.
    .plot = NULL,

    # @field .opts (`list(Optimizer)`) List of optimizers used to add traces. Each `$init_layerXXX()`
    # resets this list. An optimizer is added after each call to `$add_optimization_trace()`.
    # this private field is exclusively used to create animations with `$animate()`.
    .opts = list(),
    .vbase = list(),
    .layout = list(),

    # @field .freeze_plot (`logical(1)`) Indicator whether to freeze saving the plot elements.
    .freeze_plot = FALSE,

    # @field .surface_plot_settings (`list`) Store VisualizerSurface-specific plot settings
    .surface_plot_settings = list(),

    # Persisted camera eye set via set_scene(); applied on plot() when present
    .scene_eye = NULL,

    # Initialize default surface plot (called automatically by plot())
    .init_default_plot = function() {
      self$init_layer_surface()
    },
    check_init = function() {
      if (is.null(private$.plot)) {
        private$.init_default_plot()
      }
      return(invisible(TRUE))
    },
    check_input = function(x) {
      if (private$.layer_primary == "surface") {
        return(checkmate::assertNumeric(x, len = 3L))
      }
      if (private$.layer_primary == "contour") {
        return(checkmate::assertNumeric(x, len = 3L))
      }
      stop("Error in `$check_input()`")
    },

    # Override infer_z_values to use the surface's zmat
    infer_z_values = function(points_data) {
      # Use bilinear interpolation to estimate z values from the surface
      x_vals = points_data$x
      y_vals = points_data$y

      # Get grid ranges
      x1_range = self$grid$x1
      x2_range = self$grid$x2

      z_vals = numeric(length(x_vals))

      for (i in seq_along(x_vals)) {
        x = x_vals[i]
        y = y_vals[i]

        # Find surrounding grid points for bilinear interpolation
        x1_indices = findInterval(x, x1_range)
        x2_indices = findInterval(y, x2_range)

        # Clamp to valid grid boundaries
        x1_low = max(1, min(length(x1_range) - 1, x1_indices))
        x1_high = x1_low + 1
        x2_low = max(1, min(length(x2_range) - 1, x2_indices))
        x2_high = x2_low + 1

        # Handle edge cases where point is outside grid
        if (x1_low < 1 || x1_high > length(x1_range) ||
          x2_low < 1 || x2_high > length(x2_range)) {
          # Fall back to nearest neighbor for points outside grid
          x1_idx = which.min(abs(x1_range - x))
          x2_idx = which.min(abs(x2_range - y))
          x1_idx = max(1, min(length(x1_range), x1_idx))
          x2_idx = max(1, min(length(x2_range), x2_idx))
          z_vals[i] = self$zmat[x1_idx, x2_idx]
        } else {
          # Perform bilinear interpolation
          x1_low_val = x1_range[x1_low]
          x1_high_val = x1_range[x1_high]
          x2_low_val = x2_range[x2_low]
          x2_high_val = x2_range[x2_high]

          # Get the four corner z values
          z11 = self$zmat[x1_low, x2_low]
          z12 = self$zmat[x1_low, x2_high]
          z21 = self$zmat[x1_high, x2_low]
          z22 = self$zmat[x1_high, x2_high]

          # Calculate interpolation weights
          if (x1_high_val == x1_low_val) {
            wx = 0  # Avoid division by zero
          } else {
            wx = (x - x1_low_val) / (x1_high_val - x1_low_val)
          }

          if (x2_high_val == x2_low_val) {
            wy = 0  # Avoid division by zero
          } else {
            wy = (y - x2_low_val) / (x2_high_val - x2_low_val)
          }

          # Bilinear interpolation formula
          z_vals[i] = z11 * (1 - wx) * (1 - wy) +
            z21 * wx * (1 - wy) +
            z12 * (1 - wx) * wy +
            z22 * wx * wy
        }
      }

      return(z_vals)
    }
  )
)
