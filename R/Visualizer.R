#' @title Base Visualizer Class
#'
#' @description
#' Base class for all visualizers. Provides a common interface for creating
#' and saving plots across different plotting backends (ggplot2 for 1D/2D, plotly for 3D).
#'
#' @export
Visualizer = R6::R6Class("Visualizer",
  public = list(

    #' @description Set the instance theme (partial override stored separately)
    #' @param theme (`list`) Partial theme created with vistool_theme() or a named list
    #' @return Invisible self
    set_theme = function(theme) {
      assert_vistool_theme(theme)
      # Merge with existing instance overrides, not package defaults
      existing_overrides = if (is.null(private$.theme)) list() else private$.theme
      private$.theme = merge_theme(existing_overrides, theme)
      invisible(self)
    },

    #' @description Get the instance theme (merged with current package default)
    theme = function() {
      inst = if (is.null(private$.theme)) list() else private$.theme
      merge_theme(get_pkg_theme_default(), inst)
    },

    #' @description
    #' Base plot method that sets up common plot settings and resolves layer colors.
    #' This method should be called by all child classes via `super$plot(...)`.
    #' @param theme (`list`)\cr
    #'   Partial theme override for this render; see vistool_theme().
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
    #' @return Invisible self for method chaining (child classes handle actual plot creation).
    plot = function(theme = NULL,
                    plot_title = NULL, plot_subtitle = NULL, x_lab = NULL, y_lab = NULL, z_lab = NULL,
                    x_limits = NULL, y_limits = NULL, z_limits = NULL,
                    show_legend = TRUE, legend_title = NULL, show_title = TRUE) {
      # Validate and store render params
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
      if (!is.null(theme)) assert_vistool_theme(theme)

      # Resolve effective theme and store render params
      base = get_pkg_theme_default()
      inst = if (is.null(private$.theme)) list() else private$.theme
      eff = merge_theme(base, inst)
      eff = merge_theme(eff, theme)
      private$.effective_theme = eff
      # Title size derived if needed
      private$.effective_theme$title_size = if (is.null(eff$title_size)) (eff$text_size + 2) else eff$title_size
      private$.render_params = list(
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        x_lab = x_lab,
        y_lab = y_lab,
        z_lab = z_lab,
        x_limits = x_limits,
        y_limits = y_limits,
        z_limits = z_limits,
        show_legend = show_legend,
        legend_title = legend_title,
        show_title = show_title
      )

      return(invisible(self))
    },

    #' @description
    #' Resolve automatic color assignments in stored layers.
    #' This method should be called by child classes after rendering layers.
    resolve_layer_colors = function() {
      # Initialize layers_to_add if it doesn't exist
      if (is.null(private$.layers_to_add)) {
        private$.layers_to_add = list()
      }

      # Reset color index for consistent color assignment
      private$.color_index = 1

      # Resolve colors in any stored layer specifications
      private$resolve_all_layer_colors()

      return(invisible(self))
    },

    #' @description
    #' Save the plot to a file. The format is determined by the file extension.
    #' @param filename (`character(1)`)\cr
    #'   The filename to save the plot to. The file extension determines the format.
    #' @param width (`numeric(1)`)\cr
    #'   Width of the plot in pixels (for plotly) or inches (for ggplot2).
    #' @param height (`numeric(1)`)\cr
    #'   Height of the plot in pixels (for plotly) or inches (for ggplot2).
    #' @param dpi (`numeric(1)`)\cr
    #'   Resolution for ggplot2 plots (ignored for plotly plots).
    #' @param ... Additional arguments passed to the underlying save function.
    save = function(filename, width = NULL, height = NULL, dpi = 300, ...) {
      checkmate::assert_string(filename)
      checkmate::assert_number(width, null.ok = TRUE)
      checkmate::assert_number(height, null.ok = TRUE)
      checkmate::assert_number(dpi, lower = 1)

      # Use cached last plot if available; otherwise render now
      plot_obj = private$.last_plot
      if (is.null(plot_obj)) {
        # try to render via subclass plot() with current instance theme
        plot_obj = tryCatch(self$plot(), error = function(e) NULL)
      }

      # Check if it's a ggplot2 or plotly object and save accordingly
      if (inherits(plot_obj, "ggplot")) {
        private$save_ggplot(plot_obj, filename, width, height, dpi, ...)
      } else if (inherits(plot_obj, "plotly")) {
        private$save_plotly(plot_obj, filename, width, height, ...)
      } else {
        stop("Unknown plot type. Cannot save plot.")
      }

      invisible(self)
    },

    #' @description
    #' Add points to the plot. This method can be called multiple times to add different sets of points.
    #'
    #' @param points (`data.frame` or `matrix` or `list`)\cr
    #'   The points to add.
    #'   - For 1D: A `data.frame` or `matrix` with one column for x-values, or a numeric vector of x-values. If y-values are not provided, they will be inferred if possible (e.g., for objective functions).
    #'   - For 2D/Surface: A `data.frame` or `matrix` with two columns (x1, x2), or a list of 2-element vectors.
    #' @param color (`character(1)`)\cr
    #'   Color of the points. Use "auto" for automatic color assignment from palette. Default is "auto".
    #' @param size (`numeric(1)`)\cr
    #'   Size of the points. If NULL, uses theme$point_size. Default is NULL.
    #' @param shape (`integer(1)` or `character(1)`)\cr
    #'   Shape of the points. For ggplot2: integer codes (e.g., 19 for solid circle). For plotly: shape names. Default is 19/"circle".
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of the points. If NULL, uses theme$alpha. Default is NULL.
    #' @param annotations (`character`)\cr
    #'   Optional text labels for each point. If provided, must be the same length as the number of points.
    #' @param annotation_size (`numeric(1)`)\cr
    #'   Size of annotation text. If NULL, defaults to text_size - 2 from plot().
    #' @param ordered (`logical(1)`)\cr
    #'   If `TRUE`, draws arrows between consecutive points to indicate order. Default is `FALSE`.
    #' @param arrow_color (`character(1)`)\cr
    #'   Color of arrows when ordered = TRUE. If NULL, uses point color.
    #' @param arrow_size (`numeric(1)`)\cr
    #'   Length/size of arrows when `ordered = TRUE`. Default is 0.3 units in the coordinate system.
    add_points = function(points, color = "auto", size = NULL, shape = 19, alpha = NULL,
                          annotations = NULL, annotation_size = NULL, ordered = FALSE,
                          arrow_color = NULL, arrow_size = 0.3) {
      # Store layer specification
      private$store_layer("points", list(
        points = points, color = color, size = size, shape = shape, alpha = alpha,
        annotations = annotations, annotation_size = annotation_size, ordered = ordered,
        arrow_color = arrow_color, arrow_size = arrow_size
      ))
      invisible(self)
    }
  ),
  private = list(
    .layers_to_add = list(), # General storage for all layer types
    .color_index = 1, # Track next color index for auto assignment
    .theme = NULL, # Instance theme (partial)
    .effective_theme = NULL, # per-render effective theme
    .render_params = NULL, # per-render non-style params
    .last_plot = NULL, # last built plot object

    # save a ggplot2 object
    save_ggplot = function(plot_obj, filename, width, height, dpi, ...) {
      # default dimensions for ggplot2
      if (is.null(width)) width = 10
      if (is.null(height)) height = 6

      ggplot2::ggsave(
        filename = filename,
        plot = plot_obj,
        width = width,
        height = height,
        dpi = dpi,
        ...
      )
    },

    # save a plotly object
    save_plotly = function(plot_obj, filename, width, height, ...) {
      # default dimensions for plotly (in pixels)
      if (is.null(width)) width = 800
      if (is.null(height)) height = 600

      save_image(
        p = plot_obj,
        file = filename,
        width = width,
        height = height,
        ...
      )
    },

    # Helper method to add points to ggplot2 objects
    add_points_to_ggplot = function(plot_obj, visualizer_type = "2D") {
      # Get points layers from the layer system
      points_layers = private$get_layers_by_type("points")
      if (length(points_layers) == 0) {
        return(plot_obj)
      }

      for (point_spec in points_layers) {
        points_data = private$prepare_points_data(point_spec$points, visualizer_type)
        eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme
        size = if (is.null(point_spec$size)) eff$point_size else point_spec$size
        alpha = if (is.null(point_spec$alpha)) eff$alpha else point_spec$alpha

        # Add points layer
        plot_obj = plot_obj + ggplot2::geom_point(
          data = points_data,
          ggplot2::aes(x = x, y = y),
          color = point_spec$color,
          size = size,
          shape = point_spec$shape,
          alpha = alpha,
          inherit.aes = FALSE
        )

        # Add annotations if provided
        if (!is.null(point_spec$annotations)) {
          # Use annotation_size if provided, otherwise default to smaller text
          ann_size = if (!is.null(point_spec$annotation_size)) point_spec$annotation_size else max(3, eff$text_size - 2)

          plot_obj = plot_obj + ggplot2::geom_text(
            data = cbind(points_data, label = point_spec$annotations),
            ggplot2::aes(x = x, y = y, label = label),
            color = point_spec$color,
            size = ann_size,
            vjust = -0.5,
            inherit.aes = FALSE
          )
        }

        # Add ordered path with arrows if requested
        if (point_spec$ordered && nrow(points_data) > 1) {
          # Calculate direction vectors and create shorter arrows
          for (i in 1:(nrow(points_data) - 1)) {
            x1 = points_data$x[i]
            y1 = points_data$y[i]
            x2 = points_data$x[i + 1]
            y2 = points_data$y[i + 1]

            # Calculate direction vector and normalize it
            dx = x2 - x1
            dy = y2 - y1
            dist = sqrt(dx^2 + dy^2)

            if (dist > 0) {
              # Normalize direction vector
              dx_norm = dx / dist
              dy_norm = dy / dist

              # Calculate arrow endpoints based on arrow_size
              arrow_size = point_spec$arrow_size
              arrow_x2 = x1 + dx_norm * arrow_size
              arrow_y2 = y1 + dy_norm * arrow_size

              # Create arrow data frame
              arrow_data = data.frame(
                x = x1,
                y = y1,
                xend = arrow_x2,
                yend = arrow_y2
              )

              # Add arrow segment
              # Use the same color as points if arrow_color is not specified
              arrow_color = if (is.null(point_spec$arrow_color)) point_spec$color else point_spec$arrow_color

              plot_obj = plot_obj + ggplot2::geom_segment(
                data = arrow_data,
                ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                color = arrow_color,
                alpha = alpha * 0.7,
                arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"), type = "closed"),
                inherit.aes = FALSE
              )
            }
          }
        }
      }

      return(plot_obj)
    },

    # Helper method to add points to plotly objects
    add_points_to_plotly = function(plot_obj, visualizer_type = "surface") {
      points_layers = private$get_layers_by_type("points")
      if (length(points_layers) == 0) {
        return(plot_obj)
      }

      for (point_spec in points_layers) {
        points_data = private$prepare_points_data(point_spec$points, visualizer_type)
        eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme
        size = if (is.null(point_spec$size)) eff$point_size else point_spec$size
        alpha = if (is.null(point_spec$alpha)) eff$alpha else point_spec$alpha

        if (visualizer_type == "surface") {
          # For 3D surface plots, need z values
          if (!"z" %in% names(points_data)) {
            # Try to infer z values if the visualizer has a way to evaluate the function
            points_data$z = private$infer_z_values(points_data)
          }

          # Add 3D scatter trace
          plot_obj = plot_obj %>% add_trace(
            x = points_data$x,
            y = points_data$y,
            z = points_data$z,
            type = "scatter3d",
            mode = "markers",
            marker = list(
              color = point_spec$color,
              size = size,
              opacity = alpha
            ),
            name = "Added Points",
            showlegend = FALSE
          )

          # Add annotations if provided (3D text)
          if (!is.null(point_spec$annotations)) {
            for (i in seq_along(point_spec$annotations)) {
              plot_obj = plot_obj %>% add_trace(
                x = points_data$x[i],
                y = points_data$y[i],
                z = points_data$z[i],
                type = "scatter3d",
                mode = "text",
                text = point_spec$annotations[i],
                textfont = list(
                  color = point_spec$color,
                  size = if (!is.null(point_spec$annotation_size)) point_spec$annotation_size else (eff$text_size + 1)
                ),
                showlegend = FALSE
              )
            }
          }

          # Add ordered path if requested
          if (point_spec$ordered && nrow(points_data) > 1) {
            # For 3D plots, we'll use lines but with shorter segments
            for (i in 1:(nrow(points_data) - 1)) {
              x1 = points_data$x[i]
              y1 = points_data$y[i]
              z1 = points_data$z[i]
              x2 = points_data$x[i + 1]
              y2 = points_data$y[i + 1]
              z2 = points_data$z[i + 1]

              # Calculate direction vector and normalize it
              dx = x2 - x1
              dy = y2 - y1
              dz = z2 - z1
              dist = sqrt(dx^2 + dy^2 + dz^2)

              if (dist > 0) {
                # Normalize direction vector
                dx_norm = dx / dist
                dy_norm = dy / dist
                dz_norm = dz / dist

                # Calculate arrow endpoints based on arrow_size
                arrow_size = point_spec$arrow_size
                arrow_x2 = x1 + dx_norm * arrow_size
                arrow_y2 = y1 + dy_norm * arrow_size
                arrow_z2 = z1 + dz_norm * arrow_size

                # Add arrow segment
                plot_obj = plot_obj %>% add_trace(
                  x = c(x1, arrow_x2),
                  y = c(y1, arrow_y2),
                  z = c(z1, arrow_z2),
                  type = "scatter3d",
                  mode = "lines",
                  line = list(
                    color = if (is.null(point_spec$arrow_color)) point_spec$color else point_spec$arrow_color,
                    width = 6
                  ),
                  name = "Arrow",
                  showlegend = FALSE
                )
              }
            }
          }
        } else {
          # For 2D contour plots
          plot_obj = plot_obj %>% add_trace(
            x = points_data$x,
            y = points_data$y,
            type = "scatter",
            mode = "markers",
            marker = list(
              color = point_spec$color,
              size = size,
              opacity = alpha
            ),
            name = "Added Points",
            showlegend = FALSE
          )

          # Add annotations if provided (2D text)
          if (!is.null(point_spec$annotations)) {
            plot_obj = plot_obj %>% plotly::add_annotations(
              x = points_data$x,
              y = points_data$y,
              text = point_spec$annotations,
              showarrow = FALSE,
              font = list(
                color = point_spec$color,
                size = if (!is.null(point_spec$annotation_size)) point_spec$annotation_size else (eff$text_size + 1)
              )
            )
          }

          # Add ordered path if requested
          if (point_spec$ordered && nrow(points_data) > 1) {
            # For 2D plots, use shorter line segments
            for (i in 1:(nrow(points_data) - 1)) {
              x1 = points_data$x[i]
              y1 = points_data$y[i]
              x2 = points_data$x[i + 1]
              y2 = points_data$y[i + 1]

              # Calculate direction vector and normalize it
              dx = x2 - x1
              dy = y2 - y1
              dist = sqrt(dx^2 + dy^2)

              if (dist > 0) {
                # Normalize direction vector
                dx_norm = dx / dist
                dy_norm = dy / dist

                # Calculate arrow endpoints based on arrow_size
                arrow_size = point_spec$arrow_size
                arrow_x2 = x1 + dx_norm * arrow_size
                arrow_y2 = y1 + dy_norm * arrow_size

                # Add arrow segment
                plot_obj = plot_obj %>% add_trace(
                  x = c(x1, arrow_x2),
                  y = c(y1, arrow_y2),
                  type = "scatter",
                  mode = "lines",
                  line = list(
                    color = if (is.null(point_spec$arrow_color)) point_spec$color else point_spec$arrow_color,
                    width = 4
                  ),
                  name = "Arrow",
                  showlegend = FALSE
                )
              }
            }
          }
        }
      }

      return(plot_obj)
    },

    # Helper method to prepare points data into consistent format
    prepare_points_data = function(points, visualizer_type) {
      if (is.null(points)) {
        stop("Points cannot be NULL")
      }

      # Convert different input formats to data.frame
      if (is.numeric(points) && visualizer_type == "1D") {
        # For 1D: numeric vector of x values
        points_data = data.frame(x = points, y = NA_real_)
      } else if (is.matrix(points) || is.data.frame(points)) {
        points_data = as.data.frame(points)
        # Ensure consistent column names
        if (ncol(points_data) == 1 && visualizer_type == "1D") {
          names(points_data) = "x"
          points_data$y = NA_real_
        } else if (ncol(points_data) >= 2) {
          names(points_data)[1:2] = c("x", "y")
          if (ncol(points_data) >= 3) {
            names(points_data)[3] = "z"
          }
        }
      } else if (is.list(points) && !is.data.frame(points)) {
        # List of vectors - convert to data.frame
        if (all(sapply(points, length) == 2)) {
          points_data = data.frame(
            x = sapply(points, function(p) p[1]),
            y = sapply(points, function(p) p[2])
          )
        } else {
          stop("List elements must be vectors of length 2 for 2D points")
        }
      } else {
        stop("Unsupported points format")
      }

      # Validate dimensions
      if (visualizer_type == "1D" && !"x" %in% names(points_data)) {
        stop("1D visualizers require x coordinates")
      } else if (visualizer_type %in% c("2D", "surface") && (!("x" %in% names(points_data)) || !("y" %in% names(points_data)))) {
        stop("2D/Surface visualizers require x and y coordinates")
      }

      return(points_data)
    },

    # Helper method to infer z values for surface plots
    infer_z_values = function(points_data) {
      # This is a placeholder - subclasses should override this method
      # to provide function evaluation capabilities
      warning("Cannot infer z values - returning zeros. Consider providing z values explicitly or overriding infer_z_values method.")
      return(rep(0, nrow(points_data)))
    },

    # Resolve colors in all stored layers
    resolve_all_layer_colors = function() {
      # Initialize layers_to_add if it doesn't exist
      if (is.null(private$.layers_to_add)) {
        private$.layers_to_add = list()
      }

      if (length(private$.layers_to_add) == 0) {
        return()
      }

      for (i in seq_along(private$.layers_to_add)) {
        layer = private$.layers_to_add[[i]]

        # For training_data layers, handle "auto" colors context-aware for classification vs regression
        if (layer$type == "training_data") {
          # Resolve colors outside of style normally
          if ("spec" %in% names(layer)) {
            for (spec_name in names(layer$spec)) {
              if (spec_name != "style") {
                layer$spec[[spec_name]] = private$resolve_colors_recursive(layer$spec[[spec_name]])
              }
            }

            # For style, check if we have task information to determine if it's classification
            if ("style" %in% names(layer$spec) && !is.null(self$task)) {
              is_classification = self$task$task_type == "classif"

              # For regression tasks, resolve "auto" colors in style to avoid ggplot2 errors
              # For classification tasks, preserve "auto" for class-aware handling
              if (!is_classification) {
                layer$spec$style = private$resolve_colors_recursive(layer$spec$style)
              }
              # For classification, leave style$color as "auto" if present
            } else {
              # If no task information available, resolve all colors to be safe
              layer$spec$style = private$resolve_colors_recursive(layer$spec$style)
            }
          }
        } else {
          # For other layer types, recursively resolve any "auto" colors in the layer specification
          layer = private$resolve_colors_recursive(layer)
        }

        # Update the stored layer
        private$.layers_to_add[[i]] = layer
      }
    },

    # Recursively resolve colors in a data structure
    resolve_colors_recursive = function(obj) {
      if (is.list(obj)) {
        for (name in names(obj)) {
          if (identical(obj[[name]], "auto")) {
            obj[[name]] = private$get_auto_color_with_palette()
          } else if (is.list(obj[[name]])) {
            obj[[name]] = private$resolve_colors_recursive(obj[[name]])
          }
        }
      }
      return(obj)
    },

    # Get auto color using current effective theme
    get_auto_color_with_palette = function() {
      eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme
      color_palette = if (is.null(eff$palette)) "viridis" else eff$palette

      # Get color from discrete palette based on the selected color_palette
      color = get_vistool_color(private$.color_index, "discrete", base_palette = color_palette)

      # Increment color index for next use
      private$.color_index = private$.color_index + 1

      return(color)
    },

    # Store a layer specification that will be resolved at plot time
    store_layer = function(layer_type, layer_spec) {
      # Initialize layers_to_add if it doesn't exist
      if (is.null(private$.layers_to_add)) {
        private$.layers_to_add = list()
      }

      private$.layers_to_add = c(
        private$.layers_to_add,
        list(list(type = layer_type, spec = layer_spec))
      )
    },

    # Get a stored layer by type (returns the latest one if multiple exist)
    get_layer = function(layer_type) {
      # Initialize layers_to_add if it doesn't exist
      if (is.null(private$.layers_to_add)) {
        private$.layers_to_add = list()
      }

      if (length(private$.layers_to_add) == 0) {
        return(NULL)
      }

      for (i in rev(seq_along(private$.layers_to_add))) {
        layer = private$.layers_to_add[[i]]
        if (layer$type == layer_type) {
          return(layer$spec)
        }
      }
      return(NULL)
    },

    # Get all layers of a specific type
    get_layers_by_type = function(layer_type) {
      # Initialize layers_to_add if it doesn't exist
      if (is.null(private$.layers_to_add)) {
        private$.layers_to_add = list()
      }

      if (length(private$.layers_to_add) == 0) {
        return(list())
      }

      layers = list()
      for (layer in private$.layers_to_add) {
        if (layer$type == layer_type) {
          layers = c(layers, list(layer$spec))
        }
      }
      return(layers)
    },

    # Initialize a base ggplot2 object (for 1D/2D visualizers)
    init_ggplot = function(data, x_col = "x", y_col = "y") {
      # Create base ggplot object
      if (missing(data) || is.null(data)) {
        p = ggplot2::ggplot()
      } else {
        aes_mapping = do.call(ggplot2::aes, setNames(list(as.name(x_col), as.name(y_col)), c("x", "y")))
        p = ggplot2::ggplot(data = data, mapping = aes_mapping)
      }

      return(p)
    },

    # Apply theme and styling to ggplot2 object
    apply_ggplot_theme = function(plot_obj, text_size = 11, title_size = NULL, theme = "minimal",
                                  background = "white", show_grid = TRUE, grid_color = "gray90") {
      # derive sizes
      if (is.null(title_size)) title_size = text_size + 2

      # Apply theme
      theme_func = switch(theme,
        "minimal" = ggplot2::theme_minimal,
        "bw" = ggplot2::theme_bw,
        "classic" = ggplot2::theme_classic,
        "gray" = ggplot2::theme_gray,
        "grey" = ggplot2::theme_grey,
        "light" = ggplot2::theme_light,
        "dark" = ggplot2::theme_dark,
        "void" = ggplot2::theme_void,
        ggplot2::theme_minimal # default fallback
      )

      plot_obj = plot_obj + theme_func(base_size = text_size)

      # Apply additional theme customizations
      plot_obj = plot_obj + ggplot2::theme(
        plot.title = ggplot2::element_text(size = title_size, hjust = 0.5),
        plot.background = ggplot2::element_rect(fill = background, color = NA),
        panel.background = ggplot2::element_rect(fill = background, color = NA)
      )

      # Handle grid display
      if (!show_grid) {
        plot_obj = plot_obj + ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
      } else if (!is.null(grid_color)) {
        plot_obj = plot_obj + ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = grid_color),
          panel.grid.minor = ggplot2::element_line(color = grid_color, linewidth = 0.5)
        )
      }

      return(plot_obj)
    },

    # Apply color scales to ggplot2 object
    apply_ggplot_color_scale = function(plot_obj, color_palette = "viridis", scale_type = "fill", discrete = FALSE) {
      if (scale_type == "fill") {
        if (discrete) {
          if (color_palette == "viridis") {
            plot_obj = plot_obj + ggplot2::scale_fill_viridis_d()
          } else if (color_palette == "plasma") {
            plot_obj = plot_obj + ggplot2::scale_fill_viridis_d(option = "plasma")
          } else if (color_palette == "grayscale") {
            plot_obj = plot_obj + ggplot2::scale_fill_grey()
          }
        } else {
          if (color_palette == "viridis") {
            plot_obj = plot_obj + ggplot2::scale_fill_viridis_c()
          } else if (color_palette == "plasma") {
            plot_obj = plot_obj + ggplot2::scale_fill_viridis_c(option = "plasma")
          } else if (color_palette == "grayscale") {
            plot_obj = plot_obj + ggplot2::scale_fill_gradient(low = "black", high = "white")
          }
        }
      } else if (scale_type == "color") {
        if (discrete) {
          if (color_palette == "viridis") {
            plot_obj = plot_obj + ggplot2::scale_color_viridis_d()
          } else if (color_palette == "plasma") {
            plot_obj = plot_obj + ggplot2::scale_color_viridis_d(option = "plasma")
          } else if (color_palette == "grayscale") {
            plot_obj = plot_obj + ggplot2::scale_color_grey()
          }
        } else {
          if (color_palette == "viridis") {
            plot_obj = plot_obj + ggplot2::scale_color_viridis_c()
          } else if (color_palette == "plasma") {
            plot_obj = plot_obj + ggplot2::scale_color_viridis_c(option = "plasma")
          } else if (color_palette == "grayscale") {
            plot_obj = plot_obj + ggplot2::scale_color_gradient(low = "black", high = "white")
          }
        }
      }

      return(plot_obj)
    },

    # Helper method to initialize 1D plots with common styling
    gg_init_1d = function(data_structure, geom_layer_func) {
      eff = private$.effective_theme
      rp = private$.render_params

      # Create base data for the function line
      plot_data = data.frame(
        x = data_structure$coordinates$x1,
        y = data_structure$coordinates$y
      )

      # Create base ggplot
      private$.plot = private$init_ggplot(plot_data, "x", "y")

      # Add the visualization layer (line for functions, points for objectives)
      private$.plot = geom_layer_func(private$.plot, plot_data, eff)

      # Apply common styling
      private$gg_apply_labels_limits_theme(data_structure)
    },

    # Helper method to initialize 2D plots with common styling
    gg_init_2d = function(data_structure, geom_layer_func) {
      eff = private$.effective_theme
      rp = private$.render_params

      # Create base data for the filled contour
      plot_data = data.frame(
        x1 = data_structure$coordinates$x1,
        x2 = data_structure$coordinates$x2,
        y = data_structure$coordinates$y
      )

      # Create base ggplot
      private$.plot = private$init_ggplot(plot_data, "x1", "x2")

      # Add the visualization layer (raster for both models and objectives)
      private$.plot = geom_layer_func(private$.plot, plot_data, eff)

      # Apply common styling
      private$gg_apply_labels_limits_theme(data_structure, is_2d = TRUE)
    },

    # Helper method to apply labels, limits, and theme styling
    gg_apply_labels_limits_theme = function(data_structure, is_2d = FALSE) {
      eff = private$.effective_theme
      rp = private$.render_params

      # Apply theme and styling
      private$.plot = private$apply_ggplot_theme(private$.plot, eff$text_size, eff$title_size, eff$theme, eff$background, eff$show_grid, eff$grid_color)

      # Determine labels
      title_text = if (!is.null(rp$plot_title)) rp$plot_title else data_structure$labels$title
      x_text = if (!is.null(rp$x_lab)) rp$x_lab else data_structure$labels$x1
      y_text = if (!is.null(rp$y_lab)) rp$y_lab else if (is_2d) data_structure$labels$x2 else data_structure$labels$y

      # Build labels list
      labels_list = list(
        title = if (rp$show_title) title_text else NULL,
        subtitle = rp$plot_subtitle,
        x = x_text,
        y = y_text
      )

      # Add fill label for 2D plots
      if (is_2d) {
        fill_text = if (!is.null(rp$legend_title)) rp$legend_title else data_structure$labels$y
        labels_list$fill = fill_text
      }

      # Add labels conditionally
      private$.plot = private$.plot + do.call(ggplot2::labs, labels_list)

      # Apply axis limits
      if (!is.null(rp$x_limits)) {
        private$.plot = private$.plot + ggplot2::xlim(rp$x_limits)
      }
      if (!is.null(rp$y_limits)) {
        private$.plot = private$.plot + ggplot2::ylim(rp$y_limits)
      }

      # Apply legend settings: theme drives position; "none" disables legend
      legend_pos = if (is.null(eff$legend_position)) "right" else eff$legend_position
      if (!rp$show_legend || identical(legend_pos, "none")) {
        private$.plot = private$.plot + ggplot2::theme(legend.position = "none")
      } else {
        private$.plot = private$.plot + ggplot2::theme(legend.position = legend_pos)
      }
    }
  )
)
