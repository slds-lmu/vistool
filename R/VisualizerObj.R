#' @title Visualize Objective (Unified 1D/2D)
#'
#' @description
#' This class provides a unified interface for visualizing objective functions
#' and optimization traces on both 1D and 2D objectives. It automatically detects 
#' the dimensionality and creates appropriate visualizations using ggplot2.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
VisualizerObj = R6::R6Class("VisualizerObj",
  inherit = Visualizer,
  public = list(

    #' @template field_objective
    objective = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_objective
    #' @param x1_limits (`numeric(2)`)\cr
    #'   Limits for the first dimension. For 1D objectives, this controls the x-axis range.
    #'   If NULL, will be determined from objective bounds.
    #' @param x2_limits (`numeric(2)`)\cr
    #'   Limits for the second dimension (2D objectives only). Ignored for 1D objectives.
    #'   If NULL, will be determined from objective bounds.
    #' @template param_padding
    #' @template param_n_points
    #' @param type (`character(1)`)\cr
    #'   Optional visualization type ("1d" or "2d") to override automatic dimension detection.
    #'   Useful when objective dimensions are unknown but visualization type is explicitly specified.
    initialize = function(objective, x1_limits = NULL, x2_limits = NULL, 
                         padding = 0, n_points = 100L, type = NULL) {
      # Validate inputs
      self$objective = checkmate::assert_r6(objective, "Objective")
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_number(padding, lower = 0)
      checkmate::assert_count(n_points)
      
      # Determine dimensionality and validate
      n_dim = objective$xdim
      if (is.na(n_dim)) {
        # If objective dimension is unknown, infer from provided type
        if (!is.null(type)) {
          private$.dimensionality = type
        } else {
          stop(sprintf("VisualizerObj requires objectives with known dimensionality or explicit type specification, but objective$xdim = %s and type = NULL", n_dim))
        }
      } else {
        # Use objective's known dimension
        private$.dimensionality = if (n_dim == 1) "1d" else if (n_dim == 2) "2d" else {
          stop(sprintf("VisualizerObj supports only 1D and 2D objectives, but objective$xdim = %s", n_dim))
        }
      }
      
      # Initialize appropriate data structure
      if (private$.dimensionality == "1d") {
        private$initialize_1d_data(x1_limits, n_points)
      } else {
        private$initialize_2d_data(x1_limits, x2_limits, padding, n_points)
      }
    },

    #' @description
    #' Add optimization trace to the plot.
    #'
    #' @param optimizer (`Optimizer`)\cr
    #'   The optimizer to add to the plot. Must have been run and contain archive data.
    #' @param line_color (`character(1)`)\cr
    #'   Color of the optimization trace line. If NULL, uses automatic color.
    #' @param line_width (`numeric(1)`)\cr
    #'   Width of the trace line. Default is 1.2.
    #' @param line_type (`character(1)`)\cr
    #'   Type of the trace line. One of "solid", "dashed", "dotted". Default is "solid".
    #' @param npoints (`integer(1)`)\cr
    #'   Number of points to show from the trace. If NULL, shows all points.
    #' @param npmax (`integer(1)`)\cr
    #'   Maximum number of points to show. If NULL, no limit.
    #' @param name (`character(1)`)\cr
    #'   Name for the trace (used in legends). If NULL, uses optimizer ID.
    #' @param add_marker_at (`integer()`)\cr
    #'   Iteration numbers where to add markers. For 2D plots only.
    #' @param marker_size (`numeric(1)`)\cr
    #'   Size of markers. Default is 3.
    #' @param marker_shape (`numeric(1)` or `character(1)`)\cr
    #'   Shape of markers. Default is 16 (filled circle).
    #' @param marker_color (`character(1)`)\cr
    #'   Color of markers. If NULL, uses line color.
    #' @param show_start_end (`logical(1)`)\cr
    #'   Whether to highlight start and end points. Default is TRUE for 2D plots.
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency. Default is 0.8.
    #' @template return_self_invisible
    add_optimization_trace = function(optimizer, line_color = NULL, line_width = 1.2, 
                                     line_type = "solid", npoints = NULL, npmax = NULL, 
                                     name = NULL, add_marker_at = 1, marker_size = 3, 
                                     marker_shape = 16, marker_color = NULL, 
                                     show_start_end = TRUE, alpha = 0.8) {
      checkmate::assert_r6(optimizer, "Optimizer")
      checkmate::assert_string(line_color, null.ok = TRUE)
      checkmate::assert_number(line_width, lower = 0)
      checkmate::assert_string(line_type)
      checkmate::assert_count(npoints, null.ok = TRUE)
      checkmate::assert_count(npmax, null.ok = TRUE)
      checkmate::assert_string(name, null.ok = TRUE)
      checkmate::assert_integerish(add_marker_at, lower = 1, null.ok = TRUE)
      checkmate::assert_number(marker_size, lower = 0)
      checkmate::assert(
        checkmate::check_string(marker_shape),
        checkmate::check_integerish(marker_shape, len = 1)
      )
      checkmate::assert_string(marker_color, null.ok = TRUE)
      checkmate::assert_flag(show_start_end)
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      
      # Validate optimizer has been run
      if (is.null(optimizer$archive) || nrow(optimizer$archive) == 0) {
        stop("Optimizer must be run before adding optimization trace (archive is empty)")
      }
      
      # Process trace data based on dimensionality
      trace_data = private$process_optimization_trace(optimizer, npoints, npmax)
      
      # Set defaults - use "auto" for automatic color assignment from palette
      line_color = line_color %??% "auto"
      marker_color = marker_color %??% line_color
      name = name %??% optimizer$id
      
      # Store optimization trace specification
      private$store_layer("optimization_trace", list(
        trace_data = trace_data,
        line_color = line_color,
        line_width = line_width,
        line_type = line_type,
        name = name,
        add_marker_at = if (private$.dimensionality == "2d") add_marker_at else NULL,
        marker_size = marker_size,
        marker_shape = marker_shape,
        marker_color = marker_color,
        show_start_end = if (private$.dimensionality == "2d") show_start_end else FALSE,
        alpha = alpha,
        dimensionality = private$.dimensionality
      ))

      return(invisible(self))
    },

    #' @description
    #' Create and return the plot with objective-specific features.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param title_size (`numeric(1)`)\cr
    #'   Title text size. If NULL, defaults to text_size + 2.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param background (`character(1)`)\cr
    #'   Background color. Default is "white".
    #' @param color_palette (`character(1)`)\cr
    #'   Color palette for the fill scale. One of "viridis", "plasma", "grayscale". Default is "viridis".
    #' @param plot_title (`character(1)`)\cr
    #'   Custom plot title. If NULL, uses default title.
    #' @param plot_subtitle (`character(1)`)\cr
    #'   Plot subtitle. Default is NULL.
    #' @param show_title (`logical(1)`)\cr
    #'   Whether to show the plot title. Default is TRUE.
    #' @param x_lab (`character(1)`)\cr
    #'   Custom x-axis label. If NULL, uses feature name.
    #' @param y_lab (`character(1)`)\cr
    #'   Custom y-axis label. If NULL, uses target name.
    #' @param x_limits (`numeric(2)`)\cr
    #'   X-axis limits as c(min, max). If NULL, uses default limits.
    #' @param y_limits (`numeric(2)`)\cr
    #'   Y-axis limits as c(min, max). If NULL, uses default limits.
    #' @param show_grid (`logical(1)`)\cr
    #'   Whether to show grid lines. Default is TRUE.
    #' @param grid_color (`character(1)`)\cr
    #'   Grid line color. Default is "gray90".
    #' @param show_legend (`logical(1)`)\cr
    #'   Whether to show the legend. Default is TRUE.
    #' @param legend_position (`character(1)`)\cr
    #'   Legend position: "top", "right", "bottom", "left", "none". Default is "right".
    #' @param legend_title (`character(1)`)\cr
    #'   Custom legend title. If NULL, uses default.
    #' @param ... Additional arguments (currently unused).
    #' @return A ggplot2 object.
    plot = function(text_size = 11, title_size = NULL, theme = "minimal", background = "white",
                   color_palette = "viridis", plot_title = NULL, plot_subtitle = NULL, 
                   show_title = TRUE, x_lab = NULL, y_lab = NULL, x_limits = NULL, 
                   y_limits = NULL, show_grid = TRUE, grid_color = "gray90", 
                   show_legend = TRUE, legend_position = "right", legend_title = NULL, ...) {
      
      # Validate arguments
      checkmate::assert_number(text_size, lower = .Machine$double.eps, finite = TRUE)
      checkmate::assert_number(title_size, lower = .Machine$double.eps, finite = TRUE, null.ok = TRUE)
      checkmate::assert_choice(theme, c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      checkmate::assert_string(background)
      checkmate::assert_choice(color_palette, c("viridis", "plasma", "grayscale"))
      checkmate::assert_string(plot_title, null.ok = TRUE)
      checkmate::assert_string(plot_subtitle, null.ok = TRUE)
      checkmate::assert_flag(show_title)
      checkmate::assert_string(x_lab, null.ok = TRUE)
      checkmate::assert_string(y_lab, null.ok = TRUE)
      checkmate::assert_numeric(x_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(y_limits, len = 2, null.ok = TRUE)
      checkmate::assert_flag(show_grid)
      checkmate::assert_string(grid_color)
      checkmate::assert_flag(show_legend)
      checkmate::assert_choice(legend_position, c("top", "right", "bottom", "left", "none"))
      checkmate::assert_string(legend_title, null.ok = TRUE)
      
      # Set title size default
      if (is.null(title_size)) {
        title_size = text_size + 2
      }
      
      # Store plot settings for layer resolution
      private$.plot_settings = list(
        text_size = text_size,
        title_size = title_size,
        theme = theme,
        background = background,
        color_palette = color_palette,
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        show_title = show_title,
        x_lab = x_lab,
        y_lab = y_lab,
        x_limits = x_limits,
        y_limits = y_limits,
        show_grid = show_grid,
        grid_color = grid_color,
        show_legend = show_legend,
        legend_position = legend_position,
        legend_title = legend_title,
        ...
      )
      
      # Call parent plot method to store settings and resolve layer colors
      tryCatch({
        super$plot(text_size = text_size, title_size = title_size, theme = theme, background = background,
                   color_palette = color_palette, plot_title = plot_title, plot_subtitle = plot_subtitle,
                   x_lab = x_lab, y_lab = y_lab, x_limits = x_limits, y_limits = y_limits,
                   show_grid = show_grid, grid_color = grid_color, show_legend = show_legend,
                   legend_position = legend_position, legend_title = legend_title, show_title = show_title)
      }, error = function(e) {
        # Expected error from abstract method - we just wanted the setup
        if (!grepl("Abstract method", e$message)) {
          stop(e)
        }
      })
      
      # Initialize the base plot based on dimensionality
      if (private$.dimensionality == "1d") {
        private$init_1d_plot()
      } else {
        private$init_2d_plot()
      }
      
      # Render stored layers in order
      if (!is.null(private$.layers_to_add)) {
        for (layer in private$.layers_to_add) {
          if (layer$type == "optimization_trace") {
            private$render_optimization_trace_layer(layer$spec)
          }
        }
      }
      
      return(private$.plot)
    }
  ),
  
  private = list(
    .dimensionality = NULL,    # "1d" or "2d"
    .data_structure = NULL,    # Unified data container
    .plot_settings = NULL,     # Plot settings for color resolution
    .plot = NULL,              # The actual ggplot2 object
    
    # Initialize data structure for 1D objectives
    initialize_1d_data = function(x1_limits, n_points) {
      # Determine x limits from objective bounds
      x1_limits = x1_limits %??% c(self$objective$lower, self$objective$upper)
      if (any(is.na(x1_limits))) {
        # Provide reasonable default limits when objective bounds are not available
        x1_limits = c(-5, 5)
        message("Objective bounds not available, using default limits: [-5, 5]. Use 'x1_limits' parameter for custom range.")
      }
      
      # Generate evaluation grid
      x_vals = seq(x1_limits[1], x1_limits[2], length.out = n_points)
      y_vals = sapply(x_vals, function(x) self$objective$eval(x))
      
      # Store in unified data structure
      private$.data_structure = list(
        dimensionality = "1d",
        coordinates = list(
          x1 = x_vals,
          x2 = NULL,
          y = y_vals
        ),
        labels = list(
          title = self$objective$label %??% self$objective$id,
          x1 = "x",
          x2 = NULL,
          y = "y"
        ),
        limits = list(
          x1 = x1_limits,
          x2 = NULL
        )
      )
    },
    
    # Initialize data structure for 2D objectives
    initialize_2d_data = function(x1_limits, x2_limits, padding, n_points) {
      # Determine limits from objective bounds
      x1_limits = x1_limits %??% c(self$objective$lower[1], self$objective$upper[1])
      x2_limits = x2_limits %??% c(self$objective$lower[2], self$objective$upper[2])
      
      if (any(is.na(x1_limits)) || any(is.na(x2_limits))) {
        stop("Limits could not be extracted from the objective. Please use 'x1_limits' and 'x2_limits'.")
      }
      
      # Apply padding
      x1_pad = (x1_limits[2] - x1_limits[1]) * padding
      x2_pad = (x2_limits[2] - x2_limits[1]) * padding
      
      # Generate evaluation grid
      x1 = unique(seq(x1_limits[1] - x1_pad, x1_limits[2] + x1_pad, length.out = n_points))
      x2 = unique(seq(x2_limits[1] - x2_pad, x2_limits[2] + x2_pad, length.out = n_points))
      grid = CJ(x1, x2)
      
      # Evaluate objective on grid
      y_vals = apply(grid, 1, function(row) self$objective$eval(c(row[1], row[2])))
      
      # Store in unified data structure
      private$.data_structure = list(
        dimensionality = "2d",
        coordinates = list(
          x1 = grid$x1,
          x2 = grid$x2,
          y = y_vals
        ),
        labels = list(
          title = self$objective$label %??% self$objective$id,
          x1 = "x1",
          x2 = "x2",
          y = "y"
        ),
        limits = list(
          x1 = x1_limits,
          x2 = x2_limits
        )
      )
    },
    
    # Process optimization trace data for the appropriate dimensionality
    process_optimization_trace = function(optimizer, npoints, npmax) {
      archive = optimizer$archive
      
      # Apply point limits
      if (!is.null(npmax)) {
        npoints = min(npoints %??% nrow(archive), npmax)
      }
      if (!is.null(npoints)) {
        n_rows = min(npoints, nrow(archive))
        if (n_rows > 0) {
          archive = archive[seq_len(n_rows), ]
        }
      }
      
      if (private$.dimensionality == "1d") {
        # For 1D: x_in is a list of vectors, we need the first element
        x_vals = sapply(archive$x_in, function(x) x[1])
        y_vals = archive$fval_in
        
        return(list(
          x_vals = x_vals,
          y_vals = y_vals,
          iteration = seq_len(length(x_vals))
        ))
        
      } else {
        # For 2D: extract x1 and x2 from x_in list
        x1_vals = sapply(archive$x_in, function(x) x[1])
        x2_vals = sapply(archive$x_in, function(x) x[2])
        y_vals = archive$fval_in
        
        return(list(
          x1 = x1_vals,
          x2 = x2_vals,
          y = y_vals,
          iteration = seq_len(length(x1_vals))
        ))
      }
    },
    
    # Initialize 1D plot with function line
    init_1d_plot = function() {
      # Get plot settings
      settings = private$.plot_settings
      
      # Create base data for the function line
      plot_data = data.frame(
        x = private$.data_structure$coordinates$x1,
        y = private$.data_structure$coordinates$y
      )
      
      # Create base ggplot
      private$.plot = private$init_ggplot(plot_data, "x", "y")
      
      # Add the function line
      private$.plot = private$.plot + ggplot2::geom_line(color = "blue", linewidth = 1.2)
      
      # Apply theme and styling
      private$.plot = private$apply_ggplot_theme(private$.plot, settings$text_size, NULL, settings$theme)
      
      # Determine labels
      title_text = if (!is.null(settings$plot_title)) settings$plot_title else private$.data_structure$labels$title
      x_text = if (!is.null(settings$x_lab)) settings$x_lab else private$.data_structure$labels$x1
      y_text = if (!is.null(settings$y_lab)) settings$y_lab else private$.data_structure$labels$y
      
      # Add labels conditionally
      private$.plot = private$.plot + ggplot2::labs(
        title = if (settings$show_title) title_text else NULL,
        subtitle = settings$plot_subtitle,
        x = x_text,
        y = y_text
      )
      
      # Apply axis limits
      if (!is.null(settings$x_limits)) {
        private$.plot = private$.plot + ggplot2::xlim(settings$x_limits)
      }
      if (!is.null(settings$y_limits)) {
        private$.plot = private$.plot + ggplot2::ylim(settings$y_limits)
      }
      
      # Apply grid settings
      if (!settings$show_grid) {
        private$.plot = private$.plot + ggplot2::theme(panel.grid = ggplot2::element_blank())
      } else {
        private$.plot = private$.plot + ggplot2::theme(
          panel.grid = ggplot2::element_line(color = settings$grid_color)
        )
      }
      
      # Apply title size
      private$.plot = private$.plot + ggplot2::theme(
        plot.title = ggplot2::element_text(size = settings$title_size)
      )
      
      # Apply legend settings
      if (!settings$show_legend) {
        private$.plot = private$.plot + ggplot2::theme(legend.position = "none")
      } else if (settings$legend_position != "right") {
        private$.plot = private$.plot + ggplot2::theme(legend.position = settings$legend_position)
      }
    },
    
    # Initialize 2D plot with filled contour/raster
    init_2d_plot = function() {
      # Get plot settings
      settings = private$.plot_settings
      
      # Create base data for the filled contour
      plot_data = data.frame(
        x1 = private$.data_structure$coordinates$x1,
        x2 = private$.data_structure$coordinates$x2,
        y = private$.data_structure$coordinates$y
      )
      
      # Create base ggplot
      private$.plot = private$init_ggplot(plot_data, "x1", "x2")
      
      # Add filled contour or raster
      private$.plot = private$.plot + ggplot2::geom_raster(ggplot2::aes(fill = y), alpha = 0.8)
      
      # Apply color scale
      private$.plot = private$apply_ggplot_color_scale(private$.plot, settings$color_palette, "fill")
      
      # Apply theme and styling
      private$.plot = private$apply_ggplot_theme(private$.plot, settings$text_size, NULL, settings$theme)
      
      # Determine labels
      title_text = if (!is.null(settings$plot_title)) settings$plot_title else private$.data_structure$labels$title
      x_text = if (!is.null(settings$x_lab)) settings$x_lab else private$.data_structure$labels$x1
      y_text = if (!is.null(settings$y_lab)) settings$y_lab else private$.data_structure$labels$x2
      fill_text = if (!is.null(settings$legend_title)) settings$legend_title else private$.data_structure$labels$y
      
      # Add labels conditionally
      private$.plot = private$.plot + ggplot2::labs(
        title = if (settings$show_title) title_text else NULL,
        subtitle = settings$plot_subtitle,
        x = x_text,
        y = y_text,
        fill = fill_text
      )
      
      # Apply axis limits
      if (!is.null(settings$x_limits)) {
        private$.plot = private$.plot + ggplot2::xlim(settings$x_limits)
      }
      if (!is.null(settings$y_limits)) {
        private$.plot = private$.plot + ggplot2::ylim(settings$y_limits)
      }
      
      # Apply grid settings
      if (!settings$show_grid) {
        private$.plot = private$.plot + ggplot2::theme(panel.grid = ggplot2::element_blank())
      } else {
        private$.plot = private$.plot + ggplot2::theme(
          panel.grid = ggplot2::element_line(color = settings$grid_color)
        )
      }
      
      # Apply title size
      private$.plot = private$.plot + ggplot2::theme(
        plot.title = ggplot2::element_text(size = settings$title_size)
      )
      
      # Apply legend settings
      if (!settings$show_legend) {
        private$.plot = private$.plot + ggplot2::theme(legend.position = "none")
      } else if (settings$legend_position != "right") {
        private$.plot = private$.plot + ggplot2::theme(legend.position = settings$legend_position)
      }
    },
    
    # Render optimization trace layer
    render_optimization_trace_layer = function(layer_spec) {
      trace_data = layer_spec$trace_data
      
      if (private$.dimensionality == "1d") {
        # Create data frame for 1D traces
        dd_trace = data.frame(
          x = trace_data$x_vals, 
          y = trace_data$y_vals
        )
        
        # Add trace points
        private$.plot = private$.plot + ggplot2::geom_point(
          data = dd_trace, 
          ggplot2::aes(x = x, y = y),
          size = layer_spec$marker_size, 
          color = layer_spec$line_color,
          shape = layer_spec$marker_shape, 
          alpha = layer_spec$alpha,
          inherit.aes = FALSE
        )
      } else {
        # Create data frame for 2D traces
        trace_df = data.frame(
          x1 = trace_data$x1,
          x2 = trace_data$x2,
          y = trace_data$y,
          iteration = trace_data$iteration
        )
        
        # Add trace line
        private$.plot = private$.plot + ggplot2::geom_path(
          data = trace_df,
          ggplot2::aes(x = x1, y = x2),
          color = layer_spec$line_color,
          linewidth = layer_spec$line_width,
          linetype = layer_spec$line_type,
          alpha = layer_spec$alpha,
          inherit.aes = FALSE
        )
        
        # Add markers at specified iterations
        if (!is.null(layer_spec$add_marker_at)) {
          marker_df = trace_df[trace_df$iteration %in% layer_spec$add_marker_at, , drop = FALSE]
          if (nrow(marker_df) > 0) {
            private$.plot = private$.plot + ggplot2::geom_point(
              data = marker_df,
              ggplot2::aes(x = x1, y = x2),
              size = layer_spec$marker_size,
              color = layer_spec$marker_color,
              shape = layer_spec$marker_shape,
              alpha = layer_spec$alpha,
              inherit.aes = FALSE
            )
          }
        }
        
        # Add start/end markers if requested
        if (layer_spec$show_start_end && nrow(trace_df) > 1) {
          # Start point (green)
          private$.plot = private$.plot + ggplot2::geom_point(
            data = trace_df[1, , drop = FALSE],
            ggplot2::aes(x = x1, y = x2),
            size = layer_spec$marker_size + 1,
            color = "green",
            shape = 16,
            alpha = layer_spec$alpha,
            inherit.aes = FALSE
          )
          # End point (red)
          private$.plot = private$.plot + ggplot2::geom_point(
            data = trace_df[nrow(trace_df), , drop = FALSE],
            ggplot2::aes(x = x1, y = x2),
            size = layer_spec$marker_size + 1,
            color = "red",
            shape = 17,  # Triangle
            alpha = layer_spec$alpha,
            inherit.aes = FALSE
          )
        }
      }
    }
  )
)
