#' @title Visualize Objective
#'
#' @description
#' This class is used to create visualizations and animations of optimization traces.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer2DObj = R6::R6Class("Visualizer2DObj",
  inherit = Visualizer2D,
  public = list(

    #' @template field_objective
    objective = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_objective
    initialize = function(objective,
                          x1_limits = NULL,
                          x2_limits = NULL,
                          padding = 0,
                          n_points = 100L) {
      self$objective = checkmate::assert_r6(objective, "Objective")
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(padding)
      checkmate::assert_count(n_points)

      if (objective$xdim != 2) {
        mlr3misc::stopf("`Visualizer2D` requires 2-dimensional inputs, but `objective$xdim = %s`", objective$xdim)
      }

      x1_limits = x1_limits %??% c(objective$lower[1], objective$upper[1])
      x2_limits = x2_limits %??% c(objective$lower[2], objective$upper[2])

      if (any(is.na(x1_limits)) || any(is.na(x2_limits))) {
        stop("Limits could not be extracted from the objective. Please use `x_limits`.")
      }

      x1_pad = (x1_limits[2] - x1_limits[1]) * padding
      x2_pad = (x2_limits[2] - x2_limits[1]) * padding

      x1 = unique(seq(x1_limits[1] - x1_pad, x1_limits[2] + x1_pad, length.out = n_points))
      x2 = unique(seq(x2_limits[1] - x2_pad, x2_limits[2] + x2_pad, length.out = n_points))
      grid = CJ(x1, x2)

      y = apply(grid, 1, function(row) {
        self$objective$eval(c(row[1], row[2]))
      })

      super$initialize(
        fun_x1 = grid[, "x1", with = FALSE][[1]],
        fun_x2 = grid[, "x2", with = FALSE][[1]],
        fun_y = y,
        title = self$objective$label,
        lab_x1 = "x1",
        lab_x2 = "x2",
        lab_y = "y"
      )
    },

    #' @description
    #' Add optimization trace to the plot.
    #' @param optimizer (`Optimizer`)\cr
    #'   The optimizer from which the archive is extracted and used to plot the trace.
    #' @param line_color (`character(1)`)\cr
    #'   The color of the trace line. If NULL, a random color is generated.
    #' @param line_width (`numeric(1)`)\cr
    #'   The width of the trace line. Default is 1.2.
    #' @param line_type (`character(1)`)\cr
    #'   The line type for the trace. Default is "solid".
    #' @param npoints (`integer(1)`)\cr
    #'   The number of used points from the archive. Default is NULL which means all points are used.
    #' @param npmax (`integer(1)`)\cr
    #'   The maximum number of points to use. Default is NULL.
    #' @param name (`character(1)`)\cr
    #'   The name of the trace in the legend. Default is NULL which creates a name from optimizer and objective IDs.
    #' @param add_marker_at (`integer()`)\cr
    #'   Vector of iterations at which markers are added. Default is first iteration only.
    #' @param marker_size (`numeric(1)`)\cr
    #'   Size of the markers. Default is 3.
    #' @param marker_shape (`character(1)` or `integer(1)`)\cr
    #'   Shape of the markers. Default is "circle" (16).
    #' @param marker_color (`character(1)`)\cr
    #'   Color of the markers. If NULL, uses line_color.
    #' @param show_start_end (`logical(1)`)\cr
    #'   Whether to highlight start and end points differently. Default is TRUE.
    #' @param alpha (`numeric(1)`)\cr
    #'   Transparency level for the trace. Default is 0.8.
    #' @return Returns self invisibly for method chaining.
    add_optimization_trace = function(optimizer, line_color = NULL, line_width = 1.2, line_type = "solid",
                                      npoints = NULL, npmax = NULL, name = NULL, add_marker_at = 1,
                                      marker_size = 3, marker_shape = 16, marker_color = NULL,
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

      if (nrow(optimizer$archive) == 0) {
        stop("No optimization trace in `optimizer$archive`. Did you forget to call `optimizer$optimize(steps)`?")
      }

      # Generate color if not provided using unified color system
      if (is.null(line_color)) {
        line_color = "auto"
      }

      if (is.null(marker_color)) marker_color = line_color  # Use same color for marker
      if (is.null(name)) {
        name = paste0(optimizer$id, " on ", optimizer$objective$id)
      }

      # Extract trace data from optimizer archive
      xmat = do.call(rbind, c(optimizer$archive$x_in[1], optimizer$archive$x_out))
      trace_data = data.frame(
        x1 = xmat[, 1],
        x2 = xmat[, 2],
        iteration = seq_len(nrow(xmat)),
        stringsAsFactors = FALSE
      )

      # Apply point filtering if requested
      if (!is.null(npoints) || !is.null(npmax)) {
        if (is.null(npoints)) npoints = nrow(trace_data)
        if (is.null(npmax)) npmax = nrow(trace_data)
        npmax = min(npmax, nrow(trace_data))
        
        # Subsample points
        indices = unique(round(seq(1, nrow(trace_data), length.out = npoints)))
        indices = indices[indices <= npmax]
        trace_data = trace_data[indices, ]
        
        # Adjust marker positions
        if (!is.null(add_marker_at)) {
          add_marker_at = add_marker_at[add_marker_at <= npmax]
        }
      }

      # Store trace information without resolving colors yet
      trace_info = list(
        data = trace_data,
        line_color = line_color,  # Keep for later resolution
        line_width = line_width,
        line_type = line_type,
        name = name,
        add_marker_at = add_marker_at,
        marker_size = marker_size,
        marker_shape = marker_shape,
        marker_color = marker_color,  # Keep for later resolution  
        show_start_end = show_start_end,
        alpha = alpha
      )

      private$store_layer("optimization_trace", trace_info)
      
      invisible(self)
    },

    #' @description
    #' Create and return the ggplot2 plot with optimization traces.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(...) {
      # Call parent plot method first to set up plot_settings and resolve colors
      p = super$plot(...)
      
      # Render optimization traces if any exist
      private$render_optimization_trace_layers(p)
    }
  ),
  private = list(
    # Render stored optimization trace layers
    render_optimization_trace_layers = function(plot_obj) {
      # Get all stored optimization trace layers
      trace_layers = private$get_layers_by_type("optimization_trace")
      
      if (length(trace_layers) == 0) {
        return(plot_obj)
      }
      
      for (trace_spec in trace_layers) {
        plot_obj = private$render_optimization_trace_layer(plot_obj, trace_spec)
      }
      
      return(plot_obj)
    },
    
    # Render a single optimization trace layer
    render_optimization_trace_layer = function(plot_obj, layer_spec) {
      trace_data = layer_spec$data

      # Add the trace line
      plot_obj = plot_obj + ggplot2::geom_path(
        data = trace_data,
        ggplot2::aes(x = x1, y = x2),
        color = layer_spec$line_color,
        linewidth = layer_spec$line_width,
        linetype = layer_spec$line_type,
        alpha = layer_spec$alpha,
        inherit.aes = FALSE
      )

      # Add markers at specified iterations
      if (!is.null(layer_spec$add_marker_at) && length(layer_spec$add_marker_at) > 0) {
        marker_data = trace_data[trace_data$iteration %in% layer_spec$add_marker_at, ]
        if (nrow(marker_data) > 0) {
          # Resolve marker color if it's still "auto" or matches line color
          marker_color = if (layer_spec$marker_color == layer_spec$line_color) layer_spec$line_color else layer_spec$marker_color
          
          plot_obj = plot_obj + ggplot2::geom_point(
            data = marker_data,
            ggplot2::aes(x = x1, y = x2),
            color = marker_color,
            size = layer_spec$marker_size,
            shape = layer_spec$marker_shape,
            alpha = layer_spec$alpha,
            inherit.aes = FALSE
          )
        }
      }

      # Add special start/end markers if requested
      if (layer_spec$show_start_end && nrow(trace_data) > 1) {
        # Resolve marker color for start/end points
        marker_color = if (layer_spec$marker_color == layer_spec$line_color) layer_spec$line_color else layer_spec$marker_color
        
        # Start point (larger, different shape)
        start_data = trace_data[1, ]
        plot_obj = plot_obj + ggplot2::geom_point(
          data = start_data,
          ggplot2::aes(x = x1, y = x2),
          color = marker_color,
          size = layer_spec$marker_size + 1,
          shape = 21,  # circle with border
          fill = "white",
          stroke = 2,
          alpha = 1,
          inherit.aes = FALSE
        )

        # End point (larger, different shape)
        end_data = trace_data[nrow(trace_data), ]
        plot_obj = plot_obj + ggplot2::geom_point(
          data = end_data,
          ggplot2::aes(x = x1, y = x2),
          color = marker_color,
          size = layer_spec$marker_size + 1,
          shape = 23,  # diamond
          fill = marker_color,
          alpha = 1,
          inherit.aes = FALSE
        )
      }
      
      return(plot_obj)
    }
  )
)
