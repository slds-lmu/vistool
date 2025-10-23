#' @title Visualize objective (unified 1D/2D)
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
        private$.dimensionality = if (n_dim == 1) {
          "1d"
        } else if (n_dim == 2) {
          "2d"
        } else {
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
    #'   Width of the trace line. If NULL, uses theme$line_width.
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
    #'   Alpha transparency. If NULL, uses theme$alpha.
    #' @template return_self_invisible
    add_optimization_trace = function(optimizer, line_color = NULL, line_width = NULL,
                                      line_type = "solid", npoints = NULL, npmax = NULL,
                                      name = NULL, add_marker_at = 1, marker_size = 3,
                                      marker_shape = 16, marker_color = NULL,
                                      show_start_end = TRUE, alpha = NULL) {
      checkmate::assert_r6(optimizer, "Optimizer")
      checkmate::assert_string(line_color, null.ok = TRUE)
      checkmate::assert_number(line_width, lower = 0, null.ok = TRUE)
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
      checkmate::assert_number(alpha, lower = 0, upper = 1, null.ok = TRUE)

      # Validate optimizer has been run
      if (is.null(optimizer$archive) || nrow(optimizer$archive) == 0) {
        stop("Optimizer must be run before adding optimization trace (archive is empty)")
      }

      # Process trace data based on dimensionality
      trace_data = private$process_optimization_trace(optimizer, npoints, npmax)

      # Set defaults - use "auto" for automatic color assignment from palette
      line_color = if (is.null(line_color)) "auto" else line_color
      marker_color = if (is.null(marker_color)) line_color else marker_color
      name = if (is.null(name)) optimizer$id else name

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
    #' Create and return the ggplot2 plot with model-specific layers.
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(...) {
      # Call parent plot method for validation and settings storage
      super$plot(...)

      # Resolve layer colors before rendering
      self$resolve_layer_colors()

      # Initialize the base plot based on dimensionality
      if (private$.dimensionality == "1d") {
        private$init_1d_plot()
      } else {
        private$init_2d_plot()
      }

      # Render stored layers in the order they were added
      private$render_all_layers()

      private$.last_plot = private$.plot
      return(private$.plot)
    }
  ),
  private = list(
    .dimensionality = NULL, # "1d" or "2d"
    .data_structure = NULL, # Unified data container
    .plot = NULL, # The actual ggplot2 object


    # Render all stored layers in the order they were added
    render_all_layers = function() {
      if (!is.null(private$.layers_to_add)) {
        # 1) Render optimization traces together so they share a combined legend
        private$render_optimization_trace_layers()

        # 2) Render other generic layers
        for (layer in private$.layers_to_add) {
          if (layer$type == "points") {
            # Handle points layer using the base class method
            private$.plot = private$add_points_to_ggplot(private$.plot, private$.dimensionality)
          } else if (layer$type == "contour") {
            private$render_contour_layer(layer$spec)
          }
        }
      }

      ranges = list(
        x = private$.data_structure$coordinates$x1,
        y = if (private$.dimensionality == "1d") private$.data_structure$coordinates$y else private$.data_structure$coordinates$x2,
        z = if (private$.dimensionality == "2d") private$.data_structure$coordinates$y else NULL
      )
      private$.plot = private$add_annotations_to_ggplot(private$.plot, private$.dimensionality, ranges)
    },

    render_contour_layer = function(layer_spec) {
      if (private$.dimensionality != "2d") {
        warning("Contour layers require 2D objectives; ignoring request.", call. = FALSE)
        return(invisible(NULL))
      }

      eff = private$.effective_theme
      coords = private$.data_structure$coordinates

      linewidth = if (!is.null(layer_spec$linewidth)) layer_spec$linewidth else eff$line_width
      linetype = if (!is.null(layer_spec$linetype)) layer_spec$linetype else "solid"
      alpha = if (!is.null(layer_spec$alpha)) layer_spec$alpha else eff$alpha
      extra_args = layer_spec$extra_args
      if (length(extra_args)) {
        keep = !vapply(extra_args, is.null, logical(1))
        extra_args = extra_args[keep]
      }

      if (!identical(layer_spec$mode, "scale")) {
        data = data.frame(
          x = coords$x1,
          y = coords$x2,
          z = coords$y,
          stringsAsFactors = FALSE
        )

        args = list(
          data = data,
          mapping = ggplot2::aes(x = x, y = y, z = z),
          inherit.aes = FALSE
        )

        if (!is.null(layer_spec$breaks)) args$breaks = layer_spec$breaks
        if (!is.null(layer_spec$bins)) args$bins = layer_spec$bins
        if (!is.null(layer_spec$binwidth)) args$binwidth = layer_spec$binwidth
        if (!is.null(layer_spec$color)) args$colour = layer_spec$color
        if (!is.null(linewidth)) args$linewidth = linewidth
        if (!is.null(linetype)) args$linetype = linetype
        if (!is.null(alpha)) args$alpha = alpha
        if (length(extra_args)) args = c(args, extra_args)

        private$.plot = private$.plot + do.call(ggplot2::geom_contour, args)
        return(invisible(NULL))
      }

      x_vals = sort(unique(coords$x1))
      y_vals = sort(unique(coords$x2))

      if (!length(x_vals) || !length(y_vals)) {
        warning("Unable to compute contours because the evaluation grid is empty.", call. = FALSE)
        return(invisible(NULL))
      }

      z_vals = coords$y
      if (any(!is.finite(z_vals))) {
        warning("Cannot compute contours because the evaluation grid contains non-finite values.", call. = FALSE)
        return(invisible(NULL))
      }

      z_range = range(z_vals)
      if (!all(is.finite(z_range)) || diff(z_range) <= .Machine$double.eps) {
        return(invisible(NULL))
      }

      z_matrix = matrix(z_vals, nrow = length(x_vals), ncol = length(y_vals), byrow = TRUE)

      levels = layer_spec$breaks
      if (!is.null(levels)) {
        levels = sort(unique(levels))
      } else if (!is.null(layer_spec$binwidth) && layer_spec$binwidth > 0) {
        start = z_range[1]
        end = z_range[2]
        levels = seq(start, end, by = layer_spec$binwidth)
      } else {
        bins = if (is.null(layer_spec$bins)) 10L else layer_spec$bins
        levels = pretty(z_range, n = bins)
      }

      levels = levels[levels > z_range[1] & levels < z_range[2]]
      levels = sort(unique(levels))
      if (!length(levels)) {
        return(invisible(NULL))
      }

      contour_list = grDevices::contourLines(x = x_vals, y = y_vals, z = z_matrix, levels = levels)
      if (!length(contour_list)) {
        return(invisible(NULL))
      }

      palette_name = if (!is.null(layer_spec$palette)) layer_spec$palette else eff$palette
      if (is.null(palette_name)) palette_name = "viridis"
      scale_def = get_continuous_colorscale(palette_name)
      palette_colors = vapply(scale_def, function(entry) entry[[2]], character(1))
      palette_fun = grDevices::colorRampPalette(palette_colors)
      color_values = palette_fun(max(2L, length(levels)))
      color_map = stats::setNames(color_values[seq_along(levels)], as.character(levels))

      contour_dfs = lapply(seq_along(contour_list), function(idx) {
        line = contour_list[[idx]]
        if (length(line$x) < 2L) {
          return(NULL)
        }
        data.frame(
          x = line$x,
          y = line$y,
          level = line$level,
          group = sprintf("contour_%d", idx),
          stringsAsFactors = FALSE
        )
      })
      contour_dfs = Filter(Negate(is.null), contour_dfs)
      if (!length(contour_dfs)) {
        return(invisible(NULL))
      }

      contours_combined = do.call(rbind, contour_dfs)
      split_levels = split(contours_combined, contours_combined$level)

      for (lvl in names(split_levels)) {
        df_lvl = split_levels[[lvl]]
        if (nrow(df_lvl) < 2L) {
          next
        }

        lvl_key = as.character(df_lvl$level[1])
        colour_value = color_map[[lvl_key]]
        if (is.null(colour_value)) {
          numeric_keys = suppressWarnings(as.numeric(names(color_map)))
          if (all(is.na(numeric_keys))) {
            colour_value = color_map[[1L]]
          } else {
            closest_idx = which.min(abs(numeric_keys - df_lvl$level[1]))
            colour_value = color_map[[closest_idx]]
          }
        }

        path_args = list(
          data = df_lvl,
          mapping = ggplot2::aes(x = x, y = y, group = group),
          inherit.aes = FALSE,
          show.legend = FALSE,
          colour = colour_value
        )

        if (!is.null(linewidth)) path_args$linewidth = linewidth
        if (!is.null(linetype)) path_args$linetype = linetype
        if (!is.null(alpha)) path_args$alpha = alpha
        if (length(extra_args)) path_args = c(path_args, extra_args)

        private$.plot = private$.plot + do.call(ggplot2::geom_path, path_args)
      }

      invisible(NULL)
    },

    # Initialize data structure for 1D objectives
    initialize_1d_data = function(x1_limits, n_points) {
      # Determine x limits from objective bounds
      x1_limits = if (is.null(x1_limits)) c(self$objective$lower, self$objective$upper) else x1_limits
      if (any(is.na(x1_limits))) {
        stop("Objective bounds not available; please specify 'x1_limits' explicitly.")
      }

      # Warn if user-specified (or padded) limits exceed objective bounds
      eval_lower = self$objective$lower
      eval_upper = self$objective$upper
      if (!any(is.na(c(eval_lower, eval_upper)))) {
        if (x1_limits[1] < eval_lower || x1_limits[2] > eval_upper) {
          warning(sprintf(
            "Plot limits (x1_limits = [%s, %s]) exceed objective bounds [%s, %s]; evaluating outside the defined domain.",
            format(x1_limits[1]), format(x1_limits[2]), format(eval_lower), format(eval_upper)
          ))
        }
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
          title = if (is.null(self$objective$label)) self$objective$id else self$objective$label,
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
      x1_limits = if (is.null(x1_limits)) c(self$objective$lower[1], self$objective$upper[1]) else x1_limits
      x2_limits = if (is.null(x2_limits)) c(self$objective$lower[2], self$objective$upper[2]) else x2_limits

      if (any(is.na(x1_limits)) || any(is.na(x2_limits))) {
        stop("Objective bounds not available; please specify both 'x1_limits' and 'x2_limits' explicitly.")
      }

      # Apply padding
      x1_pad = (x1_limits[2] - x1_limits[1]) * padding
      x2_pad = (x2_limits[2] - x2_limits[1]) * padding

      # Generate evaluation grid
      x1 = unique(seq(x1_limits[1] - x1_pad, x1_limits[2] + x1_pad, length.out = n_points))
      x2 = unique(seq(x2_limits[1] - x2_pad, x2_limits[2] + x2_pad, length.out = n_points))
      grid = CJ(x1, x2)

      # Warn if limits exceed objective bounds
      eval_lower = self$objective$lower
      eval_upper = self$objective$upper
      if (!any(is.na(c(eval_lower, eval_upper)))) {
        warn_x1 = (x1_limits[1] < eval_lower[1]) || (x1_limits[2] > eval_upper[1])
        warn_x2 = (x2_limits[1] < eval_lower[2]) || (x2_limits[2] > eval_upper[2])
        if (warn_x1 || warn_x2) {
          warning(sprintf(
            "Plot limits exceed objective bounds: x1_limits=[%s,%s], x2_limits=[%s,%s]; bounds x1=[%s,%s], x2=[%s,%s]. Evaluating outside the defined domain.",
            format(x1_limits[1]), format(x1_limits[2]), format(x2_limits[1]), format(x2_limits[2]),
            format(eval_lower[1]), format(eval_upper[1]), format(eval_lower[2]), format(eval_upper[2])
          ))
        }
      }

      # Evaluate objective on the full grid
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
          title = if (is.null(self$objective$label)) self$objective$id else self$objective$label,
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
        npoints = min(if (is.null(npoints)) nrow(archive) else npoints, npmax)
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
      # Use helper method with objective-specific geom layer
      private$gg_init_1d(private$.data_structure, function(plot_obj, plot_data, eff) {
        line_color = get_vistool_color(1, "discrete", base_palette = eff$palette)
        plot_obj + ggplot2::geom_line(color = line_color, linewidth = if (is.null(eff$line_width)) 1.2 else eff$line_width)
      })
    },

    # Initialize 2D plot with filled contour/raster
    init_2d_plot = function() {
      # Use helper method with objective-specific geom layer
      private$gg_init_2d(private$.data_structure, function(plot_obj, plot_data, eff) {
        # Add filled contour or raster
        plot_obj = plot_obj + ggplot2::geom_raster(ggplot2::aes(fill = y), alpha = eff$alpha)
        # Apply color scale
        private$apply_ggplot_color_scale(plot_obj, eff$palette, "fill")
      })
    },

    # Render all optimization trace layers with a combined legend (ggplot2 backend)
    render_optimization_trace_layers = function() {
      trace_layers = private$get_layers_by_type("optimization_trace")
      if (length(trace_layers) == 0) return(invisible(NULL))

      eff = private$.effective_theme
      rp = private$.render_params

      # Build combined data and scales
      color_map = c()
      ltype_map = c()
      lw_map = c()

      if (private$.dimensionality == "1d") {
        # Combine points for 1D traces
        dd_list = list()
        for (tl in trace_layers) {
          td = tl$trace_data
          nm = tl$name
          dd = data.frame(x = td$x_vals, y = td$y_vals, .trace = nm, stringsAsFactors = FALSE)
          dd_list[[length(dd_list) + 1L]] = dd
          # Collect scale mappings
          color_map[nm] = tl$line_color
          ltype_map[nm] = if (is.null(tl$line_type)) "solid" else tl$line_type
          lw_map[nm] = if (is.null(tl$line_width)) eff$line_width else tl$line_width
        }
        all_pts = do.call(rbind, dd_list)

        # Draw points mapped by color to show legend
        private$.plot = private$.plot + ggplot2::geom_point(
          data = all_pts,
          ggplot2::aes(x = x, y = y, color = .trace),
          size = max(vapply(trace_layers, function(tl) tl$marker_size, numeric(1))),
          alpha = if (is.null(trace_layers[[1]]$alpha)) eff$alpha else trace_layers[[1]]$alpha,
          inherit.aes = FALSE
        )

        # Apply manual color scale (legend title optional)
        legend_title = if (!is.null(rp$legend_title)) rp$legend_title else "Trace"
        private$.plot = private$.plot + ggplot2::scale_color_manual(values = color_map, name = legend_title)
        # linewidth/linetype not applied for points; ignore in 1D
      } else {
        # 2D traces: combine paths and then add markers separately without affecting legend
        line_list = list()
        for (tl in trace_layers) {
          td = tl$trace_data
          nm = tl$name
          dd = data.frame(x1 = td$x1, x2 = td$x2, iteration = td$iteration, .trace = nm, stringsAsFactors = FALSE)
          line_list[[length(line_list) + 1L]] = dd
          color_map[nm] = tl$line_color
          ltype_map[nm] = if (is.null(tl$line_type)) "solid" else tl$line_type
          lw_map[nm] = if (is.null(tl$line_width)) eff$line_width else tl$line_width
        }
        all_lines = do.call(rbind, line_list)

        # Draw all paths with color/linetype/linewidth mapped to trace name
        alpha_val = eff$alpha
        private$.plot = private$.plot + ggplot2::geom_path(
          data = all_lines,
          ggplot2::aes(x = x1, y = x2, color = .trace, linetype = .trace, linewidth = .trace),
          alpha = alpha_val,
          inherit.aes = FALSE
        )

        # Manual scales: color/linetype show in legend; linewidth hidden to avoid duplicate legends
        legend_title = if (!is.null(rp$legend_title)) rp$legend_title else "Trace"
        private$.plot = private$.plot +
          ggplot2::scale_color_manual(values = color_map, name = legend_title) +
          ggplot2::scale_linetype_manual(values = ltype_map, name = legend_title) +
          ggplot2::scale_linewidth_manual(values = lw_map, guide = "none")

        # Add markers and start/end indicators (no legend)
        for (tl in trace_layers) {
          td = tl$trace_data
          nm = tl$name
          resolved_alpha = if (is.null(tl$alpha)) eff$alpha else tl$alpha
          # Markers at specified iterations
          if (!is.null(tl$add_marker_at)) {
            marker_df = data.frame(x1 = td$x1, x2 = td$x2, iteration = td$iteration, stringsAsFactors = FALSE)
            marker_df = marker_df[marker_df$iteration %in% tl$add_marker_at, , drop = FALSE]
            if (nrow(marker_df) > 0) {
              private$.plot = private$.plot + ggplot2::geom_point(
                data = marker_df,
                ggplot2::aes(x = x1, y = x2),
                size = tl$marker_size,
                color = tl$marker_color,
                shape = tl$marker_shape,
                alpha = resolved_alpha,
                inherit.aes = FALSE,
                show.legend = FALSE
              )
            }
          }

          # Start/end markers
          if (tl$show_start_end) {
            se_df = data.frame(x1 = td$x1, x2 = td$x2)
            private$.plot = private$.plot + ggplot2::geom_point(
              data = se_df[1, , drop = FALSE],
              ggplot2::aes(x = x1, y = x2),
              size = tl$marker_size + 1,
              color = tl$line_color,
              shape = 16,
              alpha = resolved_alpha,
              inherit.aes = FALSE,
              show.legend = FALSE
            )
            private$.plot = private$.plot + ggplot2::geom_point(
              data = se_df[nrow(se_df), , drop = FALSE],
              ggplot2::aes(x = x1, y = x2),
              size = tl$marker_size + 1,
              color = tl$line_color,
              shape = 17,
              alpha = resolved_alpha,
              inherit.aes = FALSE,
              show.legend = FALSE
            )
          }
        }
      }

      invisible(NULL)
    }
  )
)
