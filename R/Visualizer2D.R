#' @title Visualize 2D Functions
#'
#' @description
#' Visualizes a two-dimensional function \eqn{f: \mathbb{R}^2 \to \mathbb{R}}.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer2D <- R6::R6Class("Visualizer2D",
  inherit = Visualizer,
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

    #' @field points_y (`numeric()`)\cr
    #' y-values of points.
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
    initialize = function(fun_x1,
                          fun_x2,
                          fun_y,
                          title = NULL,
                          lab_x1 = "x1",
                          lab_x2 = "x2",
                          lab_y = "y") {
      self$fun_x1 <- checkmate::assert_numeric(fun_x1)
      self$fun_x2 <- checkmate::assert_numeric(fun_x2)
      self$fun_y <- checkmate::assert_numeric(fun_y)
      self$title <- checkmate::assert_character(title, null.ok = TRUE)
      self$lab_x1 <- checkmate::assert_character(lab_x1)
      self$lab_x2 <- checkmate::assert_character(lab_x2)
      self$lab_y <- checkmate::assert_character(lab_y)
    },

    #' @description
    #' Create and return the ggplot2 plot.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @return A ggplot2 object.
    plot = function(text_size = 11, theme = "minimal") {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      
      data <- data.table(
        fun_x1 = self$fun_x1,
        fun_x2 = self$fun_x2,
        fun_y = self$fun_y
      )

      # adjust y breaks
      min_y <- min(self$fun_y)
      max_y <- max(self$fun_y)
      breaks <- if (!is.null(self$points_y)) {
        pretty(c(min_y, max_y), n = 10, min.n = 6L)
      } else {
        pretty(c(min_y, max_y), n = 10, min.n = 6L)
      }

      p <- ggplot(data, aes(x = fun_x1, y = fun_x2, z = fun_y)) +
        geom_contour_filled(breaks = breaks, show.legend = TRUE) +
        geom_contour(color = "white", alpha = 0.3) +
        labs(title = self$title, x = self$lab_x1, y = self$lab_x2) +
        scale_fill_viridis_d(name = self$lab_y, drop = FALSE)

      # add decision boundary if available (for classification)
      if (!is.null(private$.decision_threshold)) {
        p <- p + geom_contour(aes(z = fun_y),
          breaks = private$.decision_threshold,
          color = "black", linewidth = 1.5, alpha = 0.8
        )
      }

      # add training points if available
      if (!is.null(self$points_x1) && !is.null(self$points_x2) && !is.null(self$points_y)) {
        points_data <- data.table(
          points_x1 = self$points_x1,
          points_x2 = self$points_x2,
          points_y = self$points_y
        )

        # determine color scale limits based on function values
        color_limits <- c(min(self$fun_y), max(self$fun_y))

        p <- p + geom_point(aes(x = points_x1, y = points_x2, color = points_y),
          data = points_data,
          size = 2,
          inherit.aes = FALSE,
          show.legend = FALSE
        ) +
          scale_color_viridis_c(name = self$lab_y, limits = color_limits)
      }

      # Add optimization traces if any
      if (length(private$.optimization_traces) > 0) {
        for (trace in private$.optimization_traces) {
          trace_data <- trace$data

          # Add the trace line
          p <- p + geom_path(
            data = trace_data,
            aes(x = x1, y = x2),
            color = trace$line_color,
            linewidth = trace$line_width,
            linetype = trace$line_type,
            alpha = trace$alpha,
            inherit.aes = FALSE
          )

          # Add markers at specified iterations
          if (!is.null(trace$add_marker_at) && length(trace$add_marker_at) > 0) {
            marker_data <- trace_data[trace_data$iteration %in% trace$add_marker_at, ]
            if (nrow(marker_data) > 0) {
              p <- p + geom_point(
                data = marker_data,
                aes(x = x1, y = x2),
                color = trace$marker_color,
                size = trace$marker_size,
                shape = trace$marker_shape,
                alpha = trace$alpha,
                inherit.aes = FALSE
              )
            }
          }

          # Add special start/end markers if requested
          if (trace$show_start_end && nrow(trace_data) > 1) {
            # Start point (larger, different shape)
            start_data <- trace_data[1, ]
            p <- p + geom_point(
              data = start_data,
              aes(x = x1, y = x2),
              color = trace$marker_color,
              size = trace$marker_size + 1,
              shape = 21,  # circle with border
              fill = "white",
              stroke = 2,
              alpha = 1,
              inherit.aes = FALSE
            )

            # End point (larger, different shape)
            end_data <- trace_data[nrow(trace_data), ]
            p <- p + geom_point(
              data = end_data,
              aes(x = x1, y = x2),
              color = trace$marker_color,
              size = trace$marker_size + 1,
              shape = 23,  # diamond
              fill = trace$marker_color,
              alpha = 1,
              inherit.aes = FALSE
            )
          }
        }
      }

      # apply theme
      theme_fun <- switch(theme,
        "minimal" = ggplot2::theme_minimal,
        "bw" = ggplot2::theme_bw,
        "classic" = ggplot2::theme_classic,
        "gray" = ggplot2::theme_gray,
        "light" = ggplot2::theme_light,
        "dark" = ggplot2::theme_dark,
        "void" = ggplot2::theme_void
      )
      p <- p + theme_fun(base_size = text_size)

      return(p)
    },

    #' @description
    #' Initialize contour layer (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    init_layer_contour = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },

    #' @description
    #' Initialize surface layer (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    init_layer_surface = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
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

      # Generate random color if not provided - convert from RGB format to R hex format
      if (is.null(line_color)) {
        # Use a different approach for ggplot2 - we'll use a predefined palette
        trace_num <- length(private$.optimization_traces) + 1
        # Use colorbrewer-style colors that are distinguishable
        colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
        line_color <- colors[((trace_num - 1) %% length(colors)) + 1]
      }

      if (is.null(marker_color)) marker_color <- line_color
      if (is.null(name)) {
        name <- paste0(optimizer$id, " on ", optimizer$objective$id)
      }

      # Extract trace data from optimizer archive
      xmat <- do.call(rbind, c(optimizer$archive$x_in[1], optimizer$archive$x_out))
      trace_data <- data.frame(
        x1 = xmat[, 1],
        x2 = xmat[, 2],
        iteration = seq_len(nrow(xmat)),
        stringsAsFactors = FALSE
      )

      # Apply point filtering if requested
      if (!is.null(npoints) || !is.null(npmax)) {
        if (is.null(npoints)) npoints <- nrow(trace_data)
        if (is.null(npmax)) npmax <- nrow(trace_data)
        npmax <- min(npmax, nrow(trace_data))
        
        # Subsample points
        indices <- unique(round(seq(1, nrow(trace_data), length.out = npoints)))
        indices <- indices[indices <= npmax]
        trace_data <- trace_data[indices, ]
        
        # Adjust marker positions
        if (!is.null(add_marker_at)) {
          add_marker_at <- add_marker_at[add_marker_at <= npmax]
        }
      }

      # Store trace information
      trace_info <- list(
        data = trace_data,
        line_color = line_color,
        line_width = line_width,
        line_type = line_type,
        name = name,
        add_marker_at = add_marker_at,
        marker_size = marker_size,
        marker_shape = marker_shape,
        marker_color = marker_color,
        show_start_end = show_start_end,
        alpha = alpha
      )

      private$.optimization_traces <- c(private$.optimization_traces, list(trace_info))
      
      invisible(self)
    },

    #' @description
    #' Set layout (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    set_layout = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },

    #' @description
    #' Set scene (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    set_scene = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },

    #' @description
    #' Add layer taylor (compatibility method, no-op for ggplot2 version).
    #' @template param_dots_compatibility
    add_layer_taylor = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    },

    #' @description
    #' Add layer hessian (compatibility method, no-op for ggplot2 version).
    #' @param ... Additional arguments (ignored for compatibility).
    add_layer_hessian = function(...) {
      # No-op for compatibility with plotly version
      invisible(self)
    }
  ),
  private = list(
    .decision_threshold = NULL,
    .optimization_traces = list()
  )
)
