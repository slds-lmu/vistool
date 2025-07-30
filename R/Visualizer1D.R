#' @title Visualize 1D Functions
#'
#' @description
#' Visualizes a one-dimensional function \eqn{f: \mathbb{R} \to \mathbb{R}}.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
Visualizer1D <- R6::R6Class("Visualizer1D",
  inherit = Visualizer,
  public = list(

    #' @field fun_x (`numeric(n)`)\cr
    #' x-values of function
    fun_x = NULL,

    #' @field fun_y (`numeric(n)`)\cr
    #' y-values of function
    fun_y = NULL,

    #' @field title (`character(1)`)\cr
    #' Title of plot
    title = NULL,

    #' @field lab_x (`character(1)`)\cr
    #' Label of x-axis
    lab_x = NULL,
    # FIXME: make consistent names with other visualizers

    #' @field lab_y (`character(1)`)\cr
    #' Label of y-axis
    lab_y = NULL,

    #' @field line_col (`character(1)`)\cr
    #' Color of plotted line
    line_col = NULL,

    #' @field line_width (`numeric(1)`)\cr
    #' Width of plotted line
    line_width = NULL,

    #' @field line_type (`character(1)`)\cr
    #' Type of plotted line
    line_type = NULL,

    #' @field points_col (`character(1)`)\cr
    #' Color of plotted points
    points_col = NULL,

    #' @field points_size (`numeric(1)`)\cr
    #' Size of plotted points
    points_size = NULL,

    #' @field points_shape (`integer(1)`)\cr
    #'  Shape of plotted points
    points_shape = NULL,

    #' @field points_alpha (`numeric(1)`)\cr
    #'  Alpha blending of plotted points
    points_alpha = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param fun_x (`numeric()`)\cr
    #'   x-values of function
    #' @param fun_y (`numeric()`)\cr
    #'   y-values of function
    #' @param title (`character(1)`)\cr
    #'   Title of plot
    #' @param lab_x (`character(1)`)\cr
    #'   Label of x-axis
    #' @param lab_y (`character(1)`)\cr
    #'   Label of y-axis
    initialize = function(fun_x,
                          fun_y,
                          title = NULL,
                          lab_x = "x",
                          lab_y = "y") {
      self$fun_x <- checkmate::assert_numeric(fun_x)
      self$fun_y <- checkmate::assert_numeric(fun_y)
      self$title <- checkmate::assert_character(title, null.ok = TRUE)
      self$lab_x <- checkmate::assert_character(lab_x)
      self$lab_y <- checkmate::assert_character(lab_y)
      self$line_type <- "solid"
      self$line_col <- "red"
      self$line_width <- 3
    },

    # FIXME: set better defaults here to make plot nicer, maybe ask lukas

    #' @description
    #' Create and return the ggplot2 plot.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param color_palette (`character(1)`)\cr
    #'   Default color palette to use. One of "viridis", "plasma", "grayscale". Default is "viridis".
    #' @template param_plot_title
    #' @template param_plot_subtitle
    #' @template param_x_lab
    #' @template param_y_lab
    #' @template param_x_limits
    #' @template param_y_limits
    #' @template param_show_grid
    #' @template param_grid_color
    #' @template param_show_legend
    #' @template param_legend_position
    #' @template param_legend_title
    #' @template param_show_title
    #' @param ... Additional arguments for future extensibility.
    #' @return A ggplot2 object.
    plot = function(text_size = 11, theme = "minimal", color_palette = "viridis", plot_title = NULL, plot_subtitle = NULL, 
                    x_lab = NULL, y_lab = NULL, x_limits = NULL, y_limits = NULL, 
                    show_grid = TRUE, grid_color = "gray90", show_legend = TRUE, 
                    legend_position = "right", legend_title = NULL, show_title = TRUE, ...) {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      checkmate::assert_choice(color_palette, choices = c("viridis", "plasma", "grayscale"))
      checkmate::assert_string(plot_title, null.ok = TRUE)
      checkmate::assert_string(plot_subtitle, null.ok = TRUE)
      checkmate::assert_string(x_lab, null.ok = TRUE)
      checkmate::assert_string(y_lab, null.ok = TRUE)
      checkmate::assert_numeric(x_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(y_limits, len = 2, null.ok = TRUE)
      checkmate::assert_flag(show_grid)
      checkmate::assert_string(grid_color)
      checkmate::assert_flag(show_legend)
      checkmate::assert_choice(legend_position, choices = c("top", "right", "bottom", "left", "none"))
      checkmate::assert_string(legend_title, null.ok = TRUE)
      checkmate::assert_flag(show_title)
      
      # Store plot settings and resolve layer colors
      private$.plot_settings <- list(
        text_size = text_size,
        theme = theme,
        color_palette = color_palette,
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        x_lab = x_lab,
        y_lab = y_lab,
        x_limits = x_limits,
        y_limits = y_limits,
        show_grid = show_grid,
        grid_color = grid_color,
        show_legend = show_legend,
        legend_position = legend_position,
        legend_title = legend_title,
        show_title = show_title
      )
      
      # Resolve layer colors now that we have plot settings
      private$resolve_layer_colors()
      
      dd <- data.frame(x = self$fun_x, y = self$fun_y)
      pl <- ggplot(data = dd, aes(x = x, y = y))
      pl <- pl + geom_line(linewidth = self$line_width, col = self$line_col, linetype = self$line_type)
      
      # use specified axis labels and legend title
      final_title <- if (show_title) {
        if (!is.null(plot_title)) plot_title else self$title
      } else {
        NULL
      }
      final_x_lab <- if (!is.null(x_lab)) x_lab else self$lab_x
      final_y_lab <- if (!is.null(y_lab)) y_lab else self$lab_y
      
      pl <- pl + labs(title = final_title, subtitle = plot_subtitle, x = final_x_lab, y = final_y_lab)
      
      # Apply axis limits if specified
      if (!is.null(x_limits)) {
        pl <- pl + ggplot2::xlim(x_limits[1], x_limits[2])
      }
      if (!is.null(y_limits)) {
        pl <- pl + ggplot2::ylim(y_limits[1], y_limits[2])
      }
      
      
      # Add points from add_points() method
      pl <- private$add_points_to_ggplot(pl, "1D")
      
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
      pl <- pl + theme_fun(base_size = text_size) + 
            theme(
              plot.title = ggplot2::element_text(hjust = 0.5),
              legend.position = if (show_legend && legend_position != "none") legend_position else "none",
              panel.grid = if (show_grid) ggplot2::element_line(color = grid_color) else ggplot2::element_blank()
            )
      
      return(pl)
    }
  ),
  private = list(
    # Override infer_z_values for 1D - not needed, but handle y values
    infer_z_values = function(points_data) {
      # For 1D visualizers, we might need to infer y values from the function
      if (all(is.na(points_data$y)) && "x" %in% names(points_data)) {
        # Try to interpolate y values from the existing function data
        points_data$y <- stats::approx(self$fun_x, self$fun_y, points_data$x, rule = 2)$y
      }
      return(points_data$y)
    },

    # Override prepare_points_data to handle 1D-specific y value inference
    prepare_points_data = function(points, visualizer_type) {
      points_data <- super$prepare_points_data(points, visualizer_type)
      
      # For 1D visualizers, try to infer y values if not provided
      if (visualizer_type == "1D" && all(is.na(points_data$y))) {
        points_data$y <- private$infer_z_values(points_data)
      }
      
      return(points_data)
    }
  )
)
