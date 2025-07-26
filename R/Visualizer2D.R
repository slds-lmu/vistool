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
    #' @param title_size (`numeric(1)`)\cr
    #'   Title text size. If NULL, defaults to text_size + 2.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param background (`character(1)`)\cr
    #'   Background color for the plot. Default is "white".
    #' @param color_palette (`character(1)`)\cr
    #'   Color palette for the fill scale. One of "viridis", "plasma", "grayscale". Default is "viridis".
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
    #' @return A ggplot2 object.
    plot = function(text_size = 11, title_size = NULL, theme = "minimal", background = "white", color_palette = "viridis",
                    plot_title = NULL, plot_subtitle = NULL, x_lab = NULL, y_lab = NULL, 
                    x_limits = NULL, y_limits = NULL, show_grid = TRUE, grid_color = "gray90",
                    show_legend = TRUE, legend_position = "right", legend_title = NULL) {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_number(title_size, lower = 1, null.ok = TRUE)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      checkmate::assert_string(background)
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
      
      # Set default title size
      if (is.null(title_size)) title_size <- text_size + 2
      
      data <- data.table(
        fun_x1 = self$fun_x1,
        fun_x2 = self$fun_x2,
        fun_y = self$fun_y
      )

      # Determine final labels
      final_title <- if (!is.null(plot_title)) plot_title else self$title
      final_x_lab <- if (!is.null(x_lab)) x_lab else self$lab_x1
      final_y_lab <- if (!is.null(y_lab)) y_lab else self$lab_x2
      final_legend_title <- if (!is.null(legend_title)) legend_title else self$lab_y

      # Create base plot with continuous color gradient and overlaid contours
      p <- ggplot(data, aes(x = fun_x1, y = fun_x2)) +
        geom_raster(aes(fill = fun_y), interpolate = TRUE) +
        geom_contour(aes(z = fun_y), color = "white", alpha = 0.3) +
        labs(title = final_title, subtitle = plot_subtitle, x = final_x_lab, y = final_y_lab)
      
      # Apply axis limits if specified
      if (!is.null(x_limits)) {
        p <- p + ggplot2::xlim(x_limits[1], x_limits[2])
      }
      if (!is.null(y_limits)) {
        p <- p + ggplot2::ylim(y_limits[1], y_limits[2])
      }
      
      # Apply color scale based on palette choice
      if (color_palette == "viridis") {
        p <- p + scale_fill_viridis_c(name = final_legend_title)
      } else if (color_palette == "plasma") {
        p <- p + scale_fill_viridis_c(name = final_legend_title, option = "plasma")
      } else if (color_palette == "grayscale") {
        p <- p + scale_fill_gradient(name = final_legend_title, low = "black", high = "white")
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
      p <- p + theme_fun(base_size = text_size) + 
           theme(
             plot.title = ggplot2::element_text(hjust = 0.5, size = title_size),
             panel.background = ggplot2::element_rect(fill = background, color = NA),
             legend.position = if (show_legend && legend_position != "none") legend_position else "none",
             panel.grid = if (show_grid) ggplot2::element_line(color = grid_color) else ggplot2::element_blank()
           )
      
      # Add points from add_points() method
      p <- private$add_points_to_ggplot(p, "2D")

      return(p)
    }
  ),
  private = list(
    # Base class for 2D visualizers - no private fields needed
  )
)
