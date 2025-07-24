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

      # continuous color gradient with overlaid contours
      p <- ggplot(data, aes(x = fun_x1, y = fun_x2)) +
        geom_raster(aes(fill = fun_y), interpolate = TRUE) +
        geom_contour(aes(z = fun_y), color = "white", alpha = 0.3) +
        labs(title = self$title, x = self$lab_x1, y = self$lab_x2) +
        scale_fill_viridis_c(name = self$lab_y)

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
      p <- p + theme_fun(base_size = text_size) + theme(plot.title = ggplot2::element_text(hjust = 0.5))
      
      # Add points from add_points() method
      p <- private$add_points_to_ggplot(p, "2D")

      return(p)
    }
  ),
  private = list(
    # Base class for 2D visualizers - no private fields needed
  )
)
