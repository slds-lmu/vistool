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

    #' @field points_x (`numeric(m)`)\cr
    #' x-values of extra points to plot.
    #' Use NULL if no points should be plotted.
    points_x = NULL,

    #' @field points_y (`numeric(m)`)\cr
    #' y-values of extra points to plot.
    #' Use NULL if no points should be plotted.
    points_y = NULL,

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

    # FIXME: add point-size, point col, point-symbol

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
    #' @param points_x (`numeric()`)\cr
    #'   x-values of extra points to plot.
    #'   Use NULL if no points should be plotted.
    #' @param points_y (`numeric()`)\cr
    #'   y-values of extra points to plot.
    #'   Use NULL if no points should be plotted.
    initialize = function(fun_x,
                          fun_y,
                          title = NULL,
                          lab_x = "x",
                          lab_y = "y",
                          points_x = NULL,
                          points_y = NULL) {
      self$fun_x <- checkmate::assert_numeric(fun_x)
      self$fun_y <- checkmate::assert_numeric(fun_y)
      self$title <- checkmate::assert_character(title, null.ok = TRUE)
      self$lab_x <- checkmate::assert_character(lab_x)
      self$lab_y <- checkmate::assert_character(lab_y)
      self$points_x <- checkmate::assert_numeric(points_x, null.ok = TRUE)
      self$points_y <- checkmate::assert_numeric(points_y, null.ok = TRUE)
      self$line_type <- "solid"
      self$line_col <- "red"
      self$line_width <- 3
      self$points_shape <- 19
      self$points_col <- "black"
      self$points_size <- 2
      self$points_alpha <- 0.3
    },

    # FIXME: set better defaults here to make plot nicer, maybe ask lukas

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
      
      dd <- data.frame(x = self$fun_x, y = self$fun_y)
      pl <- ggplot(data = dd, aes(x = x, y = y))
      pl <- pl + geom_line(linewidth = self$line_width, col = self$line_col, linetype = self$line_type)
      # use specified axis labels and legend title
      pl <- pl + labs(title = self$title, x = self$lab_x, y = self$lab_y)
      if (!is.null(self$points_x)) {
        dd2 <- data.frame(x = self$points_x, y = self$points_y)
        pl <- pl + geom_point(
          data = dd2, size = self$points_size, col = self$points_col,
          shape = self$points_shape, alpha = self$points_alpha
        )
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
      pl <- pl + theme_fun(base_size = text_size) + theme(plot.title = ggplot2::element_text(hjust = 0.5))
      
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
