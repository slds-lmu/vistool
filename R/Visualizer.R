#' @title Base Visualizer Class
#'
#' @description
#' Base class for all visualizers. Provides a common interface for creating
#' and saving plots across different plotting backends (ggplot2 for 1D/2D, plotly for 3D).
#'
#' @export
Visualizer <- R6::R6Class("Visualizer",
  public = list(

    #' @description
    #' Abstract method to be implemented by subclasses.
    #' @return The plot object.
    plot = function() {
      stop("Abstract method 'plot' must be implemented by subclass")
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

      # Get the plot object
      plot_obj <- self$plot()

      # Check if it's a ggplot2 or plotly object and save accordingly
      if (inherits(plot_obj, "ggplot")) {
        private$save_ggplot(plot_obj, filename, width, height, dpi, ...)
      } else if (inherits(plot_obj, "plotly")) {
        private$save_plotly(plot_obj, filename, width, height, ...)
      } else {
        stop("Unknown plot type. Cannot save plot.")
      }

      invisible(self)
    }
  ),
  private = list(

    # Save a ggplot2 object
    save_ggplot = function(plot_obj, filename, width, height, dpi, ...) {
      # Default dimensions for ggplot2 (in inches)
      if (is.null(width)) width <- 10
      if (is.null(height)) height <- 6

      ggplot2::ggsave(
        filename = filename,
        plot = plot_obj,
        width = width,
        height = height,
        dpi = dpi,
        ...
      )
    },

    # Save a plotly object
    save_plotly = function(plot_obj, filename, width, height, ...) {
      # Default dimensions for plotly (in pixels)
      if (is.null(width)) width <- 800
      if (is.null(height)) height <- 600

      plotly::save_image(
        p = plot_obj,
        file = filename,
        width = width,
        height = height,
        ...
      )
    }
  )
)
