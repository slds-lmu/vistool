#' @title Base Visualizer Class
#'
#' @description
#' Base class for all visualizers. Provides a common interface for creating
#' and saving plots across different plotting backends (ggplot2 for 1D/2D, plotly for 3D).
#'
#' @export
Visualizer <- R6::R6Class("Visualizer",
  public = list(

    #' @field defaults (`list`)\cr
    #' Default settings for plotting and visual elements.
    defaults = list(),

    #' @description
    #' Initialize default settings for the visualizer.
    #' @param default_color_palette (`character(1)`)\cr
    #'   Default color palette. Default is "viridis".
    #' @param default_text_size (`numeric(1)`)\cr
    #'   Default text size. Default is 11.
    #' @param default_theme (`character(1)`)\cr
    #'   Default theme. Default is "minimal".
    #' @param default_alpha (`numeric(1)`)\cr
    #'   Default alpha transparency. Default is 0.8.
    #' @param default_line_width (`numeric(1)`)\cr
    #'   Default line width. Default is 1.2.
    #' @param default_point_size (`numeric(1)`)\cr
    #'   Default point size. Default is 2.
    initialize_defaults = function(default_color_palette = "viridis", default_text_size = 11,
                                  default_theme = "minimal", default_alpha = 0.8,
                                  default_line_width = 1.2, default_point_size = 2) {
      self$defaults <- list(
        color_palette = default_color_palette,
        text_size = default_text_size,
        theme = default_theme,
        alpha = default_alpha,
        line_width = default_line_width,
        point_size = default_point_size
      )
      invisible(self)
    },

    #' @description
    #' Abstract method to be implemented by subclasses.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param title_size (`numeric(1)`)\cr
    #'   Title text size. If NULL, defaults to text_size + 2.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "minimal".
    #' @param background (`character(1)`)\cr
    #'   Background color for the plot. Default is "white".
    #' @param color_palette (`character(1)`)\cr
    #'   Default color palette to use. One of "viridis", "plasma", "grayscale". Default is "viridis".
    #' @return The plot object.
    plot = function(text_size = 11, title_size = NULL, theme = "minimal", background = "white", color_palette = "viridis") {
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
    #'   Size of the points. Default is 2.
    #' @param shape (`integer(1)` or `character(1)`)\cr
    #'   Shape of the points. For ggplot2: integer codes (e.g., 19 for solid circle). For plotly: shape names. Default is 19/"circle".
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of the points. Default is 1.
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
    #' @param ... Additional arguments passed to the plotting layers.
    add_points = function(points, color = "auto", size = 2, shape = 19, alpha = 1, 
                         annotations = NULL, annotation_size = NULL, ordered = FALSE, 
                         arrow_color = NULL, arrow_size = 0.3, ...) {
      # Process color (handle "auto" assignment)
      processed_color <- process_color(color, self)
      
      # Use processed color for arrows if not specified
      if (is.null(arrow_color)) arrow_color <- processed_color
      
      private$.points_to_add <- c(private$.points_to_add,
        list(list(
          points = points, color = processed_color, size = size, shape = shape, alpha = alpha,
          annotations = annotations, annotation_size = annotation_size, ordered = ordered, 
          arrow_color = arrow_color, arrow_size = arrow_size, args = list(...)
        ))
      )
      invisible(self)
    }
  ),
  private = list(

    .points_to_add = list(),
    .color_index = 1,  # Track next color index for auto assignment

    # save a ggplot2 object
    save_ggplot = function(plot_obj, filename, width, height, dpi, ...) {
      # default dimensions for ggplot2
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

    # save a plotly object
    save_plotly = function(plot_obj, filename, width, height, ...) {
      # default dimensions for plotly (in pixels)
      if (is.null(width)) width <- 800
      if (is.null(height)) height <- 600

      plotly::save_image(
        p = plot_obj,
        file = filename,
        width = width,
        height = height,
        ...
      )
    },

    # Helper method to add points to ggplot2 objects
    add_points_to_ggplot = function(plot_obj, visualizer_type = "2D") {
      if (length(private$.points_to_add) == 0) {
        return(plot_obj)
      }

      for (point_set in private$.points_to_add) {
        points_data <- private$prepare_points_data(point_set$points, visualizer_type)
        
        # Add points layer
        plot_obj <- plot_obj + ggplot2::geom_point(
          data = points_data,
          ggplot2::aes(x = x, y = y),
          color = point_set$color,
          size = point_set$size,
          shape = point_set$shape,
          alpha = point_set$alpha,
          inherit.aes = FALSE
        )
        
        # Add annotations if provided
        if (!is.null(point_set$annotations)) {
          # Use annotation_size if provided, otherwise default to smaller text
          ann_size <- if (!is.null(point_set$annotation_size)) point_set$annotation_size else 3
          
          plot_obj <- plot_obj + ggplot2::geom_text(
            data = cbind(points_data, label = point_set$annotations),
            ggplot2::aes(x = x, y = y, label = label),
            color = point_set$color,
            size = ann_size,
            vjust = -0.5,
            inherit.aes = FALSE
          )
        }
        
        # Add ordered path with arrows if requested
        if (point_set$ordered && nrow(points_data) > 1) {
          # Calculate direction vectors and create shorter arrows
          for (i in 1:(nrow(points_data) - 1)) {
            x1 <- points_data$x[i]
            y1 <- points_data$y[i]
            x2 <- points_data$x[i + 1]
            y2 <- points_data$y[i + 1]
            
            # Calculate direction vector and normalize it
            dx <- x2 - x1
            dy <- y2 - y1
            dist <- sqrt(dx^2 + dy^2)
            
            if (dist > 0) {
              # Normalize direction vector
              dx_norm <- dx / dist
              dy_norm <- dy / dist
              
              # Calculate arrow endpoints based on arrow_size
              arrow_size <- point_set$arrow_size
              arrow_x2 <- x1 + dx_norm * arrow_size
              arrow_y2 <- y1 + dy_norm * arrow_size
              
              # Create arrow data frame
              arrow_data <- data.frame(
                x = x1,
                y = y1,
                xend = arrow_x2,
                yend = arrow_y2
              )
              
              # Add arrow segment
              plot_obj <- plot_obj + ggplot2::geom_segment(
                data = arrow_data,
                ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                color = point_set$arrow_color,
                alpha = point_set$alpha * 0.7,
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
      if (length(private$.points_to_add) == 0) {
        return(plot_obj)
      }

      for (point_set in private$.points_to_add) {
        points_data <- private$prepare_points_data(point_set$points, visualizer_type)
        
        if (visualizer_type == "surface") {
          # For 3D surface plots, need z values
          if (!"z" %in% names(points_data)) {
            # Try to infer z values if the visualizer has a way to evaluate the function
            points_data$z <- private$infer_z_values(points_data)
          }
          
          # Add 3D scatter trace
          plot_obj <- plot_obj %>% plotly::add_trace(
            x = points_data$x,
            y = points_data$y,
            z = points_data$z,
            type = "scatter3d",
            mode = "markers",
            marker = list(
              color = point_set$color,
              size = point_set$size,
              opacity = point_set$alpha
            ),
            name = "Added Points",
            showlegend = FALSE
          )
          
          # Add annotations if provided (3D text)
          if (!is.null(point_set$annotations)) {
            for (i in seq_along(point_set$annotations)) {
              plot_obj <- plot_obj %>% plotly::add_trace(
                x = points_data$x[i],
                y = points_data$y[i],
                z = points_data$z[i],
                type = "scatter3d",
                mode = "text",
                text = point_set$annotations[i],
                textfont = list(
                  color = point_set$color,
                  size = if (!is.null(point_set$annotation_size)) point_set$annotation_size else 12
                ),
                showlegend = FALSE
              )
            }
          }
          
          # Add ordered path if requested
          if (point_set$ordered && nrow(points_data) > 1) {
            # For 3D plots, we'll use lines but with shorter segments
            for (i in 1:(nrow(points_data) - 1)) {
              x1 <- points_data$x[i]
              y1 <- points_data$y[i]
              z1 <- points_data$z[i]
              x2 <- points_data$x[i + 1]
              y2 <- points_data$y[i + 1]
              z2 <- points_data$z[i + 1]
              
              # Calculate direction vector and normalize it
              dx <- x2 - x1
              dy <- y2 - y1
              dz <- z2 - z1
              dist <- sqrt(dx^2 + dy^2 + dz^2)
              
              if (dist > 0) {
                # Normalize direction vector
                dx_norm <- dx / dist
                dy_norm <- dy / dist
                dz_norm <- dz / dist
                
                # Calculate arrow endpoints based on arrow_size
                arrow_size <- point_set$arrow_size
                arrow_x2 <- x1 + dx_norm * arrow_size
                arrow_y2 <- y1 + dy_norm * arrow_size
                arrow_z2 <- z1 + dz_norm * arrow_size
                
                # Add arrow segment
                plot_obj <- plot_obj %>% plotly::add_trace(
                  x = c(x1, arrow_x2),
                  y = c(y1, arrow_y2),
                  z = c(z1, arrow_z2),
                  type = "scatter3d",
                  mode = "lines",
                  line = list(
                    color = point_set$arrow_color,
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
          plot_obj <- plot_obj %>% plotly::add_trace(
            x = points_data$x,
            y = points_data$y,
            type = "scatter",
            mode = "markers",
            marker = list(
              color = point_set$color,
              size = point_set$size,
              opacity = point_set$alpha
            ),
            name = "Added Points",
            showlegend = FALSE
          )
          
          # Add annotations if provided (2D text)
          if (!is.null(point_set$annotations)) {
            plot_obj <- plot_obj %>% plotly::add_annotations(
              x = points_data$x,
              y = points_data$y,
              text = point_set$annotations,
              showarrow = FALSE,
              font = list(
                color = point_set$color,
                size = if (!is.null(point_set$annotation_size)) point_set$annotation_size else 12
              )
            )
          }
          
          # Add ordered path if requested
          if (point_set$ordered && nrow(points_data) > 1) {
            # For 2D plots, use shorter line segments
            for (i in 1:(nrow(points_data) - 1)) {
              x1 <- points_data$x[i]
              y1 <- points_data$y[i]
              x2 <- points_data$x[i + 1]
              y2 <- points_data$y[i + 1]
              
              # Calculate direction vector and normalize it
              dx <- x2 - x1
              dy <- y2 - y1
              dist <- sqrt(dx^2 + dy^2)
              
              if (dist > 0) {
                # Normalize direction vector
                dx_norm <- dx / dist
                dy_norm <- dy / dist
                
                # Calculate arrow endpoints based on arrow_size
                arrow_size <- point_set$arrow_size
                arrow_x2 <- x1 + dx_norm * arrow_size
                arrow_y2 <- y1 + dy_norm * arrow_size
                
                # Add arrow segment  
                plot_obj <- plot_obj %>% plotly::add_trace(
                  x = c(x1, arrow_x2),
                  y = c(y1, arrow_y2),
                  type = "scatter",
                  mode = "lines",
                  line = list(
                    color = point_set$arrow_color,
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
        points_data <- data.frame(x = points, y = NA_real_)
      } else if (is.matrix(points) || is.data.frame(points)) {
        points_data <- as.data.frame(points)
        # Ensure consistent column names
        if (ncol(points_data) == 1 && visualizer_type == "1D") {
          names(points_data) <- "x"
          points_data$y <- NA_real_
        } else if (ncol(points_data) >= 2) {
          names(points_data)[1:2] <- c("x", "y")
          if (ncol(points_data) >= 3) {
            names(points_data)[3] <- "z"
          }
        }
      } else if (is.list(points) && !is.data.frame(points)) {
        # List of vectors - convert to data.frame
        if (all(sapply(points, length) == 2)) {
          points_data <- data.frame(
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
    }
  )
)
