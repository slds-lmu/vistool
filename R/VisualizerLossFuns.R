#' @title Visualizer for Loss Functions
#'
#' @description
#' Visualize one or multiple loss functions.
#'
#' @export
VisualizerLossFuns <- R6::R6Class("VisualizerLossFuns",
  inherit = Visualizer,
  public = list(

    #' @field losses (`list`)\cr
    #' List of LossFunction objects.
    losses = NULL,

    #' @field task_type (`character(1)`)\cr
    #' Task type (regr or classif).
    task_type = NULL,

    #' @field title (`character(1)`)\cr
    #' Title of plot.
    title = NULL,

    #' @field lab_x (`character(1)`)\cr
    #' Label of x-axis.
    lab_x = NULL,

    #' @field lab_y (`character(1)`)\cr
    #' Label of y-axis.
    lab_y = NULL,

    #' @field x_range (`numeric(2)`)\cr
    #' Range for x-axis values.
    x_range = NULL,

    #' @field line_width (`numeric()`)\cr
    #' Line widths for different loss functions.
    line_width = NULL,

    #' @field line_col (`character()`)\cr
    #' Line colors for different loss functions.
    line_col = NULL,

    #' @field line_type (`character()`)\cr
    #' Line types for different loss functions.
    line_type = NULL,

    #' @field legend_title (`character(1)`)\cr
    #' Legend title.
    legend_title = "Loss Function",

    #' @field y_pred (`numeric()`)\cr
    #' Predicted values.
    y_pred = NULL,

    #' @field y_true (`numeric()`)\cr
    #' True values.
    y_true = NULL,

    #' @field n_points (`integer(1)`)\cr
    #' Number of points to use for plotting the loss functions.
    n_points = NULL,

    #' @field input_type (`character(1)`)\cr
    #' Input scale for classification tasks: `"score"` (margin‑based, default) or `"probability"`.
    input_type = NULL,

    #' @field y_curves (`character(1)`)\cr
    #' Which response curve(s) to draw when `input_type = "probability"`.
    #' One of `"both"`, `"y1"`, or `"y0"`.
    y_curves = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param losses (`list`)\cr
    #'   List of LossFunction objects.
    #' @param y_pred (`numeric()`)\cr
    #'   Predicted values. Optional.
    #' @param y_true (`numeric()`)\cr
    #'   True values. Optional.
    #' @param n_points (`integer(1)`)\cr
    #'   Number of points to use for plotting the loss functions. Default is 1000.
    #' @param input_type (`character(1)`)\cr
    #'   Desired input scale. One of `"auto"`, `"score"`, `"probability"`.
    #'   `"auto"` (default) chooses the common `input_default` of the supplied
    #'   losses.
    #' @param y_curves (`character(1)`)\cr
    #'   When `input_type = "probability"`, choose which curves to display: `"both"`, `"y1"`, or `"y0"`.
    #' @template param_default_color_palette
    #' @template param_default_text_size
    #' @template param_default_theme
    #' @param default_alpha (`numeric(1)`)\cr
    #'   Default alpha transparency. Default is 0.8.
    #' @param default_line_width (`numeric(1)`)\cr
    #'   Default line width. Default is 1.2.
    #' @param default_point_size (`numeric(1)`)\cr
    #'   Default point size. Default is 2.
    #' @param ... Additional arguments (currently unused).
    initialize = function(losses, y_pred = NULL, y_true = NULL, n_points = 1000L,
                          input_type = "auto", y_curves = "both", 
                          default_color_palette = "viridis", default_text_size = 11, 
                          default_theme = "bw", default_alpha = 0.8, 
                          default_line_width = 1.2, default_point_size = 2, ...) {
      checkmate::assert_list(losses, "LossFunction")
      checkmate::assert_numeric(y_pred, null.ok = TRUE)
      checkmate::assert_numeric(y_true, null.ok = TRUE)
      checkmate::assert_integerish(n_points, lower = 10, len = 1)
      checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
      checkmate::assert_choice(y_curves, choices = c("both", "y1", "y0"))

      # Initialize parent defaults
      self$initialize_defaults(
        default_color_palette = default_color_palette,
        default_text_size = default_text_size,
        default_theme = default_theme,
        default_alpha = default_alpha,
        default_line_width = default_line_width,
        default_point_size = default_point_size
      )

      tts <- unique(sapply(losses, function(x) x$task_type))
      if (length(tts) > 1) {
        mlr3misc::stopf("'LossFunction$task_type' all need to be the same, but found: %s", mlr3misc::str_collapse(tts))
      }

      # resolve input_type = "auto"
      if (input_type == "auto") {
        defs <- vapply(losses, function(x) x$input_default, character(1))
        if (length(unique(defs)) != 1L) {
          stop("input_type = 'auto' cannot resolve because supplied losses have differing 'input_default'.")
        }
        input_type <- unique(defs)
      }

      # ensure every loss supports the requested scale
      ok <- vapply(losses, function(lf) input_type %in% lf$input_supported, logical(1))
      if (!all(ok)) {
        bad <- paste(names(losses)[!ok], collapse = ", ")
        stop(sprintf("The following losses do not support input_type = '%s': %s", input_type, bad))
      }

      ids <- vapply(losses, function(x) x$id, character(1))
      names(losses) <- ids
      self$losses <- losses
      self$task_type <- unique(tts)
      
      # Set up labels and ranges based on task type and input type
      if (self$task_type == "classif") {
        if (input_type == "score") {
          self$lab_x <- expression(y * f)
          self$x_range <- c(-5, 5)
        } else {
          self$lab_x <- expression(pi)
          self$x_range <- c(0, 1)
        }
      } else {
        self$lab_x <- expression(y - f)
        self$x_range <- c(-5, 5)
      }
      
      self$input_type <- input_type
      self$y_curves  <- y_curves
      n <- length(losses)
      self$line_width <- rep(0.5, n)
      self$line_col <- NULL
      self$line_type <- rep("solid", n)
      self$y_pred <- y_pred
      self$y_true <- y_true
      self$n_points <- as.integer(n_points)
      
      # Set up basic plot labels
      self$title <- ""
      self$lab_y <- "Loss"
    },

    #' @description
    #' Create and return a ggplot2 visualisation of the loss functions.
    #' For classification you can switch between **margin/score** and
    #' **probability** representations via `input_type`.  When
    #' `input_type = "probability"` the argument `y_curves` controls whether the
    #' curve for the positive class (`"y1"`), the negative class (`"y0"`), or
    #' *both* are shown.
    #'
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light",
    #'   "dark", "void". Default is "bw".
    #' @param color_palette (`character(1)`)\cr
    #'   Color palette for visualizations. One of "viridis", "plasma", "grayscale". Default is "viridis".
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
    #' @return A ggplot2 object.
    plot = function(text_size = NULL, theme = NULL, color_palette = "viridis", plot_title = NULL, plot_subtitle = NULL, 
                    x_lab = NULL, y_lab = NULL, x_limits = NULL, y_limits = NULL, 
                    show_grid = TRUE, grid_color = "gray90", show_legend = TRUE, 
                    legend_position = "right", legend_title = NULL, show_title = TRUE) {
      
      # Use stored defaults if parameters are not provided
      if (is.null(text_size)) text_size <- if (is.null(self$defaults$text_size)) 11 else self$defaults$text_size
      if (is.null(theme)) theme <- if (is.null(self$defaults$theme)) "bw" else self$defaults$theme
      
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

      # Store plot settings for layer resolution
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
      
      # Render the plot
      return(private$render_plot())
    },

    #' @description
    #' Add points to the loss function visualization with optional vertical lines.
    #' Points are automatically positioned on the loss curve by evaluating the
    #' selected loss function at the given x-coordinates.
    #'
    #' @param x (`numeric()`)\cr
    #'   x-coordinates where to place points on the loss curves. These represent
    #'   the residual values (y-f for regression, y*f for classification scores,
    #'   or probabilities for classification probability mode).
    #' @param loss_id (`character(1)`)\cr
    #'   ID of the loss function to use for y-value calculation. If NULL (default),
    #'   uses the first loss function.
    #' @param show_line (`logical(1)`)\cr
    #'   If TRUE (default), draws vertical lines from points to x-axis.
    #' @param color (`character(1)`)\cr
    #'   Color of the points and lines. Use "auto" for automatic color assignment.
    #'   Default is "red".
    #' @param size (`numeric(1)`)\cr
    #'   Size of the points. Default is 3.
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of points and lines. Default is 0.8.
    #' @param line_color (`character(1)`)\cr
    #'   Color of vertical lines. If NULL, uses the same color as points.
    #' @param line_alpha (`numeric(1)`)\cr
    #'   Alpha transparency of vertical lines. If NULL, uses alpha * 0.7.
    #' @param ... Additional arguments passed to point and line geoms.
    add_points = function(x, loss_id = NULL, show_line = TRUE, color = "red", size = 3, alpha = 0.8,
                         line_color = NULL, line_alpha = NULL, ...) {
      checkmate::assert_numeric(x)
      checkmate::assert_string(loss_id, null.ok = TRUE)
      checkmate::assert_flag(show_line)
      checkmate::assert_string(color)
      checkmate::assert_number(size, lower = 0)
      checkmate::assert_number(alpha, lower = 0, upper = 1)
      checkmate::assert_string(line_color, null.ok = TRUE)
      checkmate::assert_number(line_alpha, lower = 0, upper = 1, null.ok = TRUE)
      
      # Store layer specification using layer system
      private$store_layer("loss_points", list(
        x = x, loss_id = loss_id, show_line = show_line, color = color, size = size, alpha = alpha,
        line_color = line_color, line_alpha = line_alpha, args = list(...)
      ))
      invisible(self)
    }
  ),
  private = list(
    # Override infer_z_values to handle loss function evaluation
    infer_z_values = function(points_data) {
      # For loss function visualizers, we can evaluate the loss at given points
      # if we have the necessary data
      if (all(is.na(points_data$y)) && "x" %in% names(points_data)) {
        # Try to evaluate the first loss function at the given x values
        if (length(self$losses) > 0) {
          first_loss <- self$losses[[1]]
          loss_fun <- first_loss$get_fun(self$input_type)
          points_data$y <- loss_fun(points_data$x)
        } else {
          # Fallback to zeros if no loss functions available
          points_data$y <- rep(0, nrow(points_data))
        }
      }
      return(points_data$y)
    },

    # Override prepare_points_data to handle loss function-specific point preparation
    prepare_points_data = function(points, visualizer_type) {
      points_data <- super$prepare_points_data(points, visualizer_type)
      
      # For loss function visualizers, try to infer y values if not provided
      if (visualizer_type == "1D" && all(is.na(points_data$y))) {
        points_data$y <- private$infer_z_values(points_data)
      }
      
      return(points_data)
    },

    # Render the complete plot using stored settings and layers
    render_plot = function() {
      settings <- private$.plot_settings
      
      # Determine final labels
      final_title <- if (settings$show_title) {
        if (!is.null(settings$plot_title)) settings$plot_title else self$title
      } else {
        NULL
      }
      final_x_lab <- if (!is.null(settings$x_lab)) settings$x_lab else self$lab_x
      final_y_lab <- if (!is.null(settings$y_lab)) settings$y_lab else self$lab_y
      final_legend_title <- if (!is.null(settings$legend_title)) settings$legend_title else self$legend_title
      
      # Create the base plot with loss function curves
      pl <- private$render_loss_curves()
      
      # Add stored layers
      pl <- private$render_stored_layers(pl)
      
      # Apply final styling
      theme_fun <- switch(settings$theme,
        "minimal" = ggplot2::theme_minimal,
        "bw"      = ggplot2::theme_bw,
        "classic" = ggplot2::theme_classic,
        "gray"    = ggplot2::theme_gray,
        "light"   = ggplot2::theme_light,
        "dark"    = ggplot2::theme_dark,
        "void"    = ggplot2::theme_void
      )
      pl <- pl + theme_fun(base_size = settings$text_size) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5),
          legend.position = if (settings$show_legend && settings$legend_position != "none") settings$legend_position else "none",
          panel.grid = if (settings$show_grid) ggplot2::element_line(color = settings$grid_color) else ggplot2::element_blank()
        )

      pl <- pl + ggplot2::labs(title = final_title, subtitle = settings$plot_subtitle, x = final_x_lab, y = final_y_lab)
      
      # Apply axis limits if specified
      if (!is.null(settings$x_limits)) {
        pl <- pl + ggplot2::xlim(settings$x_limits[1], settings$x_limits[2])
      }
      if (!is.null(settings$y_limits)) {
        pl <- pl + ggplot2::ylim(settings$y_limits[1], settings$y_limits[2])
      }

      return(pl)
    },

    # Render the base loss function curves
    render_loss_curves = function() {
      settings <- private$.plot_settings
      final_legend_title <- if (!is.null(settings$legend_title)) settings$legend_title else self$legend_title
      loss_labels <- sapply(self$losses, function(x) x$label)

      if (self$task_type == "classif" && self$input_type == "probability") {
        # ---- probability based visualisation ----
        if (!is.null(self$y_pred)) {
          p_min <- max(min(self$y_pred), 0)
          p_max <- min(max(self$y_pred), 1)
          r_seq <- seq(p_min, p_max, length.out = self$n_points)
        } else {
          r_seq <- seq(self$x_range[1], self$x_range[2], length.out = self$n_points)
        }

        y_set <- switch(self$y_curves,
          "both" = c("y = 1", "y = 0"),
          "y1"   = c("y = 1"),
          "y0"   = c("y = 0")
        )

        data_list <- list()
        for (lf in self$losses) {
          loss_fun_for_input <- lf$get_fun(self$input_type)
          if ("y = 1" %in% y_set) {
            data_list[[length(data_list) + 1L]] <- data.table::data.table(
              r        = r_seq,
              loss_val = loss_fun_for_input(r_seq),
              loss_fun = lf$id,
              y_val    = "y = 1"
            )
          }
          if ("y = 0" %in% y_set) {
            data_list[[length(data_list) + 1L]] <- data.table::data.table(
              r        = r_seq,
              loss_val = loss_fun_for_input(1 - r_seq),
              loss_fun = lf$id,
              y_val    = "y = 0"
            )
          }
        }
        dd <- data.table::rbindlist(data_list)

        pl <- ggplot2::ggplot(
          data = dd,
          ggplot2::aes(x = r, y = loss_val,
                       col = loss_fun,
                       linetype = y_val,
                       linewidth = loss_fun)
        ) +
          ggplot2::geom_line()

      } else {
        # ---- regression or score/margin classification ----
        if (!is.null(self$y_pred) && !is.null(self$y_true)) {
          if (self$task_type == "classif") {
            residuals <- self$y_true * self$y_pred
          } else {
            residuals <- self$y_true - self$y_pred
          }
          r_seq <- seq(min(residuals), max(residuals), length.out = self$n_points)
        } else {
          r_seq <- seq(self$x_range[1], self$x_range[2], length.out = self$n_points)
        }

        loss_seqs <- data.table::as.data.table(lapply(self$losses, function(ll) {
          loss_fun_for_input <- ll$get_fun(self$input_type)
          loss_fun_for_input(r_seq)
        }))
        dd <- cbind(r = r_seq, loss_seqs)
        dd <- data.table::melt(dd,
          id.vars = "r", measure.vars = colnames(loss_seqs),
          variable.name = "loss_fun", value.name = "loss_val"
        )

        pl <- ggplot2::ggplot(
          data = dd,
          ggplot2::aes(x = r, y = loss_val,
                       col = loss_fun,
                       linetype = loss_fun,
                       linewidth = loss_fun)
        ) +
          ggplot2::geom_line()
      }

      # ---- shared styling ----
      if (!is.null(self$line_col)) {
        color_values <- self$line_col
      } else {
        n_losses <- length(unique(dd$loss_fun))
        if (n_losses == 1) {
          color_values <- "#FF8C00"
        } else if (n_losses == 2) {
          color_values <- c("#FF8C00", "#1E90FF")
        } else if (n_losses == 3) {
          color_values <- c("#FF8C00", "#1E90FF", "#32CD32")
        } else if (n_losses == 4) {
          color_values <- c("#FF8C00", "#1E90FF", "#32CD32", "#DC143C")
        } else {
          if (requireNamespace("ggsci", quietly = TRUE)) {
            color_values <- ggsci::pal_npg("nrc")(n_losses)
          } else {
            color_values <- grDevices::rainbow(n_losses, start = 0, end = 0.8)
          }
        }
      }
      pl <- pl + ggplot2::scale_color_manual(values = color_values, labels = loss_labels, name = final_legend_title)

      if (!is.null(self$line_width)) {
        pl <- pl + ggplot2::scale_linewidth_manual(values = self$line_width, labels = loss_labels, name = final_legend_title)
      } else {
        pl <- pl + ggplot2::scale_linewidth_manual(values = rep(1.2, length(unique(dd$loss_fun))), labels = loss_labels, name = final_legend_title)
      }

      if (!(self$task_type == "classif" && self$input_type == "probability")) {
        if (!is.null(self$line_type)) {
          pl <- pl + ggplot2::scale_linetype_manual(values = self$line_type, labels = loss_labels, name = final_legend_title)
        }
      } else {
        if (length(unique(dd$y_val)) == 1L) {
          pl <- pl + ggplot2::scale_linetype_manual(values = "solid", labels = unique(dd$y_val), name = "Class")
        } else {
          # Get the unique y_val levels in the order they will appear as factor levels
          y_levels <- sort(unique(dd$y_val))  # This gives alphabetical order like factor()
          # Create corresponding line types: solid for y=1, dashed for y=0
          line_types <- ifelse(y_levels == "y = 1", "solid", "dashed")
          pl <- pl + ggplot2::scale_linetype_manual(values = line_types, labels = y_levels, name = "Class")
        }
      }

      return(pl)
    },

    # Render all stored layers
    render_stored_layers = function(plot_obj) {
      # First add the generic points layers from base class
      plot_obj <- private$add_points_to_ggplot(plot_obj, "1D")
      
      # Then add loss-specific point layers
      loss_points_layers <- private$get_layers_by_type("loss_points")
      for (point_spec in loss_points_layers) {
        plot_obj <- private$render_loss_points_layer(plot_obj, point_spec)
      }
      
      return(plot_obj)
    },

    # Render a single loss points layer
    render_loss_points_layer = function(plot_obj, point_spec) {
      # Determine which loss function to use
      if (is.null(point_spec$loss_id)) {
        if (length(self$losses) == 0) {
          stop("No loss functions available and no loss_id specified")
        }
        loss_fun <- self$losses[[1]]$get_fun(self$input_type)
      } else {
        if (!point_spec$loss_id %in% names(self$losses)) {
          stop(sprintf("Loss function '%s' not found", point_spec$loss_id))
        }
        loss_fun <- self$losses[[point_spec$loss_id]]$get_fun(self$input_type)
      }
      
      # Calculate y-values by evaluating the loss function
      y_vals <- loss_fun(point_spec$x)
      
      # Create points data
      points_data <- data.frame(
        x = point_spec$x,
        y = y_vals
      )
      
      # Add points
      plot_obj <- plot_obj + ggplot2::geom_point(
        data = points_data,
        ggplot2::aes(x = x, y = y),
        color = point_spec$color,
        size = point_spec$size,
        alpha = point_spec$alpha,
        shape = 21,  # Circle with border and fill
        fill = "white",  # Hollow center
        stroke = 1,  # Border thickness
        inherit.aes = FALSE
      )
      
      # Add vertical lines if requested
      if (point_spec$show_line) {
        line_color <- if (is.null(point_spec$line_color)) point_spec$color else point_spec$line_color
        line_alpha <- if (is.null(point_spec$line_alpha)) point_spec$alpha * 0.7 else point_spec$line_alpha
        
        # Create line segments from points to x-axis
        line_data <- data.frame(
          x = point_spec$x,
          y = y_vals,
          xend = point_spec$x,
          yend = 0
        )
        
        plot_obj <- plot_obj + ggplot2::geom_segment(
          data = line_data,
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          color = line_color,
          alpha = line_alpha,
          linetype = "dashed",
          inherit.aes = FALSE
        )
      }
      
      return(plot_obj)
    }
  )
)
