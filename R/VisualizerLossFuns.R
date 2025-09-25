#' @title Visualizer for Loss Functions
#'
#' @description
#' Visualize one or multiple loss functions.
#'
#' @export
VisualizerLossFuns = R6::R6Class("VisualizerLossFuns",
  inherit = Visualizer,
  public = list(

    #' @field losses (`list`)\cr
    #' List of LossFunction objects.
    losses = NULL,

    #' @field task_type (`character(1)`)\cr
    #' Task type (regr or classif).
    task_type = NULL,

    #' @field y_pred (`numeric()`)\cr
    #' Predicted values.
    y_pred = NULL,

    #' @field y_true (`numeric()`)\cr
    #' True values.
    y_true = NULL,

    #' @field input_type (`character(1)`)\cr
    #' Input scale for classification tasks: `"score"` (margin‑based, default) or `"probability"`.
    input_type = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param losses (`list`)\cr
    #'   List of LossFunction objects.
    #' @param y_pred (`numeric()`)\cr
    #'   Predicted values. Optional.
    #' @param y_true (`numeric()`)\cr
    #'   True values. Optional.
    #' @param input_type (`character(1)`)\cr
    #'   Desired input scale. One of `"auto"`, `"score"`, `"probability"`.
    #'   `"auto"` (default) chooses the common `input_default` of the supplied
    #'   losses.
    #' @param n_points (`integer(1)`)
    #'   Default resolution used when calling `$plot()` without specifying `n_points`.
    #'   Defaults to 1000.
    #' @param ... Additional arguments (currently unused).
    initialize = function(losses, y_pred = NULL, y_true = NULL,
                          input_type = "auto", n_points = 1000L, ...) {
      checkmate::assert_list(losses, "LossFunction")
      checkmate::assert_numeric(y_pred, null.ok = TRUE)
      checkmate::assert_numeric(y_true, null.ok = TRUE)
      checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
      n_points = as.integer(checkmate::assert_integerish(n_points, len = 1, lower = 10))

      # Theme defaults are handled by base Visualizer via set_theme()/plot(theme=...)

      tts = unique(sapply(losses, function(x) x$task_type))
      if (length(tts) > 1) {
        mlr3misc::stopf("'LossFunction$task_type' all need to be the same, but found: %s", mlr3misc::str_collapse(tts))
      }

      # resolve input_type = "auto"
      if (input_type == "auto") {
        defs = vapply(losses, function(x) x$input_default, character(1))
        if (length(unique(defs)) != 1L) {
          stop("input_type = 'auto' cannot resolve because supplied losses have differing 'input_default'.")
        }
        input_type = unique(defs)
      }

      # ensure every loss supports the requested scale
      ok = vapply(losses, function(lf) input_type %in% lf$input_supported, logical(1))
      if (!all(ok)) {
        bad = paste(names(losses)[!ok], collapse = ", ")
        stop(sprintf("The following losses do not support input_type = '%s': %s", input_type, bad))
      }

      ids = vapply(losses, function(x) x$id, character(1))
      names(losses) = ids
      self$losses = losses
      self$task_type = unique(tts)

      self$input_type = input_type
      self$y_pred = y_pred
      self$y_true = y_true
      private$.default_n_points = n_points
    },

    #' @description
    #' Create and return the ggplot2 plot with model-specific layers.
    #' @param n_points (`integer(1)`)\cr
    #'   Number of points to use for plotting the loss functions. Defaults to the
    #'   value configured when constructing the visualizer (via `as_visualizer()`),
    #'   falling back to 1000 if not set.
    #' @param y_curves (`character(1)`)\cr
    #'   When `input_type = "probability"`, choose which curves to display: `"both"`, `"y1"`, or `"y0"`. Default is "both".
    #' @param line_width (`numeric()`)\cr
    #'   Line widths for different loss functions. If NULL, uses default width of 1.2 for all lines.
    #' @param line_col (`character()`)\cr
    #'   Line colors for different loss functions. If NULL, uses automatic color assignment.
    #' @param line_type (`character()`)\cr
    #'   Line types for different loss functions. If NULL, uses "solid" for all lines.
    #' @param ... Additional arguments passed to the parent plot method.
    #' @return A ggplot2 object.
    plot = function(n_points = NULL, y_curves = "both", line_width = NULL, line_col = NULL, line_type = NULL, ...) {
      if (is.null(n_points)) {
        n_points = private$.default_n_points
      }
      checkmate::assert_integerish(n_points, lower = 10, len = 1)
      checkmate::assert_choice(y_curves, choices = c("both", "y1", "y0"))
      checkmate::assert_numeric(line_width, null.ok = TRUE)
      checkmate::assert_character(line_col, null.ok = TRUE)
      checkmate::assert_character(line_type, null.ok = TRUE)

      # Store VisualizerLossFuns-specific parameters before calling super$plot()
      private$.loss_plot_settings = list(
        n_points = as.integer(n_points),
        y_curves = y_curves,
        line_width = line_width,
        line_col = line_col,
        line_type = line_type
      )

      # Set up default plot labels and limits based on task type and input type
      default_x_lab = if (self$task_type == "classif") {
        if (self$input_type == "score") {
          "y * f"
        } else {
          "pi"
        }
      } else {
        "y - f"
      }

      # Set default x_limits based on task type, input type, and available data
      default_x_limits = if (self$task_type == "classif" && self$input_type == "probability") {
        if (!is.null(self$y_pred)) {
          c(max(min(self$y_pred), 0), min(max(self$y_pred), 1))
        } else {
          c(0, 1)
        }
      } else {
        if (!is.null(self$y_pred) && !is.null(self$y_true)) {
          if (self$task_type == "classif") {
            residuals = self$y_true * self$y_pred
          } else {
            residuals = self$y_true - self$y_pred
          }
          range_extend = diff(range(residuals)) * 0.1 # Add 10% padding
          c(min(residuals) - range_extend, max(residuals) + range_extend)
        } else {
          c(-5, 5)
        }
      }

      # Call parent method for common parameter validation and setup
      # Pass default labels and limits if not explicitly provided
      dots = list(...)
      if (is.null(dots$x_lab)) dots$x_lab = default_x_lab
      if (is.null(dots$y_lab)) dots$y_lab = "Loss"
      if (is.null(dots$x_limits)) dots$x_limits = default_x_limits
      if (is.null(dots$legend_title)) dots$legend_title = "Loss Function"
      if (is.null(dots$plot_title)) dots$plot_title = ""

      do.call(super$plot, dots)

      # Resolve layer colors now that we have plot settings
      self$resolve_layer_colors()

      # Render the plot
      p = private$render_plot()
      private$.last_plot = p
      return(p)
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
    #'   Default is "auto".
    #' @param size (`numeric(1)`)\cr
    #'   Size of the points. If NULL, uses theme$point_size. Default is NULL.
    #' @param alpha (`numeric(1)`)\cr
    #'   Alpha transparency of points and lines. If NULL, uses theme$alpha. Default is NULL.
    #' @param line_color (`character(1)`)\cr
    #'   Color of vertical lines. If NULL, uses the same color as points.
    #' @param line_alpha (`numeric(1)`)\cr
    #'   Alpha transparency of vertical lines. If NULL, uses alpha * 0.7.
    add_points = function(x, loss_id = NULL, show_line = TRUE, color = "auto", size = NULL, alpha = NULL,
                          line_color = NULL, line_alpha = NULL) {
      checkmate::assert_numeric(x)
      checkmate::assert_string(loss_id, null.ok = TRUE)
      checkmate::assert_flag(show_line)
      checkmate::assert_string(color)
      checkmate::assert_number(size, lower = 0, null.ok = TRUE)
      checkmate::assert_number(alpha, lower = 0, upper = 1, null.ok = TRUE)
      checkmate::assert_string(line_color, null.ok = TRUE)
      checkmate::assert_number(line_alpha, lower = 0, upper = 1, null.ok = TRUE)

      # Store layer specification using layer system
      private$store_layer("loss_points", list(
        x = x, loss_id = loss_id, show_line = show_line, color = color, size = size, alpha = alpha,
        line_color = line_color, line_alpha = line_alpha
      ))
      invisible(self)
    }
  ),
  private = list(
    .loss_plot_settings = NULL, # Store loss-specific plot settings
    .default_n_points = 1000L,

    # Override infer_z_values to handle loss function evaluation
    infer_z_values = function(points_data) {
      # For loss function visualizers, we can evaluate the loss at given points
      # if we have the necessary data
      if (all(is.na(points_data$y)) && "x" %in% names(points_data)) {
        # Try to evaluate the first loss function at the given x values
        if (length(self$losses) > 0) {
          first_loss = self$losses[[1]]
          loss_fun = first_loss$get_fun(self$input_type)
          points_data$y = loss_fun(points_data$x)
        } else {
          # Fallback to zeros if no loss functions available
          points_data$y = rep(0, nrow(points_data))
        }
      }
      return(points_data$y)
    },

    # Override prepare_points_data to handle loss function-specific point preparation
    prepare_points_data = function(points, visualizer_type) {
      points_data = super$prepare_points_data(points, visualizer_type)

      # For loss function visualizers, try to infer y values if not provided
      if (visualizer_type == "1D" && all(is.na(points_data$y))) {
        points_data$y = private$infer_z_values(points_data)
      }

      return(points_data)
    },

    # Render the complete plot using stored settings and layers
    render_plot = function() {
      eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme
      settings = private$.render_params
      loss_settings = private$.loss_plot_settings

      # Determine final labels from render params
      final_title = if (settings$show_title) {
        if (!is.null(settings$plot_title)) settings$plot_title else ""
      } else {
        NULL
      }

      # Convert string labels back to expressions for proper mathematical notation
      final_x_lab = if (!is.null(settings$x_lab)) {
        if (settings$x_lab == "y - f") {
          expression(y - f)
        } else if (settings$x_lab == "y * f") {
          expression(y * f)
        } else if (settings$x_lab == "pi") {
          expression(pi)
        } else {
          settings$x_lab # Use as-is if it's a custom label
        }
      } else {
        NULL
      }

      final_y_lab = settings$y_lab
      final_legend_title = settings$legend_title

      # Create the base plot with loss function curves
      pl = private$render_loss_curves()

      # Add stored layers
      pl = private$render_stored_layers(pl)

      # Apply consolidated theme from base helper
      pl = private$apply_ggplot_theme(
        plot_obj   = pl,
        text_size  = eff$text_size,
        title_size = eff$title_size,
        theme      = eff$theme,
        background = eff$background,
        show_grid  = eff$show_grid,
        grid_color = eff$grid_color
      )

      # Legend handling consistent with base: respect show_legend and legend_position (incl. "none")
      legend_pos = if (is.null(eff$legend_position)) "right" else eff$legend_position
      if (!settings$show_legend || identical(legend_pos, "none")) {
        pl = pl + ggplot2::theme(legend.position = "none")
      } else {
        pl = pl + ggplot2::theme(legend.position = legend_pos)
      }

      # Apply labels and subtitle
      pl = pl + ggplot2::labs(title = final_title, subtitle = settings$plot_subtitle, x = final_x_lab, y = final_y_lab)

      # Apply axis limits if specified
      if (!is.null(settings$x_limits)) {
        pl = pl + ggplot2::xlim(settings$x_limits[1], settings$x_limits[2])
      }
      if (!is.null(settings$y_limits)) {
        pl = pl + ggplot2::ylim(settings$y_limits[1], settings$y_limits[2])
      }

      return(pl)
    },

    # Render the base loss function curves
    render_loss_curves = function() {
      settings = private$.render_params
      loss_settings = private$.loss_plot_settings
      final_legend_title = settings$legend_title
      loss_labels = sapply(self$losses, function(x) x$label)
      eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme

      if (self$task_type == "classif" && self$input_type == "probability") {
        # ---- probability based visualisation ----
        if (!is.null(self$y_pred)) {
          p_min = max(min(self$y_pred), 0)
          p_max = min(max(self$y_pred), 1)
          r_seq = seq(p_min, p_max, length.out = loss_settings$n_points)
        } else {
          xlim = settings$x_limits
          if (is.null(xlim)) xlim = c(0, 1)
          r_seq = seq(xlim[1], xlim[2], length.out = loss_settings$n_points)
        }

        y_set = switch(loss_settings$y_curves,
          "both" = c("y = 1", "y = 0"),
          "y1"   = c("y = 1"),
          "y0"   = c("y = 0")
        )

        data_list = list()
        for (lf in self$losses) {
          loss_fun_for_input = lf$get_fun(self$input_type)
          if ("y = 1" %in% y_set) {
            data_list[[length(data_list) + 1L]] = data.table::data.table(
              r        = r_seq,
              loss_val = loss_fun_for_input(r_seq),
              loss_fun = lf$id,
              y_val    = "y = 1"
            )
          }
          if ("y = 0" %in% y_set) {
            data_list[[length(data_list) + 1L]] = data.table::data.table(
              r        = r_seq,
              loss_val = loss_fun_for_input(1 - r_seq),
              loss_fun = lf$id,
              y_val    = "y = 0"
            )
          }
        }
        dd = data.table::rbindlist(data_list)

        pl = ggplot2::ggplot(
          data = dd,
          ggplot2::aes(
            x = r, y = loss_val,
            col = loss_fun,
            linetype = y_val
          )
        ) +
          ggplot2::geom_line(linewidth = if (!is.null(loss_settings$line_width)) loss_settings$line_width[1] else eff$line_width)
      } else {
        # ---- regression or score/margin classification ----
        if (!is.null(self$y_pred) && !is.null(self$y_true)) {
          if (self$task_type == "classif") {
            residuals = self$y_true * self$y_pred
          } else {
            residuals = self$y_true - self$y_pred
          }
          r_seq = seq(min(residuals), max(residuals), length.out = loss_settings$n_points)
        } else {
          xlim = settings$x_limits
          if (is.null(xlim)) xlim = c(-5, 5)
          r_seq = seq(xlim[1], xlim[2], length.out = loss_settings$n_points)
        }

        loss_seqs = data.table::as.data.table(lapply(self$losses, function(ll) {
          loss_fun_for_input = ll$get_fun(self$input_type)
          loss_fun_for_input(r_seq)
        }))
        dd = cbind(r = r_seq, loss_seqs)
        dd = data.table::melt(dd,
          id.vars = "r", measure.vars = colnames(loss_seqs),
          variable.name = "loss_fun", value.name = "loss_val"
        )

        # Only map linetype to loss_fun if we have custom line types
        if (!is.null(loss_settings$line_type)) {
          pl = ggplot2::ggplot(
            data = dd,
            ggplot2::aes(
              x = r, y = loss_val,
              col = loss_fun,
              linetype = loss_fun
            )
          ) +
            ggplot2::geom_line(linewidth = if (!is.null(loss_settings$line_width)) loss_settings$line_width[1] else eff$line_width)
        } else {
          # Don't map linetype to avoid duplicate legend
          pl = ggplot2::ggplot(
            data = dd,
            ggplot2::aes(
              x = r, y = loss_val,
              col = loss_fun
            )
          ) +
            ggplot2::geom_line(linewidth = if (!is.null(loss_settings$line_width)) loss_settings$line_width[1] else eff$line_width)
        }
      }

      # ---- shared styling ----
      if (!is.null(loss_settings$line_col)) {
        color_values = loss_settings$line_col
      } else {
        n_losses = length(unique(dd$loss_fun))
        # Use theme palette instead of hard-coded colors
        eff = if (is.null(private$.effective_theme)) get_pkg_theme_default() else private$.effective_theme
        color_values = sapply(1:n_losses, function(i) get_vistool_color(i, "discrete", base_palette = eff$palette))
      }
      pl = pl + ggplot2::scale_color_manual(values = color_values, labels = loss_labels, name = final_legend_title)

      # Handle custom line widths for multiple loss functions
      if (!is.null(loss_settings$line_width) && length(unique(dd$loss_fun)) > 1) {
        # For multiple loss functions with different line widths, we need to rebuild the plot
        # Map linewidth to loss_fun and then use manual scaling with guide = "none" to avoid duplicate legend
        if (self$task_type == "classif" && self$input_type == "probability") {
          pl = ggplot2::ggplot(
            data = dd,
            ggplot2::aes(
              x = r, y = loss_val,
              col = loss_fun,
              linetype = y_val,
              linewidth = loss_fun
            )
          ) +
            ggplot2::geom_line() +
            ggplot2::scale_color_manual(values = color_values, labels = loss_labels, name = final_legend_title) +
            ggplot2::scale_linewidth_manual(values = loss_settings$line_width, guide = "none")
        } else {
          # For regression/score classification, only add linetype aesthetic if we have custom line types
          if (!is.null(loss_settings$line_type)) {
            pl = ggplot2::ggplot(
              data = dd,
              ggplot2::aes(
                x = r, y = loss_val,
                col = loss_fun,
                linetype = loss_fun,
                linewidth = loss_fun
              )
            ) +
              ggplot2::geom_line() +
              ggplot2::scale_color_manual(values = color_values, labels = loss_labels, name = final_legend_title) +
              ggplot2::scale_linewidth_manual(values = loss_settings$line_width, guide = "none")
          } else {
            pl = ggplot2::ggplot(
              data = dd,
              ggplot2::aes(
                x = r, y = loss_val,
                col = loss_fun,
                linewidth = loss_fun
              )
            ) +
              ggplot2::geom_line() +
              ggplot2::scale_color_manual(values = color_values, labels = loss_labels, name = final_legend_title) +
              ggplot2::scale_linewidth_manual(values = loss_settings$line_width, guide = "none")
          }
        }
      }

      # Apply custom line types only if specified and not for probability-based classification
      if (!(self$task_type == "classif" && self$input_type == "probability")) {
        if (!is.null(loss_settings$line_type)) {
          pl = pl + ggplot2::scale_linetype_manual(values = loss_settings$line_type, labels = loss_labels, name = final_legend_title)
        }
      } else {
        # For probability-based classification, handle y_val linetype
        if (length(unique(dd$y_val)) == 1L) {
          pl = pl + ggplot2::scale_linetype_manual(values = "solid", labels = unique(dd$y_val), name = "Class")
        } else {
          # Get the unique y_val levels in the order they will appear as factor levels
          y_levels = sort(unique(dd$y_val)) # This gives alphabetical order like factor()
          # Create corresponding line types: solid for y=1, dashed for y=0
          line_types = ifelse(y_levels == "y = 1", "solid", "dashed")
          pl = pl + ggplot2::scale_linetype_manual(values = line_types, labels = y_levels, name = "Class")
        }
      }

      return(pl)
    },

    # Render all stored layers
    render_stored_layers = function(plot_obj) {
      # render layers in the order they were added
      if (!is.null(private$.layers_to_add)) {
        for (layer in private$.layers_to_add) {
          if (layer$type == "points") {
            # Handle generic points from base class
            points_data = private$prepare_points_data(layer$spec$points, "1D")

            # Add points layer
            plot_obj = plot_obj + ggplot2::geom_point(
              data = points_data,
              ggplot2::aes(x = x, y = y),
              color = layer$spec$color,
              size = layer$spec$size,
              shape = layer$spec$shape,
              alpha = layer$spec$alpha,
              inherit.aes = FALSE
            )
          } else if (layer$type == "loss_points") {
            plot_obj = private$render_loss_points_layer(plot_obj, layer$spec)
          }
        }
      }

      return(plot_obj)
    },

    # Render a single loss points layer
    render_loss_points_layer = function(plot_obj, point_spec) {
      # Resolve style defaults from effective theme
      eff = private$.effective_theme
      resolved_size = if (is.null(point_spec$size)) eff$point_size else point_spec$size
      resolved_alpha = if (is.null(point_spec$alpha)) eff$alpha else point_spec$alpha

      # Determine which loss function to use
      if (is.null(point_spec$loss_id)) {
        if (length(self$losses) == 0) {
          stop("No loss functions available and no loss_id specified")
        }
        loss_fun = self$losses[[1]]$get_fun(self$input_type)
      } else {
        if (!point_spec$loss_id %in% names(self$losses)) {
          stop(sprintf("Loss function '%s' not found", point_spec$loss_id))
        }
        loss_fun = self$losses[[point_spec$loss_id]]$get_fun(self$input_type)
      }

      # Calculate y-values by evaluating the loss function
      y_vals = loss_fun(point_spec$x)

      # Create points data
      points_data = data.frame(
        x = point_spec$x,
        y = y_vals
      )

      # Add points
      plot_obj = plot_obj + ggplot2::geom_point(
        data = points_data,
        ggplot2::aes(x = x, y = y),
        color = point_spec$color,
        size = resolved_size,
        alpha = resolved_alpha,
        shape = 21, # Circle with border and fill
        fill = "white", # Hollow center
        stroke = 1, # Border thickness
        inherit.aes = FALSE
      )

      # Add vertical lines if requested
      if (point_spec$show_line) {
        line_color = if (is.null(point_spec$line_color)) point_spec$color else point_spec$line_color
        # Default vertical line alpha to theme alpha * 0.7 if not provided
        line_alpha = if (is.null(point_spec$line_alpha)) resolved_alpha * 0.7 else point_spec$line_alpha

        # Create line segments from points to x-axis
        line_data = data.frame(
          x = point_spec$x,
          y = y_vals,
          xend = point_spec$x,
          yend = 0
        )

        plot_obj = plot_obj + ggplot2::geom_segment(
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
