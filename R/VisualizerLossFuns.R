#' @title Visualizer for Losses
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

    #' @field lab_x (`character(1)`)\cr
    #' Label for x-axis.
    lab_x = NULL,

    #' @field lab_y (`character(1)`)\cr
    #' Label for y-axis.
    lab_y = NULL,

    #' @field title (`character(1)`)\cr
    #' Title of the plot.
    title = NULL,

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

    #' @field legend_title (`element`)\cr
    #' Legend title element.
    legend_title = element_blank(),

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

    # FIXME: better doc the class

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
    initialize = function(losses, y_pred = NULL, y_true = NULL, n_points = 1000L,
                          input_type = "auto", y_curves = "both") {
      checkmate::assert_list(losses, "LossFunction")
      checkmate::assert_numeric(y_pred, null.ok = TRUE)
      checkmate::assert_numeric(y_true, null.ok = TRUE)
      checkmate::assert_integerish(n_points, lower = 10, len = 1)
      checkmate::assert_choice(input_type, choices = c("auto", "score", "probability"))
      checkmate::assert_choice(y_curves, choices = c("both", "y1", "y0"))

      tts <- unique(sapply(losses, function(x) x$task_type))
      if (length(tts) > 1) {
        mlr3misc::stopf("'LossFunction$task_type' all need to be the same, but found: %s", mlr3misc::str_collapse(tts))
      }

      # resolve input_type = "auto"
      if (input_type == "auto") {
        defs <- sapply(losses, function(x) x$input_default)
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

      ids <- sapply(losses, function(x) x$id)
      names(losses) <- ids
      self$losses <- losses
      self$task_type <- unique(tts)
      self$title <- ""
      self$lab_y <- "Loss"
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
    },

    #' @description
    #' Initialize line layer for the plot.
    #' @param color (`character()`)\cr
    #'   Colors to use for the lines. If NULL, default colors will be used.
    #' @template return_self_invisible
    init_layer_lines = function(color = NULL) {
      if (!is.null(color)) {
        self$line_col <- color
      }
      invisible(self)
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
    #' @return A ggplot2 object.
    plot = function(text_size = 11, theme = "bw") {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))

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
      pl <- pl + ggplot2::scale_color_manual(values = color_values, labels = loss_labels)

      if (!is.null(self$line_width)) {
        pl <- pl + ggplot2::scale_linewidth_manual(values = self$line_width, labels = loss_labels)
      } else {
        pl <- pl + ggplot2::scale_linewidth_manual(values = rep(1.2, length(unique(dd$loss_fun))), labels = loss_labels)
      }

      if (!(self$task_type == "classif" && self$input_type == "probability")) {
        if (!is.null(self$line_type)) {
          pl <- pl + ggplot2::scale_linetype_manual(values = self$line_type, labels = loss_labels)
        }
      } else {
        if (length(unique(dd$y_val)) == 1L) {
          pl <- pl + ggplot2::scale_linetype_manual(values = "solid", labels = unique(dd$y_val))
        } else {
          pl <- pl + ggplot2::scale_linetype_manual(values = c("solid", "dashed"), labels = c("y = 1", "y = 0"))
        }
      }

      theme_fun <- switch(theme,
        "minimal" = ggplot2::theme_minimal,
        "bw"      = ggplot2::theme_bw,
        "classic" = ggplot2::theme_classic,
        "gray"    = ggplot2::theme_gray,
        "light"   = ggplot2::theme_light,
        "dark"    = ggplot2::theme_dark,
        "void"    = ggplot2::theme_void
      )
      pl <- pl + theme_fun(base_size = text_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

      pl <- pl + ggplot2::labs(x = self$lab_x, y = self$lab_y) +
        ggplot2::theme(legend.title = self$legend_title)

      return(pl)
    }
  )
)
