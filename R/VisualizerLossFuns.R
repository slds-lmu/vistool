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
    initialize = function(losses, y_pred = NULL, y_true = NULL) {
      checkmate::assert_list(losses, "LossFunction")
      checkmate::assert_numeric(y_pred, null.ok = TRUE)
      checkmate::assert_numeric(y_true, null.ok = TRUE)

      tts <- unique(sapply(losses, function(x) x$task_type))
      if (length(tts) > 1) {
        mlr3misc::stopf("'LossFunction$task_type' all need to be the same, but found: %s", mlr3misc::str_collapse(tts))
      }
      ids <- sapply(losses, function(x) x$id)
      names(losses) <- ids
      self$losses <- losses
      self$task_type <- unique(tts)
      self$title <- ""
      self$lab_x <- ifelse(self$task_type == "classif", "y * f", "y - f")
      self$lab_y <- "Loss"
      self$x_range <- c(-5, 5)
      n <- length(losses)
      self$line_width <- rep(0.5, n)
      self$line_col <- NULL
      self$line_type <- rep("solid", n)
      self$y_pred <- y_pred
      self$y_true <- y_true
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
    #' Create and return the ggplot2 plot.
    #' @param text_size (`numeric(1)`)\cr
    #'   Base text size for plot elements. Default is 11.
    #' @param theme (`character(1)`)\cr
    #'   ggplot2 theme to use. One of "minimal", "bw", "classic", "gray", "light", "dark", "void". Default is "bw".
    #' @return A ggplot2 object showing the loss functions.
    plot = function(text_size = 11, theme = "bw") {
      checkmate::assert_number(text_size, lower = 1)
      checkmate::assert_choice(theme, choices = c("minimal", "bw", "classic", "gray", "light", "dark", "void"))
      
      # get lossfun labels so we can use them for legend
      loss_labels <- sapply(self$losses, function(x) x$label)
      # eval losses on defined range, then melt data into long format
      if (!is.null(self$y_pred) && !is.null(self$y_true)) {
        # calculate residuals based on task type
        if (self$task_type == "classif") {
          r_seq <- self$y_true * self$y_pred # y * f(x) for classification
        } else {
          r_seq <- self$y_true - self$y_pred # y - f(x) for regression
        }
      } else {
        r_seq <- seq(self$x_range[1], self$x_range[2], by = 0.01)
      }
      loss_seqs <- data.table::as.data.table(lapply(self$losses, function(ll) ll$fun(r_seq)))
      dd <- cbind(r = r_seq, loss_seqs)
      dd <- data.table::melt(dd,
        id.vars = "r", measure.vars = colnames(loss_seqs),
        variable.name = "loss_fun", value.name = "loss_val"
      )
      # plot
      pl <- ggplot(data = dd, aes(
        x = r, y = loss_val,
        col = loss_fun, linetype = loss_fun, linewidth = loss_fun
      ))
      pl <- pl + geom_line()
      # intelligent color selection
      if (!is.null(self$line_col)) {
        # use user-specified colors
        color_values <- self$line_col
      } else {
        # smart default color selection
        n_losses <- length(unique(dd$loss_fun))
        if (n_losses == 1) {
          color_values <- "#FF8C00"  # orange
        } else if (n_losses == 2) {
          color_values <- c("#FF8C00", "#1E90FF")  # orange, dodger blue
        } else if (n_losses == 3) {
          color_values <- c("#FF8C00", "#1E90FF", "#32CD32")  # orange, dodger blue, lime green
        } else if (n_losses == 4) {
          color_values <- c("#FF8C00", "#1E90FF", "#32CD32", "#DC143C")  # orange, dodger blue, lime green, crimson
        } else {
          # for 5+ functions, use a colorblind-friendly palette
          if (requireNamespace("ggsci", quietly = TRUE)) {
            # use ggsci NPG palette which is generally colorblind-friendly
            color_values <- ggsci::pal_npg("nrc")(n_losses)
          } else {
            # fallback to viridis-like colors (colorblind-friendly)
            color_values <- rainbow(n_losses, start = 0, end = 0.8)
          }
        }
      }
      
      # apply color scale
      pl <- pl + scale_color_manual(values = color_values, labels = loss_labels)
      
      # apply line width
      if (!is.null(self$line_width)) {
        pl <- pl + scale_linewidth_manual(values = self$line_width, labels = loss_labels)
      } else {
        # defaults for linewidth
        pl <- pl + scale_linewidth_manual(values = rep(1.2, length(unique(dd$loss_fun))), labels = loss_labels)
      }
      
      # apply line type
      if (!is.null(self$line_type)) {
        pl <- pl + scale_linetype_manual(values = self$line_type, labels = loss_labels)
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
      pl <- pl + theme_fun(base_size = text_size) + theme(plot.title = ggplot2::element_text(hjust = 0.5))
      
      # use specified axis labels and legend title
      pl <- pl + labs(x = self$lab_x, y = self$lab_y)
      pl <- pl + theme(legend.title = self$legend_title)
      return(pl)
    }
  )
)
