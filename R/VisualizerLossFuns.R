#' @title Visualizer for Losses
#'
#' @description
#' Visualize one or multiple loss functions.
#'
#' @export
VisualizerLossFuns <- R6::R6Class("VisualizerLossFuns",
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
    #' @return A ggplot2 object showing the loss functions.
    plot = function() {
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
      dd <- melt(dd,
        id.vars = "r", measure.vars = colnames(loss_seqs),
        variable.name = "loss_fun", value.name = "loss_val"
      )
      # plot
      pl <- ggplot(data = dd, aes(
        x = r, y = loss_val,
        col = loss_fun, linetype = loss_fun, linewidth = loss_fun
      ))
      pl <- pl + geom_line()
      # use cols, linetypes and widths
      if (!is.null(self$line_col)) {
        pl <- pl + scale_color_manual(values = self$line_col, labels = loss_labels)
      } else {
        if (requireNamespace("ggsci", quietly = TRUE)) {
          pl <- pl + ggsci::scale_color_npg(labels = loss_labels)
        } else {
          pl <- pl + scale_color_discrete(labels = loss_labels)
        }
      }
      if (!is.null(self$line_width)) {
        pl <- pl + scale_linewidth_manual(values = self$line_width, labels = loss_labels)
      }
      if (!is.null(self$line_type)) {
        pl <- pl + scale_linetype_manual(values = self$line_type, labels = loss_labels)
      }
      # use specified axis labels and legend title
      pl <- pl + labs(x = self$lab_x, y = self$lab_y)
      pl <- pl + theme(legend.title = self$legend_title)
      return(pl)
    }
  )
)
