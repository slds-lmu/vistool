#' @title Visualizer for Losses
#'
#' @description
#' Visualize one or multiple loss functions.
#'
#' @export
VisualizerLossFuns = R6::R6Class("VisualizerLossFuns",
  public = list(

    #' @field loss_function [LossFunction]\cr
    #' Loss function.
    losses = NULL,
    task_type = NULL,
    lab_x = NULL,
    lab_y = NULL,
    title = NULL,
    x_range = NULL,
    line_width = NULL,
    line_col = NULL,
    line_type = NULL,
    legend_title = element_blank(),

    # FIXME: better doc the class

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param loss_function [LossFunction]\cr
    #'   Loss function.
    initialize = function(losses) {
      assert_list(losses, "LossFunction")
      tts = unique(map_chr(losses, function(x) x$task_type))
      if (length(tts) > 1)
        stopf("'LossFunction$task_type' all need to be the same, but found: %s", collapse(tts))
      ids = map_chr(losses, function(x) x$id)
      names(losses) = ids
      self$losses = losses
      self$task_type = unique(tts)
      self$title = ""
      self$lab_x = ifelse(self$task_type == "classif", "y * f", "y - f")
      self$lab_y = "Loss"
      self$x_range = c(-5, 5)
      n = length(losses)
      self$line_width = rep(0.5, n)
      self$line_col = NULL
      self$line_type = rep("solid", n)
    },

    plot = function() {
      # get lossfun labels so we can use them for legend
      loss_labels = map_chr(self$losses, function(x) x$label)
      # eval losses on defined range, then melt data into long format
      r_seq = seq(self$x_range[1], self$x_range[2], by = 0.01)
      loss_seqs = map_dtc(self$losses, function(ll) ll$fun(r_seq))
      dd = cbind(r = r_seq, loss_seqs)
      dd = melt(dd, id.vars = "r", measure.vars = colnames(loss_seqs),
        variable.name = "loss_fun", value.name = "loss_val")
      # plot
      pl = ggplot(data = dd, aes(x = r, y = loss_val,
        col = loss_fun, linetype = loss_fun, size = loss_fun))
      pl = pl + geom_line()
      # use cols, linetypes and widths
      if (!is.null(self$line_col))
        pl = pl + scale_color_manual(values = self$line_col, labels = loss_labels)
      else {
        if (requireNamespace("ggsci", quietly = TRUE)) {
          pl = pl + ggsci::scale_color_npg(labels = loss_labels)
        } else {
          pl = pl + scale_color_discrete(labels = loss_labels)
        }
      }
      if (!is.null(self$line_width))
        pl = pl + scale_size_manual(values = self$line_width, labels = loss_labels)
      if (!is.null(self$line_type)) {
        pl = pl + scale_linetype_manual(values = self$line_type, labels = loss_labels)
      }
      # use specified axis labels and legend title
      pl = pl + labs(x = self$lab_x, y = self$lab_y)
      pl = pl + theme(legend.title = self$legend_title)
      return(pl)
    }
  )
)

