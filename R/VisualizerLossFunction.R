#' @title Visualize Loss Function
#'
#' @description
#' This class is used to create visualizations of loss functions.
#'
#' @export
VisualizerLossFunction = R6::R6Class("VisualizerLossFunction",
  public = list(

    #' @field loss_function [LossFunction]\cr
    #' Loss function.
    loss_function = NULL,

    #' @field loss (`numeric()`)\cr
    #' Loss values.
    loss = NULL,

    #' @field y_pred `numeric()`\cr
    #' Predicted values.
    y_pred = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param loss_function [LossFunction]\cr
    #'   Loss function.
    #' @param y_pred (`numeric()`)\cr
    #'  Predicted values.
    #' @param y_true (`numeric(1)`)\cr
    #'   True value.
    #' @param ... (`any`)\cr
    #'   Additional arguments passed to the loss function.
    initialize = function(loss_function, y_pred, y_true, ...) {
      self$loss_function = assert_r6(loss_function, "LossFunction")
      self$y_pred = assert_numeric(y_pred)
      assert_number(y_true)

      self$loss = loss_function$fun(y_true, self$y_pred, ...)
    },

    #' @description
    #' Initialize the plot with a line plot.
    #'
    #' @param width (`integer(1)`)\cr
    #'  Width of the line.
    #' @param color (`character(1)`)\cr
    #'  Color of the line.
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    init_layer_lines = function(width = 2, color = "rgb(160,82,45)", ...) {
      assert_int(width, lower = 1)
      assert_character(color)

      llp = list(x = self$y_pred, y = self$loss)
      private$.args = list(
        name = self$loss_function$label,
        showlegend = TRUE,
        x = llp$x,
        y = llp$y,
        type = "scatter",
        mode = "lines",
        line = list(width = width, color = color),
        ...
      )

      self$set_layout(
        title = private$.title,
        xaxis = list(title = if ("classif" %in% self$loss_function$properties) "Margin y * f(x)" else "Residual y - f(x)"),
        yaxis = list(title = "Loss"))

      return(invisible(self))
    },

    #' @description
    #' Set the layout of the plotly plot.
    #'
    #' @param ... (`any`)\cr
    #'   Layout options directly passed to `layout(...)`.
    set_layout = function(...) {
      private$.layout = insert_named(private$.layout, list(...))

      return(invisible(self))
    },

    #' @description
    #' Return the plot and hence plot it or do further processing.
    plot = function() {
      if (is.null(private$.args)) self$init_layer_lines()
      plot = invoke(add_trace, plot_ly(), .args = private$.args)
      invoke(layout, plot, .args = private$.layout)
    },

    #' @description
    #' Save the plot by using plotlys `orca()` function.
    #'
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `orca()`.
    save = function(...) {
      orca(self$plot, ...)
    }
  ),

  private = list(
    .args = NULL,
    .layout = NULL
  )
)

#' @export
c.VisualizerLossFunction = function(...) {
  visualizers = list(...)
  walk(visualizers, function(visualizer) visualizer$init_layer_lines())
  title = str_collapse(map_chr(visualizers, function(visualizer) visualizer$loss_function$label), " & ")

  Reduce(function(x, y) {
    invoke(add_trace, x, line = NULL, .args = get_private(y)$.args)
  }, visualizers, plot_ly()) %>%
  layout(
    title = title,
    xaxis = list(title = "Predictions"),
    yaxis = list(title = "Loss"))
}
