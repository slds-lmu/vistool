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

#' @title Loss Function
#'
#' @description
#' This class is used to create loss functions.
#'
#' @export
LossFunction = R6::R6Class("LossFunction",
  public = list(

    #' @field id `character(1)`\cr
    #' Unique identifier of the loss function.
    id = NULL,

    #' @field fun `function(y_true, y_pred, ...)`\cr
    #' Loss function.
    fun = NULL,

    #' @field label `character(1)`\cr
    #' Label of the loss function.
    label = NULL,

    #' @field properties `character()`\cr
    #' Additional properties of the loss function.
    properties = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'    Unique identifier of the loss function.
    #' @param label (`character(1)`)\cr
    #'   Label of the loss function.
    #' @param properties (`character()`)\cr
    #'   Additional properties of the loss function.
    #' @param fun (`function(y_true, y_pred, ...)`)\cr
    #'   Loss function.
    initialize = function(id, label, properties, fun) {
      self$id = assert_character(id)
      self$fun = assert_function(fun)
      self$label = assert_character(label)
      self$properties = assert_character(properties)
    }
  )
)

#' @title Dictionary of Loss Functions
#'
#' @description
#' Dictionary of loss functions.
#'
#' @export
dictionary_loss = R6::R6Class("DictionaryLoss", inherit = Dictionary, cloneable = FALSE)$new()

#' @title Retrieve Loss Function
#'
#' @description
#' Retrieve a loss function from the dictionary.
#'
#' @export
lss = function(.key, ...) {
  dictionary_loss$get(.key, ...)
}

dictionary_loss$add("l2_se", LossFunction$new("l2_se", "L2 Squared Error", "regr", function(y_true, y_pred) {
  (y_true - y_pred)^2
}))

dictionary_loss$add("l1_ae", LossFunction$new("l1_ae", "L1 Absolute Error", "regr", function(y_true, y_pred) {
  abs(y_true - y_pred)
}))

dictionary_loss$add("huber", LossFunction$new("huber", "Huber Loss", "regr", function(y_true, y_pred, ...) {
  delta = list(...)[["delta"]]
  a = abs(y_true - y_pred)
  ifelse(a <= delta, 0.5 * a^2, delta * a - delta^2 / 2)
}))

dictionary_loss$add("log-cosh", LossFunction$new("log-cosh", "Log-Cosh Loss", "regr", function(y_true, y_pred, delta) {
  log(cosh(y_pred - y_true))
}))

dictionary_loss$add("cross-entropy", LossFunction$new("cross-entropy", "Cross-Entropy", "classif", function(y_true, y_pred) {
  log(1 + exp(-y_true * y_pred))
}))

dictionary_loss$add("hinge", LossFunction$new("hinge", "Hinge Loss", "classif", function(y_true, y_pred) {
  pmax(1 - y_true * y_pred, 0)
}))


