
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
dict_loss = R6::R6Class("DictionaryLoss", inherit = Dictionary, cloneable = FALSE)$new()

#' @title Retrieve Loss Function
#'
#' @description
#' Retrieve a loss function from the dictionary.
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve the object.
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, or to be set as public field.
#'   See [mlr3misc::dictionary_sugar_get()] for more details.
#'
#' @export
lss = function(.key, ...) {
  dict_loss$get(.key, ...)
}

dict_loss$add("l2_se", LossFunction$new("l2_se", "L2 Squared Error", "regr", function(y_true, y_pred) {
  (y_true - y_pred)^2
}))

dict_loss$add("l1_ae", LossFunction$new("l1_ae", "L1 Absolute Error", "regr", function(y_true, y_pred) {
  abs(y_true - y_pred)
}))

dict_loss$add("huber", LossFunction$new("huber", "Huber Loss", "regr", function(y_true, y_pred, delta = 1) {
  a = abs(y_true - y_pred)
  ifelse(a <= delta, 0.5 * a^2, delta * a - delta^2 / 2)
}))

dict_loss$add("log-cosh", LossFunction$new("log-cosh", "Log-Cosh Loss", "regr", function(y_true, y_pred) {
  log(cosh(y_pred - y_true))
}))

dict_loss$add("log-barrier", LossFunction$new("log-barrier", "Log-Barrier Loss", "regr", function(y_true, y_pred, epsilon = 1) {
  abs_res <- abs(y_true - y_pred)
  abs_res[abs_res > epsilon] <- Inf
  abs_res[abs_res <= epsilon] <- -epsilon**2 * log(1 - (abs_res[abs_res <= epsilon] / epsilon)**2)
  sum(abs_res)
}))

dict_loss$add("epsilon-insensitive", LossFunction$new("epsilon-insensitive", "Epsilon-Insensitive Loss", "regr", function(y_true, y_pred, epsilon = 1) {
  sum(ifelse(abs(y_true - y_pred) > epsilon, abs(y_true - y_pred) - epsilon, 0L))
}))

dict_loss$add("pinball", LossFunction$new("pinball", "Pinball Loss", "regr", function(y_true, y_pred, quantile = 5) {
  sum(ifelse(y_true - y_pred < 0, ((1 - quantile) * (-(y_true - y_pred)), quantile * (y_true - y_pred))
}))

dict_loss$add("cauchy", LossFunction$new("cauchy", "Cauchy Loss", "regr", function(y_true, y_pred, epsilon = 1) {
  sum(0.5 * epsilon**2 * log(1 + ((y_true - y_pred) / epsilon)**2))
}))

dict_loss$add("cross-entropy", LossFunction$new("cross-entropy", "Cross-Entropy", "classif", function(y_true, y_pred) {
  log(1 + exp(-y_true * y_pred))
}))

dict_loss$add("hinge", LossFunction$new("hinge", "Hinge Loss", "classif", function(y_true, y_pred) {
  pmax(1 - y_true * y_pred, 0)
}))

#' @export
as.data.table.DictionaryLoss = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    t = x$get(key)
    insert_named(
      list(key = key, label = t$label, properties = list(t$properties)),
      if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
