# FIXME: doc API of funs better


#' @title Loss Function
#'
#' @description
#' This class is used to create loss functions.
#'
#' @export
LossFunction <- R6::R6Class("LossFunction",
  public = list(

    #' @template field_id
    id = NULL,

    #' @field fun `function(y_true, y_pred, ...)`\cr
    #' Loss function.
    fun = NULL,

    #' @field label `character(1)`\cr
    #' Label of the loss function.
    label = NULL,

    #' @field task_type `character(1)`\cr
    #' Task type for which the loss function is designed.
    task_type = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_id
    #' @param label (`character(1)`)\cr
    #'   Label of the loss function.
    #' @param task_type (`character(1)`)\cr
    #'   Task type for which the loss function is designed.
    #' @param fun (`function(y_true, y_pred, ...)`)\cr
    #'   Loss function.
    initialize = function(id, label, task_type, fun) {
      self$id <- checkmate::assert_character(id)
      self$label <- checkmate::assert_character(label)
      self$task_type <- checkmate::assert_choice(task_type, c("regr", "classif"))
      self$fun <- checkmate::assert_function(fun)
    }
  )
)

#' @title Dictionary of Loss Functions
#'
#' @description
#' Dictionary of loss functions.
#'
#' @export
dict_loss <- R6::R6Class("DictionaryLoss", inherit = Dictionary, cloneable = FALSE)$new()

#' @title Retrieve Loss Function
#'
#' @description
#' Retrieve a loss function from the dictionary.
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve the object.
#' @template param_dots
#'
#' @export
lss <- function(.key, ...) {
  dict_loss$get(.key, ...)
}

dict_loss$add("l2_se", LossFunction$new("l2", "L2 Squared Error", "regr", function(r) {
  (r)^2
}))

dict_loss$add("l1_ae", LossFunction$new("l1", "L1 Absolute Error", "regr", function(r) {
  abs(r)
}))

dict_loss$add("huber", LossFunction$new("huber", "Huber Loss", "regr", function(r, delta = 1) {
  a <- abs(r)
  ifelse(a <= delta, 0.5 * a^2, delta * a - delta^2 / 2)
}))

dict_loss$add("log-cosh", LossFunction$new("logcosh", "Log-Cosh Loss", "regr", function(r) {
  log(cosh(r))
}))

dict_loss$add("cross-entropy", LossFunction$new("logloss", "Log Loss", "classif", function(r) {
  log(1 + exp(-r))
}))

dict_loss$add("hinge", LossFunction$new("hinge", "Hinge Loss", "classif", function(r) {
  pmax(1 - r, 0)
}))

dict_loss$add("log-barrier", LossFunction$new("log-barrier", "Log-Barrier Loss", "regr", function(r, epsilon = 1) {
  abs_res <- abs(r)
  abs_res[abs_res > epsilon] <- Inf
  abs_res[abs_res <= epsilon] <- -epsilon^2 * log(1 - (abs_res[abs_res <= epsilon] / epsilon)^2)
  abs_res
}))

dict_loss$add("epsilon-insensitive", LossFunction$new("epsilon-insensitive", "Epsilon-Insensitive Loss", "regr", function(r, epsilon = 1) {
  ifelse(abs(r) > epsilon, abs(r) - epsilon, 0)
}))

dict_loss$add("pinball", LossFunction$new("pinball", "Pinball Loss", "regr", function(r, quantile = 0.5) {
  ifelse(r < 0, (1 - quantile) * (-r), quantile * r)
}))

dict_loss$add("cauchy", LossFunction$new("cauchy", "Cauchy Loss", "regr", function(r, epsilon = 1) {
  0.5 * epsilon^2 * log(1 + (r / epsilon)^2)
}))


#' @title Convert Dictionary to Data Table
#'
#' @description
#' Converts a loss function dictionary to a data table.
#'
#' @param x (`DictionaryLoss`)\cr
#'   The dictionary to convert.
#' @template param_dots
#' @param objects (`logical(1)`)\cr
#'   Whether to include the objects in the result.
#'
#' @export
as.data.table.DictionaryLoss <- function(x, ..., objects = FALSE) {
  checkmate::assert_flag(objects)

  setkeyv(mlr3misc::map_dtr(x$keys(), function(key) {
    t <- x$get(key)
    mlr3misc::insert_named(
      list(key = key, label = t$label, task_type = t$task_type),
      if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
