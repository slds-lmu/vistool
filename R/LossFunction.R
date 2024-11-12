#FIXME: doc API of funs better


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
    task_type = NULL,

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
    initialize = function(id, label, task_type, fun) {
      self$id = assert_character(id)
      self$label = assert_character(label)
      self$task_type = assert_choice(task_type, c("regr", "classif"))
      self$fun = assert_function(fun)
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

dict_loss$add("l2_se", LossFunction$new("l2", "L2 Squared Error", "regr", function(r) {
  (r)^2
}))

dict_loss$add("l1_ae", LossFunction$new("l1", "L1 Absolute Error", "regr", function(r) {
  abs(r)
}))

dict_loss$add("huber", LossFunction$new("huber", "Huber Loss", "regr", function(r, delta = 1) {
  a = abs(r)
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


#' @export
as.data.table.DictionaryLoss = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    t = x$get(key)
    insert_named(
      list(key = key, label = t$label, task_type = t$task_type),
      if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}