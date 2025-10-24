#' @title Loss function
#'
#' @description
#' Represents a loss function for regression or classification tasks.
#'
#' @details
#' Loss functions are expected to be vectorized and to accept a single numeric
#' argument representing the input on the chosen scale:
#' * `"score"`: residuals `y_true - y_pred` for regression, or signed margins
#'   `y_true * f(x)` for binary classification.
#' * `"probability"`: predicted probabilities for the positive class. Visualizers
#'   obtain the loss for `y = 0` by evaluating the function at `1 - p`.
#'
#' The returned value must be a numeric vector of losses with the same length as
#' the input. Additional optional parameters can be encoded via closures and are
#' configurable through [lss()], which wraps the stored functions with the
#' supplied arguments.
#'
#' @export
LossFunction = R6::R6Class("LossFunction",
  public = list(
    #' @field id (`character(1)`)\cr
    #' Identifier for the loss function.
    id = NULL,

    #' @field label (`character(1)`)\cr
    #' Label for the loss function.
    label = NULL,

    #' @field task_type (`character(1)`)\cr
    #' Task type: `"regr"` or `"classif"`.
    task_type = NULL,

    #' @field input_default (`character(1)`)\cr
    #'   The natural input scale for the loss: `"score"` or `"probability"`.
    input_default = NULL,

    #' @field input_supported (`character()`)\cr
    #'   All input scales this loss can be expressed on (subset of
    #'   `c("score", "probability")`).
    input_supported = NULL,

    #' @field fun (`function` or `list`)\cr
    #' The loss function itself. Provide either a vectorized closure matching the
    #' active input scale or a named list of such closures keyed by `"score"` and
    #' / or `"probability"`.
    fun = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the loss function.
    #' @param label (`character(1)`)\cr
    #'   Label for the loss function.
    #' @param task_type (`character(1)`)\cr
    #'   Task type: `"regr"` or `"classif"`.
    #' @param fun (`function` or `list`)\cr
    #'   The loss function. Supply a vectorized closure matching the active
    #'   input scale or a named list of closures keyed by `"score"` / `"probability"`.
    #' @param input_default (`character(1)`)\cr
    #'   Default input scale (`"score"` or `"probability"`).
    #' @param input_supported (`character()`)\cr
    #'   Character vector of supported input scales. Must contain
    #'   `input_default`.
    initialize = function(id, label, task_type, fun,
                          input_default = "score",
                          input_supported = c("score")) {
      self$id = checkmate::assert_character(id)
      self$label = checkmate::assert_character(label)
      self$task_type = checkmate::assert_choice(task_type, c("regr", "classif"))

      checkmate::assert_choice(input_default, choices = c("score", "probability"))
      checkmate::assert_character(input_supported, min.len = 1, any.missing = FALSE)
      if (!input_default %in% input_supported) {
        stop("'input_default' must be included in 'input_supported'.")
      }
      if (!all(input_supported %in% c("score", "probability"))) {
        stop("'input_supported' may only contain 'score' and/or 'probability'.")
      }

      self$input_default = input_default
      self$input_supported = unique(input_supported)

      # Handle function input - can be single function or named list
      if (is.function(fun)) {
        # Single function - use for all supported input types
        self$fun = fun
      } else if (is.list(fun)) {
        # Named list of functions for different input types
        if (!all(names(fun) %in% c("score", "probability"))) {
          stop("Named function list may only contain 'score' and/or 'probability' entries.")
        }
        if (!all(input_supported %in% names(fun))) {
          stop("All input_supported types must have corresponding functions in the named list.")
        }
        self$fun = fun
      } else {
        stop("'fun' must be either a function or a named list of functions.")
      }
    },

    #' @description
    #' Get the loss function for a specific input type.
    #'
    #' @param input_type (`character(1)`)\cr
    #'   The input type: `"score"` or `"probability"`.
    #' @return A function for the specified input type.
    get_fun = function(input_type = self$input_default) {
      checkmate::assert_choice(input_type, choices = self$input_supported)

      if (is.function(self$fun)) {
        return(self$fun)
      } else {
        return(self$fun[[input_type]])
      }
    }
  )
)

#' @title Dictionary of loss functions
#'
#' @description
#' Dictionary of loss functions.
#'
#' @export
dict_loss = R6::R6Class("DictionaryLoss", inherit = Dictionary, cloneable = FALSE)$new()

#' @title Retrieve loss function
#'
#' @description
#' Retrieve a loss function from the dictionary. If additional parameters are provided,
#' they will be used to customize the loss function (e.g., quantile for pinball loss).
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve the object.
#' @template param_dots
#'
#' @export
lss = function(.key, ...) {
  # Resolve mlr3-style aliases first
  .key_resolved = resolve_loss_key(.key)
  # Get the base loss function from dictionary
  base_loss = dict_loss$get(.key_resolved)

  # Check if additional parameters were provided
  params = list(...)

  if (length(params) == 0) {
    # No additional parameters, return the original loss function
    return(base_loss)
  } else {
    # Additional parameters provided, create a customized loss function
    # Handle both single function and multi-function cases
    if (is.function(base_loss$fun)) {
      original_fun = base_loss$fun
      new_fun = function(r) {
        do.call(original_fun, c(list(r), params))
      }
    } else {
      # Multiple functions - apply parameters to each
      new_fun = lapply(base_loss$fun, function(original_fun) {
        function(r) {
          do.call(original_fun, c(list(r), params))
        }
      })
    }

    # Create parameter string for the label
    param_str = paste(names(params), params, sep = "=", collapse = ", ")
    new_label = paste0(base_loss$label, " (", param_str, ")")

    # Create new LossFunction with custom parameters
    return(LossFunction$new(
      id = paste0(base_loss$id, "_custom"),
      label = new_label,
      task_type = base_loss$task_type,
      fun = new_fun,
      input_default = base_loss$input_default,
      input_supported = base_loss$input_supported
    ))
  }
}

# Internal: mapping of mlr3 measure ids to vistool loss keys
.loss_key_aliases = c(
  "regr.mse" = "l2_se",
  "regr.rmse" = "l2_se",
  "regr.mae" = "l1_ae",
  "regr.huber" = "huber",
  "classif.logloss" = "cross-entropy",
  "classif.ce" = "zero-one",
  "classif.brier" = "brier"
)

#' Resolve Loss Key (Internal)
#' @keywords internal
resolve_loss_key = function(key) {
  if (key %in% names(.loss_key_aliases)) .loss_key_aliases[[key]] else key
}

dict_loss$add(
  "l2_se",
  LossFunction$new("l2", "L2 Squared Error", "regr",
    function(r) (r)^2,
    input_default   = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "l1_ae",
  LossFunction$new("l1", "L1 Absolute Error", "regr",
    function(r) abs(r),
    input_default   = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "huber",
  LossFunction$new("huber", "Huber Loss", "regr",
    function(r, delta = 1) {
      a = abs(r)
      ifelse(a <= delta, 0.5 * a^2, delta * a - delta^2 / 2)
    },
    input_default = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "log-cosh",
  LossFunction$new("logcosh", "Log-Cosh Loss", "regr",
    function(r) log(cosh(r)),
    input_default   = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "cross-entropy",
  LossFunction$new("logloss", "Logistic Loss", "classif",
    fun = list(
      score = function(r) log(1 + exp(-r)),
      probability = function(pi) -log(pi)
    ),
    input_default = "score",
    input_supported = c("score", "probability")
  )
)

dict_loss$add(
  "zero-one",
  LossFunction$new("zero-one", "0-1 Loss", "classif",
    fun = list(
      score = function(r) as.numeric(r <= 0),
      probability = function(pi) as.numeric(pi <= 0.5)
    ),
    input_default = "score",
    input_supported = c("score", "probability")
  )
)

dict_loss$add(
  "hinge",
  LossFunction$new("hinge", "Hinge Loss", "classif",
    function(r) pmax(1 - r, 0),
    input_default   = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "log-barrier",
  LossFunction$new("log-barrier", "Log-Barrier Loss", "regr",
    function(r, epsilon = 1) {
      abs_res = abs(r)
      abs_res[abs_res > epsilon] = Inf
      abs_res[abs_res <= epsilon] = -epsilon^2 * log(1 - (abs_res[abs_res <= epsilon] / epsilon)^2)
      abs_res
    },
    input_default = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "epsilon-insensitive",
  LossFunction$new("epsilon-insensitive", "Epsilon-Insensitive Loss", "regr",
    function(r, epsilon = 1) ifelse(abs(r) > epsilon, abs(r) - epsilon, 0),
    input_default = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "pinball",
  LossFunction$new("pinball", "Pinball Loss", "regr",
    function(r, quantile = 0.5) ifelse(r < 0, (1 - quantile) * (-r), quantile * r),
    input_default = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "cauchy",
  LossFunction$new("cauchy", "Cauchy Loss", "regr",
    function(r, epsilon = 1) 0.5 * epsilon^2 * log(1 + (r / epsilon)^2),
    input_default = "score",
    input_supported = "score"
  )
)

dict_loss$add(
  "brier",
  LossFunction$new("brier", "Brier Score", "classif",
    function(pi) (1 - pi)^2,
    input_default   = "probability",
    input_supported = "probability"
  )
)


#' @title Convert dictionary to data table
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
as.data.table.DictionaryLoss = function(x, ..., objects = FALSE) {
  checkmate::assert_flag(objects)

  setkeyv(mlr3misc::map_dtr(x$keys(), function(key) {
    t = x$get(key)
    mlr3misc::insert_named(
      list(key = key, label = t$label, task_type = t$task_type),
      if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
