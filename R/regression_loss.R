# TODO check if RegressionLoss & Objective should descend from ABC

#' Regression loss
#'
#' This class defines the regression loss to visualize.
#' @examples
#' rl = RegressionLoss$new()
#' @export
RegressionLoss = R6::R6Class("RegressionLoss",
  public = list(
    #' @field id (`character(1)` The id of the objective.
    id = NULL,

    # TODO: how to manage and use labels?
    #' @field label (`character(1)` The label of the objective, i.e. a.
    label = NULL,

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param id (`character(1)` The id of the objective.
    #' @param fun (`function` The objective function. The first argument must be a numerical input of length `xdim`.
    #' @param label (`character(1)` The label of the objective, i.e. a.
        #' @param xtest (`numeric()`) Test value for `fun` during initialization. If not defined,
    #' `xtest = 0` is used.
    #' @param ... Additional arguments passed to `fun`.
    initialize = function(id, fun, label = "f", xtest = NULL, ...) {
      self$id = checkmate::assertString(id)
      self$label = checkmate::assertString(label)
      private$p_fun = checkmate::assertFunction(fun)
      private$p_fargs = list(...)
      if ("x" %in% names(private$p_fargs)) {
        stop("`x` is reserved for the input to `fun`, please use another additional argument name.")
      }
      if (is.null(xtest)) xtest = 0
      private$p_xtest = self$assertX(xtest)
      checkmate::assertNumber(self$eval(xtest)) # check that fun works as expected
      return(invisible(self))
    },
    
    #' @description Reset additional arguments of objective function.
    # #' @param ... Additional arguments passed to `fun`.
    reset_fargs = function(...) {
        private$p_fargs = mlr3misc::insert_named(private$p_fargs, list(...))
        checkmate::assertNumber(self$eval(private$p_xtest)) # check that fun works as expected
    },

    #' @description Evaluate the objective function.
    #' @param x (`numeric`) The numerical input of `fun`.
    # #' @param ... Additional arguments passed to `fun`.
    #' @return The result of `fun(x)`.
    eval = function(x) do.call(private$p_fun, c(list(x = x), private$p_fargs)),

    #' @description Assert a numeric input if it is suitable or not.
    #' @param x (`numeric()`) Input value for `fun`.
    #' @param ... Additional arguments passed to `checkmate::assertNumeric(...)`.
    assertX = function(x, ...) checkmate::assertNumeric(x, len = 1, ...)),

  ),
  private = list(
    # @field fun (`function`) The objective function. The first argument must be a numerical input of length `xdim`.
    p_fun = NULL,

    # @field xtest (`numeric()`) The test value to check functions.
    p_xtest = NULL,

    p_fargs = list(),
  )
)

# ADD PRE-DEFINED OBJECTIVES ---------------------------------------------------

l2norm = function(x) sqrt(sum(crossprod(x)))

rlosses = c(
  list(list(minimize = TRUE, name = "l2", desc = "Quadratic regression loss.")),
  list(list(minimize = TRUE, name = "l1", desc = "Absolute regression loss.")),
  list(list(minimize = TRUE, name = "huber", desc = "Huber regression loss.")),
  list(list(minimize = TRUE, name = "logcosh", desc = "Log-cosh regression loss.")),
  list(list(minimize = TRUE, name = "logbarrier", desc = "Log-barrier regression loss.")),
  list(list(minimize = TRUE, name = "epsinsensitive", desc = "Epsilon-insensitive regression loss.")),
  list(list(minimize = TRUE, name = "pinball", desc = "Pinball aka quantile regression loss.")),
  list(list(minimize = TRUE, name = "cauchy", desc = "Cauchy regression loss."))
)
rlossfuns = list(
  l2 = list(fun = function(x) sum(x**2), args = list()),
  l1 = list(fun = function(x) sum(abs(x)), args = list()),
  huber = list(
    fun = function(x, epsilon) {
      sum(
        ifelse(
          abs(x) <= epsilon, 0.5 * x**2, epsilon * abs(x) - 0.5 * epsilon**2
        )
      )
    },
    args = list(epsilon = 0.5)
  ),
  logcosh = list(fun = function(x) sum(log(cosh(abs(x)))), args = list()),
  logbarrier = list(
    fun = function(x, epsilon) {
      abs_res <- abs(x)
      abs_res[abs_res > epsilon] <- Inf
      abs_res[abs_res <= epsilon] <- -epsilon**2 * log(
        1 - (abs_res[abs_res <= epsilon] / epsilon)**2
      )
      sum(abs_res)
    },
    args = list(epsilon = 1)
  ),
  epsinsensitive = list(
    fun = function(x, epsilon) {
      sum(ifelse(abs(x) > epsilon, abs(x) - epsilon, 0L))
    },
    args = list(epsilon = 1)
  ),
  pinball = list(
    fun = function(x, quantile) {
      sum(ifelse(x < 0, ((1 - quantile) * (-x)), quantile * x))
    },
    args = list(quantile = 0.5)
  ),
  cauchy = list(
    fun = function(x, epsilon) {
      sum(0.5 * epsilon**2 * log(1 + (x / epsilon)**2))
    },
    args = list(epsilon = 1)
  )
)

rloss_dict = R6::R6Class(
  "DictionaryLoss", inherit = mlr3misc::Dictionary, cloneable = FALSE
)$new()

for (i in seq_along(rlosses)) {
  rl = rlosses[[i]]
  id = sprintf("RL_%s", rl$name)
  args_list = append(
    list(
      fun = rlossfuns[[rl$name]]$fun,
      id = id, 
      label = rl$name, 
      minimize = rl$minimize
    ),
    rlossfuns[[rl$name]]$args
  )
  rloss_dict$add(id, do.call(RegressionLoss$new, args_list))
}

#' @export
as.data.table.DictionaryLoss = function(x, ..., objects = FALSE) {
  
  data.table::setkeyv(mlr3misc::map_dtr(x$keys(), function(key) {
    t = x$get(key)
    mlr3misc::insert_named(
      c(list(key = key, label = t$label)), if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
