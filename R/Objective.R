#' @title Objective function
#'
#' @description
#' This class defines the objective that is used for optimization.
#'
#' @export
Objective = R6::R6Class("Objective",
  public = list(
    #' @template field_id
    id = NULL,

    #' @field label (`character(1)` The label of the objective, i.e. a.
    label = NULL,

    #' @field lower (`numeric()`) The lower limits for each dimension.
    lower = NA,

    #' @field upper (`numeric()`) The upper limits for each dimension.
    upper = NA,

    #' @field minimize (`logical(1)`) Is the problem a minimization problem?
    minimize = FALSE,

    # store evals as (x, fval)
    # x = listcol, unclear whether thats best, we can always add an unwrapper

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @template param_id
    #' @param fun (`function` The objective function. The first argument must be a numerical input of length `xdim`.
    #' @param label (`character(1)` The label of the objective, i.e. a.
    #' @param xdim (`integer(1)`) The input dimension of `fun`. Use `xdim = NA` for an arbitrary input dimension.
    #' @param lower (`numeric(xdim)`) The lower boundaries for inputs to `fun`. Must
    #' @param upper (`numeric(xdim)`) The upper boundaries for inputs to `fun`. Must
    #' be of length `xdim`.
    #' @param xtest (`numeric()`) Test value for `fun` during initialization. If not defined,
    #' `xtest = rep(0, ifelse(is.na(xdim), 2, xdim))` is used.
    #' @param minimize (`logical(1)`) Is the problem a minimization problem? Default is no (`FALSE`).
    #' @template param_dots_fun
    #' @template return_self_invisible
    initialize = function(id, fun, label = "f", xdim, lower = NA,
                          upper = NA, xtest = NULL, minimize = FALSE, ...) {
      self$id = assertString(id)
      self$label = assertString(label)
      self$minimize = assertLogical(minimize, len = 1L)
      private$p_fun = assertFunction(fun)
      private$p_xdim = assertCount(xdim, na.ok = TRUE, positive = TRUE, coerce = TRUE)

      private$p_fargs = list(...)
      if ("x" %in% names(private$p_fargs)) {
        stop("`x` is reserved for the input to `fun` please use another additional argument name.")
      }

      # using xtest=0 that might break if origin is outofbound?
      # OTOH unlikely and we dont really consider bounds atm
      if (is.null(xtest)) {
        xtest = rep(0, ifelse(is.na(xdim), 2, xdim))
      }
      private$p_xtest = self$assert_x(xtest)
      assertNumber(self$eval(xtest)) # check that fun works as expected

      self$add_log_fun(function(x, fval, grad) l2norm(grad), "gnorm")
      if (!is.na(lower[1])) self$lower = self$assert_x(lower)
      if (!is.na(upper[1])) self$upper = self$assert_x(upper)

      return(invisible(self))
    },

    #' @description Evaluate the objective function.
    #' @param x (`numeric`) The numerical input of `fun`.
    #' @return The result of `fun(x)`.
    eval = function(x) {
      if (!is.na(private$p_xdim)) {
        assertNumeric(x, len = private$p_xdim)
      }
      return(do.call(private$p_fun, c(list(x = x), private$p_fargs)))
    },

    #' @description Evaluate the objective function and log into the archive. Each call logs
    #' the input vector `x`, result of fun `fval`, the gradient `grad`, the norm of the gradient
    #' `gnorm`, and additional logs that were added by `$add_log_fun`.
    #' @param x (`numeric`) The numerical input of `fun`.
    #' @return Invisible list of logs that are added to the archive.
    eval_store = function(x) {
      if (!is.na(private$p_xdim)) {
        assertNumeric(x, len = private$p_xdim)
      }
      fval = self$eval(x)
      grad = self$grad(x)

      dlogs = list(x = list(x), fval = fval, grad = list(grad))
      alogs = lapply(private$p_log_funs, function(f) f(x, fval, grad))
      names(alogs) = names(private$p_log_funs)

      ilog = c(dlogs, alogs)
      private$p_archive = rbind(private$p_archive, ilog)

      return(invisible(ilog))
    },

    #' @description Assert a numeric input if it is suitable or not.
    #' @param x (`numeric()`) Input value for `fun`.
    #' @param ... Additional arguments passed to `assertNumeric(...)`.
    assert_x = function(x, ...) {
      if (is.na(private$p_xdim)) {
        return(assertNumeric(x, ...))
      } else {
        return(assertNumeric(x, len = private$p_xdim, ...))
      }
    },

    #' @description Evaluate the gradient of the objective function at x.
    #' @param x (`numeric`) The numerical input of `fun`.
    grad = function(x) {
      if (is.null(private$p_gradient)) {
        return(do.call(private$p_gradient_fallback, c(list(x = x), private$p_fargs)))
        # return(private$p_gradient_fallback(x))
      } else {
        return(do.call(private$p_gradient, c(list(x = x), private$p_fargs)))
        # return(private$p_gradient(x))
      }
    },

    #' @description Evaluate the hessian of the objective function at x.
    #' @param x (`numeric`) The numerical input of `fun`.
    hess = function(x) {
      if (is.null(private$p_hessian)) {
        # return(private$p_hessian_fallback(x))
        return(do.call(private$p_hessian_fallback, c(list(x = x), private$p_fargs)))
      } else {
        # return(private$p_hessian(x))
        return(do.call(private$p_hessian, c(list(x = x), private$p_fargs)))
      }
    },

    #' @description Method to add custom logger to the objective.
    #' @param l (`function`) Function that returns a single numerical value or a string.
    #' The arguments of `l` must be `x`, `fval` and `grad`.
    #' @param label (`character(1)`) The name of the logger.
    add_log_fun = function(l, label) {
      assertFunction(l, c("x", "fval", "grad"))
      xtest = private$p_xtest
      testfval = self$eval(xtest)
      testgrad = self$grad(xtest)
      e = try(l(xtest, testfval, testgrad), silent = TRUE)
      if (inherits(e, "try-error")) {
        stop("Error in `$add_log_fun`: ", attr(e, "condition")$message)
      } else {
        checked = FALSE
        if (is.numeric(e)) {
          assertNumber(e)
          checked = TRUE
        }
        if (is.character(e) || is.factor(e)) {
          assertString(as.character(e))
        }
        if (!checked) {
          stop("Function did not return a single numerical value or string of length one")
        }
      }
      il = list(l)
      names(il) = label
      private$p_log_funs = c(private$p_log_funs, il)
    },

    #' @description Delete the archive.
    clear_archive = function() {
      private$p_archive = data.table()
    }
  ),
  active = list(
    #' @field archive (`data.table()`) Archive of all calls to `$eval_store`.
    archive = function(x) {
      if (!missing(x)) stop("`archive` is read only")
      return(private$p_archive)
    },

    #' @field log_funs (`list()`) A list containing logging functions. Each function must have argument.
    log_funs = function(x) {
      if (!missing(x)) stop("`log_funs` is read only")
      return(private$p_log_funs)
    },

    #' @field xdim (`integer(1)`) Input dimension of `f`.
    xdim = function(x) {
      if (!missing(x)) stop("`xdim` is read only")
      return(private$p_xdim)
    }
  ),
  private = list(
    # @field fun (`function`) The objective function. The first argument must be a numerical input of length `xdim`.
    p_fun = NULL,

    # @field xdim (`integer(1)`) The input dimension of `fun`.
    p_xdim = NULL,

    # @field xtest (`numeric()`) The test value to check functions.
    p_xtest = NULL,

    # @field p_archive (`data.table`) The archive or logs, a new entry is added for each call to `eval_store`.
    p_archive = data.table(),

    # @field p_gradient (`function`) A function that calculate the gradient of `fun` at `x`.
    # If not defined, `rootSolve::gradient` is used as fallback.
    p_gradient = NULL,

    # @field p_hessian (`function`) A function that calculate the hessian of `fun` at `x`.
    # If not defined, `rootSolve::hessian` is used as fallback.
    p_hessian = NULL,

    # @field p_log_funs (`list(`) A list containing logging functions. Each function must have argument.
    # `x`, `fval`, `grad`.
    p_log_funs = list(),
    p_fargs = list(),
    p_gradient_fallback = function(x, ...) rootSolve::gradient(f = private$p_fun, x = x, ...)[1, ],
    p_hessian_fallback = function(x, ...) rootSolve::hessian(f = private$p_fun, x = x, ...)
  )
)

l2norm = function(x) sqrt(sum(crossprod(x)))

# Internal helper to build elastic net penalty components.
build_penalty_components = function(lambda_l1, lambda_l2, mask) {
  mask = assertLogical(mask, any.missing = FALSE)
  p = length(mask)
  idx = which(mask)

  zero_grad = rep(0, p)
  zero_hessian = matrix(0, p, p)

  value_fun = function(theta) {
    if (length(idx) == 0) {
      return(0)
    }

    value = 0
    if (lambda_l2 > 0) {
      value = value + 0.5 * lambda_l2 * sum(theta[idx]^2)
    }
    if (lambda_l1 > 0) {
      value = value + lambda_l1 * sum(abs(theta[idx]))
    }
    value
  }

  grad_fun = if (lambda_l2 > 0 || lambda_l1 > 0) {
    function(theta) {
      grad = zero_grad
      if (length(idx) > 0) {
        if (lambda_l2 > 0) {
          grad[idx] = grad[idx] + lambda_l2 * theta[idx]
        }
        if (lambda_l1 > 0) {
          grad[idx] = grad[idx] + lambda_l1 * sign(theta[idx])
        }
      }
      grad
    }
  } else {
    function(theta) zero_grad
  }

  hess_fun = if (lambda_l2 > 0 && length(idx) > 0) {
    diag_template = zero_grad
    diag_template[idx] = lambda_l2
    function(theta) diag(diag_template)
  } else {
    function(theta) zero_hessian
  }

  list(value = value_fun, grad = grad_fun, hess = hess_fun)
}

#' @title Logistic regression objective
#'
#' @description
#' Builds a logistic regression objective with optional elastic net penalization.
#'
#' @param x (`matrix()`)
#'   Design matrix without an intercept column. Rows correspond to observations.
#' @param y (`numeric()`)
#'   Binary responses taking values in `{0, 1}`.
#' @param weights (`numeric()`)
#'   Optional non-negative weights with length `nrow(x)`.
#' @param lambda (`numeric(1)`)
#'   Overall penalty strength used together with `alpha` for elastic net penalties.
#' @param alpha (`numeric(1)`)
#'   Elastic net mixing parameter in `[0, 1]`. `0` corresponds to ridge, `1` to lasso.
#' @param lambda_l1 (`numeric(1)`)
#'   Optional direct specification of the L1 penalty weight. Overrides `lambda * alpha` when provided.
#' @param lambda_l2 (`numeric(1)`)
#'   Optional direct specification of the L2 penalty weight. Overrides `lambda * (1 - alpha)` when provided.
#' @param include_intercept (`logical(1)`)
#'   If `TRUE`, an intercept column is prepended to `x` before constructing the objective.
#' @param penalize_intercept (`logical(1)`)
#'   If `FALSE` and `include_intercept = TRUE`, the intercept term is excluded from the penalties.
#' @param loss_scale (`numeric(1)`)
#'   Scaling factor applied to the empirical risk. Defaults to `1 / (2 * sum(weights))`.
#' @param id (`character(1)`)
#'   Identifier forwarded to [Objective]. Defaults to `"logreg"`.
#' @param label (`character(1)`)
#'   Label used for the resulting objective. Defaults to `"logistic risk"`.
#'
#' @return An [Objective] instance capturing the logistic regression risk.
#' @importFrom stats plogis
#' @export
objective_logistic = function(x, y, weights = NULL, lambda = 0, alpha = 0,
                              lambda_l1 = NULL, lambda_l2 = NULL,
                              include_intercept = TRUE, penalize_intercept = FALSE,
                              loss_scale = NULL, id = NULL, label = NULL) {
  x = as.matrix(x)
  assertMatrix(x, mode = "numeric", any.missing = FALSE)
  n = nrow(x)
  if (n == 0) {
    stop("`x` must contain at least one observation.")
  }

  y = as.numeric(y)
  assertNumeric(y, len = n, any.missing = FALSE)
  if (!all(y %in% c(0, 1))) {
    stop("`y` must contain only 0/1 values.")
  }

  if (is.null(weights)) {
    weights = rep(1, n)
  } else {
    weights = as.numeric(weights)
    assertNumeric(weights, len = n, any.missing = FALSE)
    if (any(weights < 0)) {
      stop("`weights` must be non-negative.")
    }
  }

  include_intercept = assertFlag(include_intercept)
  penalize_intercept = assertFlag(penalize_intercept)

  assertNumber(lambda, lower = 0)
  assertNumber(alpha, lower = 0, upper = 1)

  if (is.null(lambda_l1)) {
    lambda_l1 = lambda * alpha
  } else {
    lambda_l1 = assertNumber(lambda_l1, lower = 0)
  }

  if (is.null(lambda_l2)) {
    lambda_l2 = lambda * (1 - alpha)
  } else {
    lambda_l2 = assertNumber(lambda_l2, lower = 0)
  }

  sum_weights = sum(weights)
  if (sum_weights <= 0) {
    stop("`weights` must sum to a positive value.")
  }

  if (is.null(loss_scale)) {
    loss_scale = 1 / (2 * sum_weights)
  } else {
    loss_scale = assertNumber(loss_scale, lower = 0)
  }

  if (include_intercept) {
    x = cbind(Intercept = 1, x)
  }

  mask = rep(TRUE, ncol(x))
  if (include_intercept && !penalize_intercept) {
    mask[1L] = FALSE
  }

  penalty = build_penalty_components(lambda_l1, lambda_l2, mask)

  logistic_value = function(x, design, y, weights, loss_scale, penalty) {
    theta = x
    eta = drop(design %*% theta)
    base = loss_scale * sum(weights * (log1p(exp(eta)) - y * eta))
    base + penalty$value(theta)
  }

  logistic_grad = function(x, design, y, weights, loss_scale, penalty) {
    theta = x
    eta = drop(design %*% theta)
    probs = plogis(eta)
    diff = (probs - y) * weights
    grad = as.numeric(loss_scale * crossprod(design, diff))
    grad + penalty$grad(theta)
  }

  logistic_hess = function(x, design, y, weights, loss_scale, penalty) {
    theta = x
    eta = drop(design %*% theta)
    probs = plogis(eta)
    weight_vec = weights * probs * (1 - probs)
    if (all(weight_vec == 0)) {
      base = matrix(0, ncol(design), ncol(design))
    } else {
      scaled_design = design * sqrt(weight_vec)
      colnames(scaled_design) = NULL
      base = loss_scale * crossprod(scaled_design, scaled_design)
    }
    base + penalty$hess(theta)
  }

  objective = Objective$new(
    id = if (is.null(id)) "logreg" else assertString(id),
    label = if (is.null(label)) "logistic risk" else assertString(label),
    fun = logistic_value,
    xdim = ncol(x),
    minimize = TRUE,
    design = x,
    y = y,
    weights = weights,
    loss_scale = loss_scale,
    penalty = penalty
  )

  objective$.__enclos_env__$private$p_gradient = logistic_grad
  objective$.__enclos_env__$private$p_hessian = logistic_hess

  objective
}

#' @title Linear regression objective
#'
#' @description
#' Builds a squared-error regression objective with optional elastic net penalization.
#'
#' @inheritParams objective_logistic
#' @param y (`numeric()`)
#'   Numeric responses for the regression task.
#' @param loss_scale (`numeric(1)`)
#'   Scaling factor applied to the summed squared errors. Defaults to `1 / (2 * sum(weights))`.
#' @return An [Objective] instance capturing the squared-error regression risk.
#' @export
objective_linear = function(x, y, weights = NULL, lambda = 0, alpha = 0,
                            lambda_l1 = NULL, lambda_l2 = NULL,
                            include_intercept = TRUE, penalize_intercept = FALSE,
                            loss_scale = NULL, id = NULL, label = NULL) {
  x = as.matrix(x)
  assertMatrix(x, mode = "numeric", any.missing = FALSE)
  n = nrow(x)
  if (n == 0) {
    stop("`x` must contain at least one observation.")
  }

  y = as.numeric(y)
  assertNumeric(y, len = n, any.missing = FALSE)

  if (is.null(weights)) {
    weights = rep(1, n)
  } else {
    weights = as.numeric(weights)
    assertNumeric(weights, len = n, any.missing = FALSE)
    if (any(weights < 0)) {
      stop("`weights` must be non-negative.")
    }
  }

  include_intercept = assertFlag(include_intercept)
  penalize_intercept = assertFlag(penalize_intercept)

  assertNumber(lambda, lower = 0)
  assertNumber(alpha, lower = 0, upper = 1)

  if (is.null(lambda_l1)) {
    lambda_l1 = lambda * alpha
  } else {
    lambda_l1 = assertNumber(lambda_l1, lower = 0)
  }

  if (is.null(lambda_l2)) {
    lambda_l2 = lambda * (1 - alpha)
  } else {
    lambda_l2 = assertNumber(lambda_l2, lower = 0)
  }

  sum_weights = sum(weights)
  if (sum_weights <= 0) {
    stop("`weights` must sum to a positive value.")
  }

  if (is.null(loss_scale)) {
    loss_scale = 1 / (2 * sum_weights)
  } else {
    loss_scale = assertNumber(loss_scale, lower = 0)
  }

  if (include_intercept) {
    x = cbind(Intercept = 1, x)
  }

  mask = rep(TRUE, ncol(x))
  if (include_intercept && !penalize_intercept) {
    mask[1L] = FALSE
  }

  penalty = build_penalty_components(lambda_l1, lambda_l2, mask)

  linear_value = function(x, design, y, weights, loss_scale, penalty) {
    theta = x
    residual = y - drop(design %*% theta)
    base = loss_scale * sum(weights * residual^2)
    base + penalty$value(theta)
  }

  linear_grad = function(x, design, y, weights, loss_scale, penalty) {
    theta = x
    residual = y - drop(design %*% theta)
    grad = -2 * loss_scale * as.numeric(crossprod(design, weights * residual))
    grad + penalty$grad(theta)
  }

  linear_hess = function(x, design, y, weights, loss_scale, penalty) {
    theta = x
    if (all(weights == 0)) {
      base = matrix(0, ncol(design), ncol(design))
    } else {
      scaled_design = design * sqrt(weights)
      colnames(scaled_design) = NULL
      base = 2 * loss_scale * crossprod(scaled_design, scaled_design)
    }
    base + penalty$hess(theta)
  }

  objective = Objective$new(
    id = if (is.null(id)) "linreg" else assertString(id),
    label = if (is.null(label)) "squared error risk" else assertString(label),
    fun = linear_value,
    xdim = ncol(x),
    minimize = TRUE,
    design = x,
    y = y,
    weights = weights,
    loss_scale = loss_scale,
    penalty = penalty
  )

  objective$.__enclos_env__$private$p_gradient = linear_grad
  objective$.__enclos_env__$private$p_hessian = linear_hess

  objective
}

#' @title Objective dictionary
#'
#' @description
#' Lookup table for reusable [Objective] definitions. The dictionary ships with
#' vistool and comes pre-populated with regularized regression objectives as
#' well as a selection of benchmark test functions when the optional
#' `TestFunctions` package is available. New objectives can be registered at
#' runtime via [mlr3misc::Dictionary] methods such as `$add()`.
#'
#' @format A [mlr3misc::Dictionary] where each entry is an [Objective] instance.
#'
#' @examples
#' dict_objective$get("TF_branin")
#'
#' @seealso [obj()] for convenient retrieval.
#'
#' @export
dict_objective = R6::R6Class("DictionaryObjective",
  inherit = mlr3misc::Dictionary,
  cloneable = FALSE
)$new()

# Register regression objectives provided by vistool.
if (!dict_objective$has("logreg")) {
  # Supply minimal prototypes so callers can inspect metadata without user data
  dict_objective$add(
    "logreg",
    objective_logistic,
    .prototype_args = list(
      x = matrix(0, nrow = 1, ncol = 1),
      y = c(0)
    )
  )
}
if (!dict_objective$has("linreg")) {
  dict_objective$add(
    "linreg",
    objective_linear,
    .prototype_args = list(
      x = matrix(0, nrow = 1, ncol = 1),
      y = c(0)
    )
  )
}

tfuns = c(
  # Canonical domains stored as evaluation bounds.
  # For Branin we use the canonical, unscaled internal formula `TF_branin` from TestFunctions
  # with its standard domain x1 in [-5, 10], x2 in [0, 15]. We keep `name` for labeling, and
  # specify `fun_symbol` for the actual function symbol to resolve from the namespace.
  list(list(minimize = TRUE, name = "branin", fun_symbol = "TF_branin", desc = "Branin function (canonical domain). 2D.", xdim = 2, lower = c(-5, 0), upper = c(10, 15))),
  list(list(minimize = TRUE, name = "borehole", desc = "A function estimating water flow through a borehole. 8 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1.5, 1))),
  list(list(minimize = FALSE, name = "franke", desc = "A function. 2 dimensional function.", xdim = 2, lower = c(-0.5, -0.5), upper = c(1, 1))),
  list(list(minimize = FALSE, name = "zhou1998", desc = "A function. 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = TRUE, name = "currin1991", desc = "A function. 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = FALSE, name = "banana", desc = "A banana shaped function. 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = FALSE, name = "sinumoid", desc = "A sinusoid added to a sigmoid function. 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = FALSE, name = "waterfall", desc = "A sinusoid added to a sigmoid function. 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = TRUE, name = "GoldsteinPrice", desc = "Goldstein-Price function. Exponential scale, you might want to use GoldsteinPriceLog instead 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = TRUE, name = "GoldsteinPriceLog", desc = "Goldstein-Price function on a log scale. 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = TRUE, name = "beale", desc = "Beale function 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = TRUE, name = "easom", desc = "Easom function 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = TRUE, name = "hump", desc = "Hump function 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = FALSE, name = "quad_peaks", desc = "quad_peaks function 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = FALSE, name = "quad_peaks_slant", desc = "quad_peaks_slant function 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = TRUE, name = "ackley", desc = "Ackley function. 2 dimensional function.", xdim = 2, lower = c(0, 0), upper = c(1, 1))),
  list(list(minimize = FALSE, name = "gaussian1", desc = "A Gaussian function centered at 0.5. Any dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "sqrtsin", desc = "A square root of a sine function. Any dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "powsin", desc = "A sine function raised to a power keeping its original sign. Any dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "OTL_Circuit", desc = "OTL Circuit. 6 dimensional function.", xdim = 6, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "piston", desc = "Piston simulation function. 7 dimensional function", xdim = 7, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "wingweight", desc = "Wing weight function. 10 dimensional function.", xdim = 10, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "robotarm", desc = "Robot arm function. 8 dimensional function.", xdim = 8, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "RoosArnold", desc = "Roos & Arnold (1963) function. d dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "Gfunction", desc = "G-function d dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "griewank", desc = "Griewank function n dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "levy", desc = "Levy function n dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "michalewicz", desc = "Michalewicz function n dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "rastrigin", desc = "Rastrigin function n dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "linkletter_nosignal", desc = "Linkletter (2006) no signal function, just returns zero d dimensional function.", xdim = NA, lower = NA, upper = NA)),
  list(list(minimize = FALSE, name = "hartmann", desc = "hartmann function 6 dimensional function.", xdim = 6, lower = NA, upper = NA))
)

if (requireNamespace("TestFunctions", quietly = TRUE)) {
  ns_tf = asNamespace("TestFunctions")
  for (i in seq_along(tfuns)) {
    tf = tfuns[[i]]
    id = sprintf("TF_%s", tf$name)

    # Resolve function symbol: default to `name`, allow override via `fun_symbol`.
    sym = if (!is.null(tf$fun_symbol)) tf$fun_symbol else tf$name
    cl_fun = get0(sym, envir = ns_tf, inherits = FALSE, ifnotfound = NULL)

    if (is.null(cl_fun) || !is.function(cl_fun)) {
      message("Error retrieving '", sym, "' from namespace TestFunctions, skipping.")
      next
    }

    suppressWarnings(dict_objective$add(
      id, Objective$new(
        fun = cl_fun,
        id = id, label = tf$name, xdim = tf$xdim, lower = tf$lower,
        upper = tf$upper, minimize = tf$minimize
      )
    ))
  }
} else {
  message("Package 'TestFunctions' not available; skipping registration of test functions in dict_objective.")
}

#' @title Retrieve objective functions
#'
#' @description
#' Retrieve an objective function from the dictionary.
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to the respective [dictionary][mlr3misc::Dictionary] to retrieve the object.
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, or to be set as public field.
#'   See [mlr3misc::dictionary_sugar_get()] for more details.
#'
#' @export
obj = function(.key, ...) {
  dict_objective$get(.key, ...)
}

#' @export
as.data.table.DictionaryObjective = function(x, ..., objects = FALSE) {
  data.table::setkeyv(mlr3misc::map_dtr(x$keys(), function(key) {
    t = x$get(key, .prototype = TRUE)
    mlr3misc::insert_named(
      c(list(
        key = key, label = t$label, xdim = t$xdim, lower = list(t$lower),
        upper = list(t$upper)
      )), if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
