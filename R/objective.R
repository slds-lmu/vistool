#' Objective function
#'
#' This class defines the objective that is used for optimization.
#' @examples
#' ob = Objective$new()
#' @export
Objective = R6::R6Class("Objective",
  public = list(
    #' @field id (`character(1)` The id of the objective.
    id = NULL,

    # TODO: how to manage and use labels?
    #' @field label (`character(1)` The label of the objective, i.e. a.
    label = NULL,

    #' @field limits_lower (`numeric()`) The lower limits for each dimension.
    limits_lower = NA,

    #' @field limits_upper (`numeric()`) The upper limits for each dimension.
    limits_upper = NA,

    #' @field minimize (`logical(1)`) Is the problem a minimization problem?
    minimize = FALSE,

    # store evals as (x, fval)
    # x = listcol, unclear whether thats best, we can always add an unwrapper

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param id (`character(1)` The id of the objective.
    #' @param fun (`function` The objective function. The first argument must be a numerical input of length `xdim`.
    #' @param label (`character(1)` The label of the objective, i.e. a.
    #' @param xdim (`integer(1)`) The input dimension of `fun`. Use `xdim = NA` for an arbitrary input dimension.
    #' @param limits_lower (`numeric(xdim)`) The lower boundaries for inputs to `fun`. Must
    #' @param limits_upper (`numeric(xdim)`) The upper boundaries for inputs to `fun`. Must
    #' be of length `xdim`.
    #' @param xtest (`numeric()`) Test value for `fun` during initialization. If not defined,
    #' `xtest = rep(0, ifelse(is.na(xdim), 2, xdim))` is used.
    #' @param minimize (`logical(1)`) Is the problem a minimization problem? Default is no (`FALSE`).
    #' @param ... Additional arguments passed to `fun`.
    initialize = function(id, fun, label = "f", xdim, limits_lower = NA,
      limits_upper = NA, xtest = NULL, minimize = FALSE, ...) {

      self$id = checkmate::assertString(id)
      self$label = checkmate::assertString(label)
      self$minimize = checkmate::assertLogical(minimize, len = 1L)
      private$p_fun = checkmate::assertFunction(fun)
      private$p_xdim = checkmate::assertCount(xdim, na.ok = TRUE, positive = TRUE, coerce = TRUE)

      private$p_fargs = list(...)
      if ("x" %in% names(private$p_fargs)) {
        stop("`x` is reserved for the input to `fun`, please use another additional argument name.")
      }

      # using xtest=0 that might break if origin is outofbound?
      # OTOH unlikely and we dont really consider bounds atm
      if (is.null(xtest)) {
        xtest = rep(0, ifelse(is.na(xdim), 2, xdim))
      }
      private$p_xtest = self$assertX(xtest)
      checkmate::assertNumber(self$eval(xtest)) # check that fun works as expected

      self$addLogFun(function(x, fval, grad) l2norm(grad), "gnorm")
      if (! is.na(limits_lower[1])) self$limits_lower = self$assertX(limits_lower)
      if (! is.na(limits_upper[1])) self$limits_upper = self$assertX(limits_upper)

      return(invisible(self))
    },
    
    #' @description Reset additional arguments of objective function.
    # #' @param ... Additional arguments passed to `fun`.
    reset_fargs = function(...) {
        private$p_fargs = mlr3misc::insert_named(private$p_fargs, list(...))
        checkmate::assertNumber(self$eval(private$p_xtest)) # check that fun works as expected
    },
    
    #' @description Reset dimension of x (in regression, depending on task).
    #' @param dim (`numeric(1)`) Dimension of parameter (incl. intercept).
    reset_xdim = function(dim) {
      checkmate::assertCount(dim, positive = TRUE)
      private$p_xdim = dim
    },

    #' @description Evaluate the objective function.
    #' @param x (`numeric`) The numerical input of `fun`.
    # #' @param ... Additional arguments passed to `fun`.
    #' @return The result of `fun(x)`.
    eval = function(x) {
      if (! is.na(private$p_xdim)) {
        checkmate::assertNumeric(x, len = private$p_xdim)
      }
      return(do.call(private$p_fun, c(list(x = x), private$p_fargs)))
    },

    #' @description Evaluate the objective function and log into the archive. Each call logs
    #' the input vector `x`, result of fun `fval`, the gradient `grad`, the norm of the gradient
    #' `gnorm`, and additional logs that were added by `$addLogFun`.
    #' @param x (`numeric`) The numerical input of `fun`.
    # #' @param ... Additional arguments passed to `fun`.
    #' @return Invisible list of logs that are added to the archive.
    evalStore = function(x) {
      if (! is.na(private$p_xdim)) {
        checkmate::assertNumeric(x, len = private$p_xdim)
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
    #' @param ... Additional arguments passed to `checkmate::assertNumeric(...)`.
    assertX = function(x, ...) {
      if (is.na(private$p_xdim)) {
        return(checkmate::assertNumeric(x, ...))
      } else {
        return(checkmate::assertNumeric(x, len = private$p_xdim, ...))
      }
    },

    #' @description Evaluate the gradient of the objective function at x.
    #' @param x (`numeric`) The numerical input of `fun`.
    grad = function(x) {
      if (is.null(private$p_gradient)) {
        return(do.call(private$p_gradientFallback, c(list(x = x), private$p_fargs)))
        #return(private$p_gradientFallback(x))
      } else {
        return(do.call(private$p_gradient, c(list(x = x), private$p_fargs)))
        #return(private$p_gradient(x))
      }
    },

    #' @description Evaluate the hessian of the objective function at x.
    #' @param x (`numeric`) The numerical input of `fun`.
    #' @param ... Additional arguments passed to `fun`.
    hess = function(x) {
      if (is.null(private$p_hessian)) {
        #return(private$p_hessianFallback(x))
        return(do.call(private$p_hessianFallback, c(list(x = x), private$p_fargs)))
      } else {
        #return(private$p_hessian(x))
        return(do.call(private$p_hessian, c(list(x = x), private$p_fargs)))
      }
    },

    #' @description Method to add custom logger to the objective.
    #' @param l (`function`) Function that returns a single numerical value or a string.
    #' The arguments of `l` must be `x`, `fval` and `grad`.
    #' @param label (`character(1)`) The name of the logger.
    #' @param ... Additional arguments passed to `fun`.
    addLogFun = function(l, label) {
      checkmate::assertFunction(l, c("x", "fval", "grad"))
      xtest = private$p_xtest
      testfval = self$eval(xtest)
      testgrad = self$grad(xtest)
      e = try(l(xtest, testfval, testgrad), silent = TRUE)
      if (inherits(e, "try-error")) {
        stop("Error in `$addLogFun`: ", attr(e, "condition")$message)
      } else {
        checked = FALSE
        if (is.numeric(e)) {
          checkmate::assertNumber(e)
          checked = TRUE
        }
        if (is.character(e) || is.factor(e)) {
          checkmate::assertString(as.character(e))
        }
        if (! checked) {
          stop("Function did not return a single numerical value or string of length one")
        }
      }
      il = list(l)
      names(il) = label
      private$p_log_funs = c(private$p_log_funs, il)
    },

    #' @description Delete the archive.
    clearArchive = function() {
      private$p_archive = data.table()
    }
  ),
  active = list(
    #' @field archive (`data.table()`) Archive of all calls to `$evalStore`.
    archive = function(x) {
      if (! missing(x)) stop("`archive` is read only")
      return(private$p_archive)
    },

    #' @field log_funs (`list()`) A list containing logging functions. Each function must have argument.
    log_funs = function(x) {
      if (! missing(x)) stop("`log_funs` is read only")
      return(private$p_log_funs)
    },

    #' @field xdim (`integer(1)`) Input dimension of `f`.
    xdim = function(x) {
      if (! missing(x)) stop("`xdim` is read only")
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

    # @field p_archive (`data.table`) The archive or logs, a new entry is added for each call to `evalStore`.
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

    p_gradientFallback = function(x, ...) rootSolve::gradient(f = private$p_fun, x = x, ...)[1, ],
    p_hessianFallback = function(x, ...) rootSolve::hessian(f = private$p_fun, x = x, ...)
  )
)

# ADD PRE-DEFINED OBJECTIVES ---------------------------------------------------

l2norm = function(x) sqrt(sum(crossprod(x)))

# Define custom objective functions

tfuns = c(list(list(minimize = TRUE, name = "branin", desc = "A function. 2 dimensional function.", xdim = 2, limits_lower = c(-2, -2), limits_upper = c(3, 3))),
  list(list(minimize = TRUE, name = "borehole", desc = "A function estimating water flow through a borehole. 8 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1.5, 1))),
  list(list(minimize = FALSE, name = "franke", desc = "A function. 2 dimensional function.", xdim = 2, limits_lower = c(-0.5, -0.5), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "zhou1998", desc = "A function. 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = TRUE, name = "currin1991", desc = "A function. 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "lim2002", desc = "Some function? 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "banana", desc = "A banana shaped function. 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "sinumoid", desc = "A sinusoid added to a sigmoid function. 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "waterfall", desc = "A sinusoid added to a sigmoid function. 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = TRUE, name = "GoldsteinPrice", desc = "Goldstein-Price function. Exponential scale, you might want to use GoldsteinPriceLog instead 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = TRUE, name = "GoldsteinPriceLog", desc = "Goldstein-Price function on a log scale. 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = TRUE, name = "beale", desc = "Beale function 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = TRUE, name = "easom", desc = "Easom function 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = TRUE, name = "hump", desc = "Hump function 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "quad_peaks", desc = "quad_peaks function 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "quad_peaks_slant", desc = "quad_peaks_slant function 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = TRUE, name = "ackley", desc = "Ackley function. 2 dimensional function.", xdim = 2, limits_lower = c(0, 0), limits_upper = c(1, 1))),
  list(list(minimize = FALSE, name = "gaussian1", desc = "A Gaussian function centered at 0.5. Any dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "sqrtsin", desc = "A square root of a sine function. Any dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "powsin", desc = "A sine function raised to a power keeping its original sign. Any dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "OTL_Circuit", desc = "OTL Circuit. 6 dimensional function.", xdim = 6, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "piston", desc = "Piston simulation function. 7 dimensional function", xdim = 7, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "wingweight", desc = "Wing weight function. 10 dimensional function.", xdim = 10, limits_lower = NA, limits_upper = NA)),
  #list(list(minimize = FALSE, name = "welch", desc = "Welch et al (1992) function. 20 dimensional function.", xdim = 20, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "robotarm", desc = "Robot arm function. 8 dimensional function.", xdim = 8, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "RoosArnold", desc = "Roos & Arnold (1963) function. d dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "Gfunction", desc = "G-function d dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "griewank", desc = "Griewank function n dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "levy", desc = "Levy function n dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "michalewicz", desc = "Michalewicz function n dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "rastrigin", desc = "Rastrigin function n dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  #list(list(minimize = FALSE, name = "moon_high", desc = "Moon (2010) high-dimensional function for screening 20 dimensional function.", xdim = 20, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "linkletter_nosignal", desc = "Linkletter (2006) no signal function, just returns zero d dimensional function.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  #list(list(minimize = FALSE, name = "Morris", desc = "Morris function 20 dimensional function.", xdim = 20, limits_lower = NA, limits_upper = NA)),
  #list(list(minimize = FALSE, name = "detpep8d", desc = "detpep8d function 8 dimensional function.", xdim = 8, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = FALSE, name = "hartmann", desc = "hartmann function 6 dimensional function.", xdim = 6, limits_lower = NA, limits_upper = NA)))

robjectives = c(
  list(list(minimize = TRUE, name = "l2", desc = "Quadratic regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = TRUE, name = "l1", desc = "Absolute regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = TRUE, name = "huber", desc = "Huber regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = TRUE, name = "logcosh", desc = "Log-cosh regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = TRUE, name = "logbarrier", desc = "Log-barrier regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = TRUE, name = "epsinsensitive", desc = "Epsilon-insensitive regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = TRUE, name = "pinball", desc = "Pinball aka quantile regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA)),
  list(list(minimize = TRUE, name = "cauchy", desc = "Cauchy regression loss.", xdim = NA, limits_lower = NA, limits_upper = NA))
)
robjective_funs = list(
  l2 = list(
    fun = function(x, Xmat, y) sum((y - Xmat %*% x)**2),
    args = list(Xmat = matrix(), y = c())
  ),
  l1 = list(
    fun = function(x, Xmat, y) sum(abs(y - Xmat %*% x)),
    args = list(Xmat = matrix(), y = c())
  ),
  huber = list(
    fun = function(x, Xmat, y, epsilon) {
      res <- y - Xmat %*% x
      sum(
        ifelse(
          abs(res) <= epsilon, 
          0.5 * res**2, 
          epsilon * abs(res) - 0.5 * epsilon**2
        )
      )
    },
    args = list(Xmat = matrix(), y = c(), epsilon = 0.5)
  ),
  logcosh = list(
    fun = function(x, Xmat, y) sum(log(cosh(abs(y - Xmat %*% x)))),
    args = list(Xmat = matrix(), y = c())
  ),
  logbarrier = list(
    fun = function(x, Xmat, y, epsilon) {
      abs_res <- abs(y - Xmat %*% x)
      abs_res[abs_res > epsilon] <- Inf
      abs_res[abs_res <= epsilon] <- -epsilon**2 * log(
        1 - (abs_res[abs_res <= epsilon] / epsilon)**2
      )
      sum(abs_res)
    },
    args = list(Xmat = matrix(), y = c(), epsilon = 1)
  ),
  epsinsensitive = list(
    fun = function(x, Xmat, y, epsilon) {
      res <- y - Xmat %*% x
      sum(ifelse(abs(res) > epsilon, abs(res) - epsilon, 0L))
    },
    args = list(Xmat = matrix(), y = c(), epsilon = 1)
  ),
  pinball = list(
    fun = function(x, Xmat, y, quantile) {
      res <- y - Xmat %*% x
      sum(ifelse(res < 0, ((1 - quantile) * (-res)), quantile * res))
    },
    args = list(Xmat = matrix(), y = c(), quantile = 0.5)
  ),
  cauchy = list(
    fun = function(x, Xmat, y, epsilon) {
      sum(0.5 * epsilon**2 * log(1 + ((y - Xmat %*% x) / epsilon)**2))
    },
    args = list(Xmat = matrix(), y = c(), epsilon = 1)
  )
)


# Instantiate dicts

tfun_dict = R6::R6Class("DictionaryObjective", inherit = mlr3misc::Dictionary,
                        cloneable = FALSE)$new()
robj_dict = R6::R6Class(
  "DictionaryObjective", inherit = mlr3misc::Dictionary, cloneable = FALSE
)$new()

# Add custom objectives to dicts

for (i in seq_along(tfuns)) {
  tf = tfuns[[i]]
  id = sprintf("TF_%s", tf$name)
  cl = sprintf("TestFunctions::%s", tf$name)
  suppressWarnings(tfun_dict$add(id, Objective$new(fun = eval(parse(text = cl)),
    id = id, label = tf$name, xdim = tf$xdim, limits_lower = tf$limits_lower,
    limits_upper = tf$limits_upper)))
}
for (i in seq_along(robjectives)) {
    rl = robjectives[[i]]
    id = sprintf("RL_%s", rl$name)
    args_list = append(
        list(
            fun = robjective_funs[[rl$name]]$fun,
            id = id, 
            label = rl$name, 
            xdim = rl$xdim, 
            limits_lower = rl$limits_lower,
            limits_upper = rl$limits_upper,
            minimize = rl$minimize
        ),
        robjective_funs[[rl$name]]$args
    )
    robj_dict$add(id, do.call(Objective$new, args_list))
}

# Export dicts

#' @export
as.data.table.DictionaryObjective = function(x, ..., objects = FALSE) {

  data.table::setkeyv(mlr3misc::map_dtr(x$keys(), function(key) {
    t = x$get(key)
    mlr3misc::insert_named(
      c(list(key = key, label = t$label, xdim = t$xdim, limits_lower = list(t$limits_lower),
        limits_upper = list(t$limits_upper))), if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
