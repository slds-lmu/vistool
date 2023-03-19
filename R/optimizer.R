# question: do we enforce that x is always 2d?
#   currently not, as we maybe want to benchmark in some d, then show at least
#   y-print_traces. restriction seems to have no value, currently

#' Optimizer class
#'
#' This class defines the optimization technique.
#' @export
Optimizer = R6::R6Class("Optimizer",
  public = list(

    #' @field id (`character(1)` The id of the objective.
    id = NULL,

    #' @field print_trace (`logical(1)` Indicator whether to print the status of `$optimize()`.
    print_trace = TRUE,

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param objective (`Objective`) The objective to optimize.
    #' @param x_start (`numeric()`) Start value of the optimization. Note, after
    #' the first call of `$optimize()` the last value is used to continue
    #' optimization. Get this value with `$x`.
    #' @param id (`character(1)`) Id of the object.
    #' @param print_trace (`logical(1)`) Indicator whether to print the status of `$optimize()`.
    initialize = function(objective, x_start, id = NULL, print_trace = TRUE) {
      private$p_objective = checkmate::assertR6(objective$clone(deep = TRUE), "Objective")
      private$p_x_start = objective$assertX(x_start)
      private$p_x = private$p_x_start
      self$id = checkmate::assertString(id, null.ok = TRUE)
      self$print_trace = checkmate::assertLogical(print_trace, len = 1L)

      return(invisible(self))
    },

    #' @description Prepare updates for adding them to the archive.
    #' @param x_out (`numeric()`) The new proposed point by the optimizer.
    #' @param x_in (`numeric()`) The old input value which is updated to `x_out`.
    #' @param update (`numeric()`) The update from `x_in` to `x_out`.
    #' @param fval_out (`numeric(1)`) The objective value `objetive$eval(x_out)`.
    #' @param fval_in (`numeric(1)`) The objective value `objetive$eval(x_in)`.
    #' @param lr (`numeric(1)`) The learning rate used to multiply `update` with.
    #' @param step_size (`numeric(1)`) The step_size used to multiply `lr * update` with.
    #' @param objective (`Objective`) The objective used by `$optimize()`.
    #' @param step (`integer(1)`) The step or iteration.
    #' @param ... Additional objects added to the archive (e.g. `momentum`).
    #' @return `data.table()` of the input arguments.
    prepareUpdateForArchive = function(x_out, x_in, update, fval_out, fval_in, lr, step_size, objective, step, ...) {

      out = data.table(
        x_out = list(x_out),
        x_in = list(x_in),
        update = list(update),
        fval_out = fval_out,
        fval_in = fval_in,
        lr = as.numeric(lr),
        step_size = as.numeric(step_size),
        objective = list(objective))
      out = cbind(out, do.call(data.table, list(...)))
      out$step = step

      return(out)
    },


    #' @description Add points to the archive.
    #' @param ain `data.table()` with names "x_out", "x_in", "update", "fval_out",
    #' "fval_in", "lr", "objective", and "step".
    updateArchive = function(ain) {
      checkmate::assertDataTable(ain, min.rows = 1L)
      batch = ifelse(nrow(private$p_archive) == 0, 1, max(private$p_archive$batch) + 1)

      ain$batch = batch
      private$p_archive = rbind(private$p_archive, ain)

      if (self$print_trace) private$p_printer(ain)

    },

    #' @description Set the current input vector used as start point of `$optimize()`.
    #' @param x (`numeric()`) Input vector.
    setX = function(x) {
      private$p_x = private$p_objective$assertX(x)
    }
  ),
  active = list(
    #' @field lr (`numeric(1`) Step size of the algorithm.
    lr = function(x) {
      if (! missing(x)) {
        private$p_lr = checkmate::assertNumber(x, lower = 0)
      } else {
        return(private$p_lr)
      }
    },

    #' @field archive (`data.table()`) Archive of all calls to `$evalStore`.
    archive = function(x) {
      if (! missing(x)) stop("`archive` is read only")
      return(private$p_archive)
    },

    #' @field objective (`Objective`) The objective function.
    objective = function(x) {
      if (! missing(x)) stop("`objective` is read only")
      return(private$p_objective)
    },

    #' @field x (`numeric()`) The numerical input vector used as starting point by `$optimize()`.
    x = function(x) {
      if (! missing(x)) stop("`x` is read only")
      return(private$p_x)
    }
  ),
  private = list(
    p_x = NULL,
    p_archive = data.table(),

    p_lr = 0.01,

    p_objective = NULL,
    p_x_start = NULL,

    p_printer = function(ain) {
      xvals = vapply(ain$x_out, function(x) sprintf("c(%s)", paste(round(x, 4), collapse = ", ")), character(1))
      msg = sprintf("%s: Batch %s step %s: f(x) = %s, x = %s", ain$objective[[1]]$id, ain$batch,
        ain$step, round(ain$fval_out, 4), xvals)
      message(paste(msg, collapse = "\n"))
    }
  )
)

#' Momentum optimizer
#'
#' This class defines momentum.
#' @export
OptimizerMomentum = R6::R6Class("OptimizerMomentum", inherit = Optimizer,
  public = list(

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param objective (`Objective`) The objective to optimize.
    #' @param x_start (`numeric()`) Start value of the optimization. Note, after
    #' the first call of `$optimize()` the last value is used to continue
    #' optimization. Get this value with `$x`.
    #' @param lr (`numeric(1)`) Step size with which the update is multiplied.
    #' @param momentum (`numeric(1)`) Momentum value.
    #' @param id (`character(1)`) Id of the object.
    #' @param print_trace (`logical(1)`) Indicator whether to print the status of `$optimize()`.
    initialize = function(objective, x_start, lr = 0.01, momentum = 0.9, id = "Momentum", print_trace = TRUE) {
      super$initialize(objective$clone(deep = TRUE), x_start, id, print_trace) # assert in super

      private$p_lr = checkmate::assertNumber(lr, lower = 0)
      private$p_momentum = checkmate::assertNumber(momentum, lower = 0)

      return(invisible(self))
    },

    #FIXME: implement a generic step size control mechanism here.
    # constant, armijo, downschedule
    #' @description Optimize `steps` iteration.
    #' @param steps (`integer(1)`) Number of steps/iterations.
    #' @param stepSizeControl (`function()`) A function with arguments `x` (the old input value),
    #' `u` (the upate generated by `$update()`), `obj` (the objective object), and `opt` the optimizer as `self`.
    #' Default simply returns 1 to include the update as is. Note that the update `u` is calculated based on the
    #' formula with learning rate, e.g., for GD x_new = x_old + lr * g, with update u = lr * g. Step size control takes
    #' x_old and u (with included learning rate) and calculates a value that is used to extend or compress u for the
    #' final update.
    #' @param minimize (`logical(1)`) Indicator to whether minimize or optimize the objective. The default (`NULL`)
    #' uses the option defined in `objective$minimize`.
    optimize = function(steps = 1L, stepSizeControl = function(x, u, obj, opt) return(1), minimize = NULL) {
      #checkmate::assertR6(objective, "Objective")
      checkmate::assertCount(steps, positive = TRUE)
      checkmate::assertFunction(stepSizeControl, args = c("x", "u", "obj", "opt"))
      checkmate::assertLogical(minimize, len = 1L, null.ok = TRUE)

      if (is.null(minimize)) {
        minimize = self$objective$minimize
      }

      lldt = list()
      for (step in seq_len(steps)) {
        u = self$update(private$p_lr, private$p_momentum)
        step_size = stepSizeControl(super$x, u, super$objective, self)

        if (minimize) u = -u

        x_new = super$x + step_size * u

        lldt = c(lldt, list(super$prepareUpdateForArchive(x_new, super$x, u,
          super$objective$eval(x_new), super$objective$eval(super$x), private$p_lr,
          step_size, super$objective, step, momentum = private$p_momentum)))
        super$setX(x_new)

      }
      super$updateArchive(do.call(rbind, lldt))

      return(invisible(self))
    },

    #' @description Calculate the update for `x`
    #' @param lr (`numeric(1)`) The learning rate.
    #' @param mom (`numeric(1)`) The momentum.
    update = function(lr, mom) {
      checkmate::assertNumber(lr, lower = 0)
      checkmate::assertNumber(mom, lower = 0)

      g = super$objective$evalStore(super$objective$assertX(super$x))$grad[[1]]
      u = mom * private$p_grad_old + lr * g

      private$p_grad_old = u

      return(u)
    }
  ),
  active = list(
    #' @field momentum (`numeric(1)`) Momentum of the algorithm.
    momentum = function(x) {
      if (! missing(x)) {
        private$p_momentum = checkmate::assertNumber(x, lower = 0)
      } else {
        return(private$p_momentum)
      }
    }
  ),
  private = list(
    p_momentum = 0.9,
    p_grad_old = 0
  )
)

#' Gradient descent optimizer
#'
#' This class defines gradient descent
#' @export
OptimizerGD = R6::R6Class("OptimizerGD", inherit = OptimizerMomentum,
  public = list(

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param objective (`Objective`) The objective to optimize.
    #' @param x_start (`numeric()`) Start value of the optimization. Note, after
    #' the first call of `$optimize()` the last value is used to continue
    #' optimization. Get this value with `$x`.
    #' @param lr (`numeric(1)`) Step size with which the update is multiplied.
    #' @param id (`character(1)`) Id of the object.
    #' @param print_trace (`logical(1)`) Indicator whether to print the status of `$optimize()`.
    initialize = function(objective, x_start, lr = 0.01, id = "Gradient Descent", print_trace = TRUE) {
      super$initialize(objective, x_start, lr, 0, id, print_trace) # assert in super
      private$p_lr = checkmate::assertNumber(lr, lower = 0)

      return(invisible(self))
    }
  ),
  active = list(
    #' @field momentum (`numeric(1)`) Momentum of the algorithm.
    momentum = function(x) {
      stop("Momentum has no effect for `OptimizerGD`.")
    }
  )
)

#' Nesterovs momentum optimizer
#'
#' This class defines Nesterovs momentum using Nesterov accelerated gradient (NAG).
#' @export
OptimizerNAG = R6::R6Class("OptimizerNAG", inherit = Optimizer,
  public = list(

    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param objective (`Objective`) The objective to optimize.
    #' @param x_start (`numeric()`) Start value of the optimization. Note, after
    #' the first call of `$optimize()` the last value is used to continue
    #' optimization. Get this value with `$x`.
    #' @param lr (`numeric(1)`) Step size with which the update is multiplied.
    #' @param momentum (`numeric(1)`) Momentum value.
    #' @param id (`character(1)`) Id of the object.
    #' @param print_trace (`logical(1)`) Indicator whether to print the status of `$optimize()`.
    initialize = function(objective, x_start, lr = 0.01, momentum = 0.9, id = "NAG", print_trace = TRUE) {
      super$initialize(objective$clone(deep = TRUE), x_start, id, print_trace) # assert in super

      private$p_lr = checkmate::assertNumber(lr, lower = 0)
      private$p_momentum = checkmate::assertNumber(momentum, lower = 0)

      return(invisible(self))
    },

    #FIXME: implement a generic step size control mechanism here.
    # constant, armijo, downschedule
    #' @description Optimize `steps` iteration.
    #' @param steps (`integer(1)`) Number of steps/iterations.
    #' @param stepSizeControl (`function()`) A function with arguments `x` (the old input value),
    #' `u` (the upate generated by `$update()`), and `obj` (the objective object).
    #' @param minimize (`logical(1)`) Indicator to whether minimize or optimize the objective. The default (`NULL`)
    #' uses the option defined in `objective$minimize`.
    optimize = function(steps = 1L, stepSizeControl = function(x, u, obj, opt) return(1), minimize = NULL) {
      #checkmate::assertR6(objective, "Objective")
      checkmate::assertCount(steps, positive = TRUE)
      checkmate::assertFunction(stepSizeControl, args = c("x", "u", "obj"))
      checkmate::assertLogical(minimize, len = 1L, null.ok = TRUE)

      if (is.null(minimize)) {
        minimize = self$objective$minimize
      }

      lldt = list()
      for (step in seq_len(steps)) {
        u = self$update(private$p_lr, private$p_momentum, minimize)
        step_size = stepSizeControl(super$x, u, super$objective)

        if (minimize) u = -u

        x_new = super$x + step_size * u

        lldt = c(lldt, list(super$prepareUpdateForArchive(x_new, super$x, u,
          super$objective$evalStore(x_new)$fval, super$objective$eval(super$x),
          private$p_lr, step_size, super$objective, step, momentum = private$p_momentum)))
        super$setX(x_new)

      }
      super$updateArchive(do.call(rbind, lldt))

      return(invisible(self))
    },

    #' @description Calculate the update for `x`
    #' @param lr (`numeric(1)`) The learning rate.
    #' @param mom (`numeric(1)`) The momentum.
    #' @param minimize (`logical(1)`) Indicator to whether minimize or optimize the objective (default = `TRUE`).
    update = function(lr, mom, minimize) {
      checkmate::assertNumber(lr, lower = 0)
      checkmate::assertNumber(mom, lower = 0)

      mf = 1
      if (minimize) mf = -1

      mgo = mom * private$p_grad_old
      lookahead = super$objective$assertX(super$x) + mf * mgo
      glookahead = super$objective$grad(lookahead)
      u = mgo + lr * glookahead

      private$p_grad_old = u

      return(u)
    }
  ),
  active = list(
    #' @field momentum (`numeric(1)`) Momentum of the algorithm.
    momentum = function(x) {
      if (! missing(x)) {
        private$p_momentum = checkmate::assertNumber(x, lower = 0)
      } else {
        return(private$p_momentum)
      }
    }
  ),
  private = list(
    p_momentum = 0.9,
    p_grad_old = 0
  )
)

#' Merge optimization archives
#' @param ... Optimization objects.
#' @export
mergeOptimArchives = function(...) {
  opts = list(...)
  lapply(opts, checkmate::assertR6, classes = "Optimizer")

  common_opt = c("x_out", "x_in", "update", "fval_out", "fval_in", "lr", "step_size")
  arxs = do.call(rbind, lapply(opts, function(o) {
    oa = o$archive
    out = try({
      ax = as.data.table(do.call(cbind, lapply(common_opt, function(fn) {
        v = oa[[fn]]
        if (is.null(v)) v = NA
        return(v)
      })))
      names(ax) = common_opt
      ax$optim_id = o$id
      ax$gnorm = o$objective$archive$gnorm
      ax$iteration = seq_len(nrow(ax))
      ax
    }, silent = TRUE)
    if (inherits(out, "try-error")) {
      stop(sprintf("Error for optimizer '%s'\n%s", o$id, attr(out, "condition")$message))
    }
    # Ugly, but columns are returned as list otherwise ... Must be fixed.
    cnums = c("fval_out", "fval_in", "lr", "step_size")
    for (cn in cnums) {
      out[[cn]] = as.numeric(out[[cn]])
    }
    return(out)
  }))
  return(arxs)
}
