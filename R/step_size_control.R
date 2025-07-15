#' Assertion for the main signature of a `stepSizeControlXX` function.
#' @param x (`numeric()`) The "old" point.
#' @param u (`numeric()`) The update added to `x` without a step size control.
#' @param obj (`Objective`) The usd objective object.
#' @param opt (`Optimizer`) The optimizer object from which the function is called.
#' @export
assertStepSizeControl <- function(x, u, obj, opt) {
  checkmate::assertR6(obj, "Objective")
  checkmate::assertR6(opt, "Optimizer")
  obj$assertX(checkmate::assertNumeric(x))
  obj$assertX(checkmate::assertNumeric(u))
}

#' Conduct line search in each iteration to adjust the update.
#' @param lower (`numeric(1)`) The lower bound for the step_size.
#' @param upper (`numeric(1)`) The upper bound for the step_size.
#' @template return_step_size
#' @export
stepSizeControlLineSearch <- function(lower = 0, upper = 10) {
  checkmate::assertNumber(lower)
  checkmate::assertNumber(upper, lower = lower)
  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)
    f <- function(a) {
      dir <- ifelse(obj$minimize, -1, 1)
      obj$eval(x + dir * a * u)
    }
    res <- optimize(f, lower = lower, upper = upper, maximum = !obj$minimize)
    return(res[[ifelse(obj$minimize, "minimum", "maximum")]])
  }
}

#' Conduct time decay to adjust the update.
#' See https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler
#' @template param_decay
#' @template return_step_size
#' @export
stepSizeControlDecayTime <- function(decay = 0.01) {
  checkmate::assertNumber(decay, lower = 0, upper = 1)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch <- nrow(obj$archive)
    return(1 / (1 + decay * epoch))
  }
}

#' Conduct exponential decay to adjust the update.
#' See https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler
#' @template param_decay
#' @template return_step_size
#' @export
stepSizeControlDecayExp <- function(decay = 0.01) {
  checkmate::assertNumber(decay, lower = 0, upper = 1)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch <- nrow(obj$archive)
    return(exp(-decay * epoch))
  }
}

#' Conduct linear decay to adjust the update.
#' See https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler
#' @param iter_zero (`integer(1)`) The iteration at which the update is shrinked to zero.
#' @template return_step_size
#' @export
stepSizeControlDecayLinear <- function(iter_zero = 100L) {
  checkmate::assertIntegerish(iter_zero, lower = 0, len = 1)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch <- nrow(obj$archive)
    if (epoch > iter_zero) {
      warning("Learning rate already decayed to 0, progressing further doesn't have any effect.")
    }
    decay <- 1 - (epoch / iter_zero)
    return(ifelse(decay < 0, 0, decay))
  }
}

#' Conduct a step-wise decay to adjust the update.
#' See https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler
#' @param drop_rate (`numeric(1)`) The rate indicating how much the learning rate is reduced
#' after each `every_iter`.
#' @param every_iter (`integer(1)`) Number indicates after how many iterations the
#' learning rate is reduced by `drop_rate`.
#' @template return_step_size
#' @export
stepSizeControlDecaySteps <- function(drop_rate = 0.1, every_iter = 10) {
  checkmate::assertNumber(drop_rate, lower = 0, upper = 1)
  checkmate::assertIntegerish(every_iter, lower = 1, len = 1L)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch <- nrow(obj$archive)
    return((1 - drop_rate)^(floor(epoch / every_iter)))
  }
}
