assertStepSizeControl = function(x, u, obj, opt) {
  checkmate::assertR6(obj, "Objective")
  checkmate::assertR6(opt, "Optimizer")
  obj$assertX(checkmate::assertNumeric(x))
  obj$assertX(checkmate::assertNumeric(u))
}

stepSizeControlLineSearch = function(lower = 0, upper = 10) {
  checkmate::assertIntegerish(lower, len = 1L)
  checkmate::assertIntegerish(upper, len = 1L, lower = lower)
  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)
    f = function(a) {
      dir = ifelse(obj$minimize, -1, 1)
      obj$eval(x + dir * a * u)
    }
    res = optimize(f, lower = lower, upper = upper, maximum = ! obj$minimize)
    return(res[[ifelse(obj$minimize, "minimum", "maximum")]])
  }
}

# See https://neptune.ai/blog/how-to-choose-a-learning-rate-scheduler
stepSizeControlDecayTime = function(decay = 0.01) {
  checkmate::assertNumber(decay, lower = 0, upper = 1)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch = nrow(obj$archive)
    return(1 / (1 + decay * epoch))
  }
}

stepSizeControlDecayExp = function(decay = 0.01) {
  checkmate::assertNumber(decay, lower = 0, upper = 1)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch = nrow(obj$archive)
    return(exp(-decay * epoch))
  }
}

stepSizeControlDecayLinear = function(iter_zero = 100L) {
  checkmate::assertNumber(iter_zero, lower = 0)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch = nrow(obj$archive)
    if (epoch > iter_zero) {
      warning("Learning rate already decayed to 0, progressing further doesn't have any effect.")
    }
    decay = 1 - (epoch / iter_zero)
    return(ifelse(decay < 0, 0, decay))
  }
}

stepSizeControlDecaySteps = function(drop_rate = 0.1, every_iter = 10) {
  checkmate::assertNumber(drop_rate, lower = 0, upper = 1)
  checkmate::assertIntegerish(every_iter, lower = 1, len = 1L)

  function(x, u, obj, opt) {
    assertStepSizeControl(x, u, obj, opt)

    epoch = nrow(obj$archive)
    return((1 - drop_rate)^(floor(epoch / every_iter)))
  }
}



if (FALSE) {
devtools::load_all()

obj = tfun_dict$get("TF_banana")
opt = OptimizerGD$new(obj, x_start = c(0.6, 0.6), step_size = 0.001, id = "GD x0=(0.6, 0.6)", print_trace = FALSE)
opt$optimize(steps = 10)

opt2 = OptimizerGD$new(obj, x_start = c(0.6, 0.6), step_size = 0.001, id = "GD x0=(0.6, 0.6) line search", print_trace = FALSE)
opt2$optimize(steps = 10, stepSizeControl = stepSizeControlLineSearch)
}

