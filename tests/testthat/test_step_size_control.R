test_that("step_size_control_decay_time works", {
  control = step_size_control_decay_time(decay = 0.1)
  expect_true(is.function(control))

  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 1,
    lower = -5,
    upper = 5
  )

  opt = OptimizerGD$new(obj, x_start = 1, lr = 0.1, print_trace = FALSE)

  # Test that step size decreases over time
  # First step (epoch 0)
  obj$eval_store(1) # Add to archive
  step_size_1 = control(x = 1, u = 0.1, obj = obj, opt = opt)

  # Second step (epoch 1)
  obj$eval_store(0.9) # Add another to archive
  step_size_2 = control(x = 0.9, u = 0.1, obj = obj, opt = opt)

  expect_true(step_size_2 < step_size_1)
  expect_true(step_size_1 <= 1) # Should be <= 1
})

test_that("step_size_control_decay_exp works", {
  control = step_size_control_decay_exp(decay = 0.05)
  expect_true(is.function(control))

  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 1,
    lower = -5,
    upper = 5
  )

  opt = OptimizerGD$new(obj, x_start = 1, lr = 0.1, print_trace = FALSE)

  # Test exponential decay
  obj$eval_store(1)
  step_size_1 = control(x = 1, u = 0.1, obj = obj, opt = opt)

  obj$eval_store(0.9)
  step_size_2 = control(x = 0.9, u = 0.1, obj = obj, opt = opt)

  expect_true(step_size_2 < step_size_1)
  expect_true(step_size_1 <= 1)
})

test_that("step_size_control_decay_linear works", {
  control = step_size_control_decay_linear(iter_zero = 10)
  expect_true(is.function(control))

  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 1,
    lower = -5,
    upper = 5
  )

  opt = OptimizerGD$new(obj, x_start = 1, lr = 0.1, print_trace = FALSE)

  # Test linear decay
  obj$eval_store(1)
  step_size_1 = control(x = 1, u = 0.1, obj = obj, opt = opt)

  # Add more evaluations to simulate progression
  for (i in 2:5) {
    obj$eval_store(1 - i * 0.1)
  }

  step_size_5 = control(x = 0.5, u = 0.1, obj = obj, opt = opt)

  expect_true(step_size_5 < step_size_1)
})

test_that("step_size_control_decay_steps works", {
  control = step_size_control_decay_steps(drop_rate = 0.8, every_iter = 3)
  expect_true(is.function(control))

  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 1,
    lower = -5,
    upper = 5
  )

  opt = OptimizerGD$new(obj, x_start = 1, lr = 0.1, print_trace = FALSE)

  # Test step decay
  for (i in 1:6) {
    obj$eval_store(1 - i * 0.1)
  }

  step_size_6 = control(x = 0.4, u = 0.1, obj = obj, opt = opt)
  expect_true(step_size_6 <= 1)
})

test_that("step_size_control_line_search works", {
  control = step_size_control_line_search(lower = 0, upper = 2)
  expect_true(is.function(control))

  obj = Objective$new(
    id = "test",
    fun = function(x) x^2, # Simple quadratic
    xdim = 1,
    lower = -5,
    upper = 5,
    minimize = TRUE
  )

  opt = OptimizerGD$new(obj, x_start = 1, lr = 0.1, print_trace = FALSE)

  # Test line search (should find optimal step size)
  step_size = control(x = 1, u = -0.1, obj = obj, opt = opt) # u should be negative for minimization

  expect_true(step_size >= 0)
  expect_true(step_size <= 2)
  expect_true(is.numeric(step_size))
})

test_that("assert_step_size_control works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 1,
    lower = -5,
    upper = 5
  )

  opt = OptimizerGD$new(obj, x_start = 1, lr = 0.1, print_trace = FALSE)

  expect_silent(assert_step_size_control(x = 1, u = 0.1, obj = obj, opt = opt))
  expect_error(assert_step_size_control(x = "invalid", u = 0.1, obj = obj, opt = opt))
  expect_error(assert_step_size_control(x = 1, u = "invalid", obj = obj, opt = opt))
})

test_that("Step size control integration with optimizer works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5),
    minimize = TRUE
  )

  opt = OptimizerGD$new(obj, x_start = c(2, 2), lr = 0.1, print_trace = FALSE)

  # Test with decay control
  control = step_size_control_decay_time(decay = 0.1)
  opt$optimize(steps = 3L, step_size_control = control)

  expect_equal(nrow(opt$archive), 3)
  expect_true("step_size" %in% colnames(opt$archive))

  # Step sizes should be decreasing
  step_sizes = opt$archive$step_size
  expect_true(step_sizes[2] <= step_sizes[1])
  expect_true(step_sizes[3] <= step_sizes[2])
})

test_that("Step size control parameter validation works", {
  expect_error(step_size_control_decay_time(decay = -0.1), "Assertion on 'decay' failed")
  expect_error(step_size_control_decay_time(decay = 1.1), "Assertion on 'decay' failed")
  expect_error(step_size_control_decay_exp(decay = -0.1), "Assertion on 'decay' failed")
  expect_error(step_size_control_decay_exp(decay = 1.1), "Assertion on 'decay' failed")
  expect_error(step_size_control_decay_linear(iter_zero = -1), "Assertion on 'iter_zero' failed")
  expect_error(step_size_control_line_search(lower = 1, upper = 0.5), "Assertion on 'upper' failed")
})
