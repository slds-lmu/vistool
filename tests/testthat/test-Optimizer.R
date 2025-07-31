test_that("OptimizerGD creation and basic functionality works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5),
    minimize = TRUE
  )

  opt = OptimizerGD$new(obj, x_start = c(1, 1), lr = 0.1)

  expect_s3_class(opt, "OptimizerGD")
  expect_s3_class(opt, "Optimizer")
  expect_equal(opt$lr, 0.1)
  expect_equal(opt$x, c(1, 1))
  expect_equal(nrow(opt$archive), 0)
})

test_that("OptimizerGD optimization works", {
  obj = Objective$new(
    id = "quadratic",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5),
    minimize = TRUE
  )

  opt = OptimizerGD$new(obj, x_start = c(2, 2), lr = 0.1, print_trace = FALSE)

  # Initial function value
  initial_fval = obj$eval(c(2, 2))

  # Optimize for a few steps
  opt$optimize(steps = 5L)

  # Check that archive was updated
  expect_equal(nrow(opt$archive), 5)
  expect_true(all(c("x_out", "x_in", "fval_out", "fval_in", "lr") %in% colnames(opt$archive)))

  # Check that we're moving towards minimum
  final_fval = opt$archive$fval_out[nrow(opt$archive)]
  expect_true(final_fval < initial_fval)
})

test_that("OptimizerMomentum works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5),
    minimize = TRUE
  )

  opt = OptimizerMomentum$new(obj, x_start = c(1, 1), lr = 0.1, momentum = 0.9, print_trace = FALSE)

  expect_s3_class(opt, "OptimizerMomentum")
  expect_equal(opt$momentum, 0.9)

  # Test optimization
  opt$optimize(steps = 3L)
  expect_equal(nrow(opt$archive), 3)
  expect_true("momentum" %in% colnames(opt$archive))
})

test_that("OptimizerNAG works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5),
    minimize = TRUE
  )

  opt = OptimizerNAG$new(obj, x_start = c(1, 1), lr = 0.1, momentum = 0.9, print_trace = FALSE)

  expect_s3_class(opt, "OptimizerNAG")
  expect_equal(opt$momentum, 0.9)

  # Test optimization
  opt$optimize(steps = 3L)
  expect_equal(nrow(opt$archive), 3)
})

test_that("Optimizer parameter updates work", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 1,
    lower = -5,
    upper = 5,
    minimize = TRUE
  )

  opt = OptimizerGD$new(obj, x_start = 1, lr = 0.1, print_trace = FALSE)

  # Test learning rate updates
  opt$lr = 0.2
  expect_equal(opt$lr, 0.2)

  # Test position updates
  opt$setX(2)
  expect_equal(opt$x, 2)
})

test_that("Optimizer with step size control works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5),
    minimize = TRUE
  )

  opt = OptimizerGD$new(obj, x_start = c(1, 1), lr = 0.1, print_trace = FALSE)

  # Test with custom step size control
  step_control = function(x, u, obj, opt) 0.5 # Half step size
  opt$optimize(steps = 2L, stepSizeControl = step_control)

  expect_equal(nrow(opt$archive), 2)
  expect_true(all(opt$archive$step_size == 0.5))
})

test_that("mergeOptimArchives works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5),
    minimize = TRUE
  )

  opt1 = OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(1, 1), lr = 0.1, id = "GD1", print_trace = FALSE)
  opt2 = OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(-1, -1), lr = 0.05, id = "GD2", print_trace = FALSE)

  opt1$optimize(steps = 2L)
  opt2$optimize(steps = 3L)

  merged = mergeOptimArchives(opt1, opt2)

  expect_s3_class(merged, "data.table")
  expect_equal(nrow(merged), 5) # 2 + 3
  expect_true("optim_id" %in% colnames(merged))
  expect_true(all(c("GD1", "GD2") %in% merged$optim_id))
})

test_that("Optimizer input validation works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-5, -5),
    upper = c(5, 5)
  )

  # Invalid learning rate
  expect_error(
    OptimizerGD$new(obj, x_start = c(1, 1), lr = -0.1),
    "Assertion on 'lr' failed"
  )

  # Invalid momentum
  expect_error(
    OptimizerMomentum$new(obj, x_start = c(1, 1), lr = 0.1, momentum = -0.1),
    "Assertion on 'momentum' failed"
  )
})

test_that("Optimizer minimize/maximize works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 1,
    lower = -5,
    upper = 5,
    minimize = FALSE # Maximize
  )

  opt = OptimizerGD$new(obj, x_start = 0, lr = 0.1, print_trace = FALSE)
  opt$optimize(steps = 3L)

  # Should move away from 0 when maximizing x^2
  expect_true(abs(opt$x) > 0)
})
