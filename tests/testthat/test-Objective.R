test_that("Objective creation works", {
  # Simple 1D objective
  obj_1d = Objective$new(
    id = "test_1d",
    fun = function(x) x^2,
    xdim = 1,
    lower = -5,
    upper = 5
  )

  expect_s3_class(obj_1d, "Objective")
  expect_equal(obj_1d$id, "test_1d")
  expect_equal(obj_1d$xdim, 1)
  expect_equal(obj_1d$lower, -5)
  expect_equal(obj_1d$upper, 5)
  expect_false(obj_1d$minimize)

  # Simple 2D objective
  obj_2d = Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2),
    minimize = TRUE
  )

  expect_s3_class(obj_2d, "Objective")
  expect_equal(obj_2d$xdim, 2)
  expect_true(obj_2d$minimize)
  expect_equal(length(obj_2d$lower), 2)
  expect_equal(length(obj_2d$upper), 2)
})

test_that("Objective evaluation works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  # Test eval
  result = obj$eval(c(1, 1))
  expect_equal(result, 2)

  # Test evalStore
  stored = obj$evalStore(c(0.5, 0.5))
  expect_equal(stored$fval, 0.5)
  expect_length(stored$x[[1]], 2)

  # Check archive
  expect_true(nrow(obj$archive) >= 1)
})

test_that("Objective gradient and hessian work", {
  # Objective with gradient and hessian
  obj = Objective$new(
    id = "quadratic",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  # Test gradient (should be 2*x for quadratic)
  grad = obj$grad(c(1, 2))
  expect_equal(grad, c(2, 4))

  # Test hessian (hessian fallback uses numerical approximation)
  hess = obj$hess(c(1, 1))
  expect_equal(dim(hess), c(2, 2))
  # For numerical hessian approximation, we can't expect exact values
  expect_true(all(is.finite(hess)))
})

test_that("Objective input validation works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  # Test dimension mismatch
  expect_error(obj$eval(c(1)), "Assertion on 'x' failed")
  expect_error(obj$eval(c(1, 2, 3)), "Assertion on 'x' failed")

  # Test boundary checking with assertX
  valid_x = obj$assertX(c(0.5, 0.5))
  expect_equal(valid_x, c(0.5, 0.5))
})

test_that("Objective archive works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  # Initially empty
  expect_equal(nrow(obj$archive), 0)

  # Add some evaluations
  obj$evalStore(c(0, 0))
  obj$evalStore(c(1, 1))

  expect_equal(nrow(obj$archive), 2)
  expect_true("fval" %in% colnames(obj$archive))
  expect_true("x" %in% colnames(obj$archive))

  # Clear archive
  obj$clearArchive()
  expect_equal(nrow(obj$archive), 0)
})

test_that("Built-in objectives work", {
  # Test TF_branin objective
  obj_branin = obj("TF_branin")
  expect_s3_class(obj_branin, "Objective")
  expect_equal(obj_branin$xdim, 2)

  # Test evaluation
  result = obj_branin$eval(c(0.5, 0.5))
  expect_true(is.numeric(result))
  expect_length(result, 1)

  # Test TF_banana objective
  obj_banana = obj("TF_banana")
  expect_s3_class(obj_banana, "Objective")
  expect_equal(obj_banana$xdim, 2)
})

test_that("Dictionary operations work", {
  # Test dictionary access
  dict = dict_objective
  expect_true(length(dict) > 0)

  # Test getting available objectives
  keys = dict$keys()
  expect_true("TF_branin" %in% keys)
  expect_true("TF_banana" %in% keys)
})

test_that("Objective with custom arguments works", {
  # Objective with additional arguments
  obj = Objective$new(
    id = "custom",
    fun = function(x, scale = 1) scale * sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1),
    scale = 2
  )

  result = obj$eval(c(1, 1))
  expect_equal(result, 4) # 2 * (1^2 + 1^2)
})
