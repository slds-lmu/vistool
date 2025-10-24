test_that("Objective creation works", {
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
  result = obj$eval(c(1, 1))
  expect_equal(result, 2)

  stored = obj$eval_store(c(0.5, 0.5))
  expect_equal(stored$fval, 0.5)
  expect_length(stored$x[[1]], 2)

  expect_true(nrow(obj$archive) >= 1)
})

test_that("Objective gradient and hessian work", {
  obj = Objective$new(
    id = "quadratic",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  grad = obj$grad(c(1, 2))
  expect_equal(grad, c(2, 4))

  hess = obj$hess(c(1, 1))
  expect_equal(dim(hess), c(2, 2))
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

  expect_error(obj$eval(c(1)), "Assertion on 'x' failed")
  expect_error(obj$eval(c(1, 2, 3)), "Assertion on 'x' failed")

  valid_x = obj$assert_x(c(0.5, 0.5))
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

  expect_equal(nrow(obj$archive), 0)

  obj$eval_store(c(0, 0))
  obj$eval_store(c(1, 1))

  expect_equal(nrow(obj$archive), 2)
  expect_true("fval" %in% colnames(obj$archive))
  expect_true("x" %in% colnames(obj$archive))

  obj$clear_archive()
  expect_equal(nrow(obj$archive), 0)
})

test_that("Built-in objectives work", {
  obj_branin = obj("TF_branin")
  expect_s3_class(obj_branin, "Objective")
  expect_equal(obj_branin$xdim, 2)

  result = obj_branin$eval(c(0.5, 0.5))
  expect_true(is.numeric(result))
  expect_length(result, 1)

  obj_banana = obj("TF_banana")
  expect_s3_class(obj_banana, "Objective")
  expect_equal(obj_banana$xdim, 2)
})

test_that("Dictionary operations work", {
  dict = dict_objective
  expect_true(length(dict) > 0)

  keys = dict$keys()
  expect_true("TF_branin" %in% keys)
  expect_true("TF_banana" %in% keys)
})

test_that("Objective with custom arguments works", {
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
