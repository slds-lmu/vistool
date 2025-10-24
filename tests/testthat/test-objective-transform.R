test_that("identity transform preserves base objective outputs", {
  base_fun = function(x) sum(x^2)
  base_grad = function(x) 2 * x
  base_hess = function(x) 2 * diag(length(x))

  obj = Objective$new(
    id = "quad",
    fun = base_fun,
    xdim = 2,
    minimize = TRUE,
    transform = objective_transform_identity(),
    label_base = "quadratic"
  )

  obj$.__enclos_env__$private$p_gradient = base_grad
  obj$.__enclos_env__$private$p_hessian = base_hess

  x = c(1, -2)
  expect_equal(obj$eval(x), base_fun(x))
  expect_equal(obj$grad(x), base_grad(x))
  expect_equal(obj$hess(x), base_hess(x))
  expect_equal(obj$transform_id, "identity")
})

test_that("log transform composes value, gradient, and hessian", {
  design = matrix(c(-1, 0, 1, 2), ncol = 1)
  response = c(0, 1, 0, 1)

  obj_identity = objective_logistic(design, response, transform = objective_transform_identity())
  obj_log = objective_logistic(design, response, transform = objective_transform_log())

  theta = c(0.2, -0.4)

  base_value = obj_identity$eval(theta)
  log_value = obj_log$eval(theta)
  expect_equal(log_value, log(base_value), tolerance = 1e-10)

  base_grad = obj_identity$grad(theta)
  log_grad = obj_log$grad(theta)
  expect_equal(log_grad, base_grad / base_value, tolerance = 1e-10)

  base_hess = obj_identity$hess(theta)
  log_hess = obj_log$hess(theta)
  expected_hess = (-1 / base_value^2) * tcrossprod(base_grad) + (1 / base_value) * base_hess
  expect_equal(log_hess, expected_hess, tolerance = 1e-10)

  expect_equal(obj_log$transform_id, "log")
  expect_equal(obj_log$label, "logistic risk (log)")
})

test_that("set_transform updates identifier and label", {
  design = matrix(c(1, 2, 3), ncol = 1)
  response = c(1.5, 2, 2.5)

  obj = objective_linear(design, response, transform = objective_transform_identity())
  base_label = obj$label

  theta = c(0.5, 1)

  base_value = obj$eval(theta)
  base_grad = obj$grad(theta)
  base_hess = obj$hess(theta)

  obj$set_transform(objective_transform_log())
  expect_equal(obj$transform_id, "log")
  expect_equal(obj$label, sprintf("%s (%s)", base_label, "log"))
  expect_equal(obj$eval(theta), log(base_value), tolerance = 1e-10)
  expect_equal(obj$grad(theta), base_grad / base_value, tolerance = 1e-10)
  expect_equal(
    obj$hess(theta),
    (-1 / base_value^2) * tcrossprod(base_grad) + (1 / base_value) * base_hess,
    tolerance = 1e-10
  )

  obj$set_transform(objective_transform_identity())
  expect_equal(obj$transform_id, "identity")
  expect_equal(obj$label, base_label)
})

test_that("log transform enforces domain restrictions", {
  expect_error(
    Objective$new(
      id = "negative",
      fun = function(x) -1,
      xdim = 1,
      transform = objective_transform_log()
    ),
    "Log transform undefined",
    fixed = FALSE
  )

  obj = Objective$new(
    id = "linear",
    fun = function(x) x[1] + 2,
    xdim = 1,
    xtest = 0,
    transform = objective_transform_identity()
  )

  obj$set_transform(objective_transform_log())
  expect_error(obj$eval(-2), "Log transform undefined", fixed = FALSE)
})
