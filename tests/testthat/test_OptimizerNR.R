test_that("OptimizerNR improves Rosenbrock objective", {
  rosen_fun = function(x) {
    100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
  }
  rosen_grad = function(x) {
    c(
      -400 * x[1] * (x[2] - x[1]^2) + 2 * (x[1] - 1),
      200 * (x[2] - x[1]^2)
    )
  }
  rosen_hess = function(x) {
    matrix(c(
      1200 * x[1]^2 - 400 * x[2] + 2,
      -400 * x[1],
      -400 * x[1],
      200
    ), nrow = 2, byrow = TRUE)
  }

  obj = Objective$new(
    id = "rosen",
    fun = rosen_fun,
    xdim = 2L,
    lower = c(-2, -1),
    upper = c(2, 3),
    minimize = TRUE
  )
  obj$.__enclos_env__$private$p_gradient = rosen_grad
  obj$.__enclos_env__$private$p_hessian = rosen_hess

  opt = OptimizerNR$new(obj, x_start = c(1.8, -0.8), step_size = 1, print_trace = FALSE)
  initial = opt$objective$eval(opt$x)
  expect_no_error(opt$optimize(steps = 10L))
  final = opt$objective$eval(opt$x)

  expect_lt(final, initial)
  expect_gt(nrow(opt$archive), 0)
})

test_that("OptimizerNR falls back to gradient direction when requested", {
  quad_fun = function(x) sum((x - 1)^2)
  quad_grad = function(x) 2 * (x - 1)
  quad_hess = function(x) matrix(0, nrow = 2, ncol = 2)

  obj = Objective$new(
    id = "quad",
    fun = quad_fun,
    xdim = 2L,
    minimize = TRUE
  )
  obj$.__enclos_env__$private$p_gradient = quad_grad
  obj$.__enclos_env__$private$p_hessian = quad_hess

  opt = OptimizerNR$new(obj, x_start = c(3, -2), fallback = "gradient", print_trace = FALSE)
  expect_no_error(opt$optimize(steps = 2L))
  expect_gt(nrow(opt$archive), 0)
})

test_that("OptimizerNR errors when Hessian cannot be inverted and fallback = 'stop'", {
  obj = Objective$new(
    id = "ill",
    fun = function(x) sum((x - 1)^2),
    xdim = 2L,
    minimize = TRUE
  )
  obj$.__enclos_env__$private$p_hessian = function(x) matrix(0, 2, 2)

  opt = OptimizerNR$new(obj, x_start = c(0, 0), fallback = "stop", print_trace = FALSE)
  expect_error(opt$optimize(steps = 1L), "Failed to invert Hessian")
})
