test_that("2D Objective works", {
  skip_if_not_installed("mlr3learners")

  obj_branin <- obj("TF_branin")

  vis <- as_visualizer(obj_branin, type = "2d")

  expect_s3_class(vis, "Visualizer2DObj")
  expect_s3_class(vis, "Visualizer2D")
  expect_identical(vis$objective, obj_branin)
  
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("2D Objective creation with custom parameters works", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) x[1]^2 + x[2]^2,
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2)
  )

  vis <- Visualizer2DObj$new(obj, n_points = 50L, padding = 0.1)

  expect_s3_class(vis, "Visualizer2DObj")
  expect_identical(vis$objective, obj)
  expect_true(length(vis$fun_x1) > 0)
  expect_true(length(vis$fun_x2) > 0)
  expect_true(length(vis$fun_y) > 0)
  
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("2D Objective fails with wrong dimension", {
  obj_1d <- Objective$new(
    id = "test_1d",
    fun = function(x) x^2,
    xdim = 1,
    lower = -2,
    upper = 2
  )

  expect_error(
    Visualizer2DObj$new(obj_1d),
    "requires 2-dimensional inputs"
  )
})

test_that("2D Objective with optimization trace works", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) x[1]^2 + x[2]^2,
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2)
  )

  vis <- Visualizer2DObj$new(obj)
  
  # Create a simple optimizer with proper x_start
  opt <- OptimizerGD$new(objective = obj, x_start = c(1, 1))
  opt$optimize(steps = 5L)

  expect_true(nrow(opt$archive) > 0)
  
  # Add optimization trace
  vis$add_optimization_trace(opt)
  
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
  
  # Should have additional layers for the optimization trace
  expect_true(length(p$layers) >= 3) # contour + contour lines + optimization trace
})

test_that("2D Objective add_layer methods show appropriate warnings", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) x[1]^2 + x[2]^2,
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2)
  )

  vis <- Visualizer2DObj$new(obj)
  
  # Test warning methods
  expect_warning(vis$add_layer_taylor(c(0, 0)), "only available for VisualizerSurface")
  expect_warning(vis$add_layer_hessian(c(0, 0)), "only available for VisualizerSurface")
})
