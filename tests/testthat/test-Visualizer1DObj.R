library(vistool)

test_that("Visualizer1DObj creation works", {
  obj = Objective$new(
    id = "test_1d",
    fun = function(x) x^2,
    xdim = 1,
    lower = -2,
    upper = 2
  )

  vis = Visualizer1DObj$new(obj)

  expect_s3_class(vis, "Visualizer1DObj")
  expect_s3_class(vis, "Visualizer1D")
  expect_identical(vis$objective, obj)
  expect_true(length(vis$fun_x) > 0)
  expect_true(length(vis$fun_y) > 0)
})

test_that("Visualizer1DObj plotting works", {
  obj = Objective$new(
    id = "sine",
    fun = function(x) sin(x),
    xdim = 1,
    lower = 0,
    upper = 2 * pi
  )

  vis = Visualizer1DObj$new(obj, n_points = 50L)

  # Test plotting
  p = vis$plot()
  expect_s3_class(p, "ggplot")

  # Check that we have the right number of points
  expect_equal(length(vis$fun_x), 50)
  expect_equal(length(vis$fun_y), 50)
})

test_that("Visualizer1DObj with custom limits works", {
  obj = Objective$new(
    id = "test",
    fun = function(x) x^3,
    xdim = 1,
    lower = -10, # Wide bounds
    upper = 10
  )

  # Custom narrower limits
  vis = Visualizer1DObj$new(obj, xlim = c(-2, 2))

  expect_true(min(vis$fun_x) >= -2)
  expect_true(max(vis$fun_x) <= 2)
})

test_that("Visualizer1DObj optimization trace works", {
  obj = Objective$new(
    id = "quadratic",
    fun = function(x) x^2,
    xdim = 1,
    lower = -3,
    upper = 3,
    minimize = TRUE
  )

  vis = Visualizer1DObj$new(obj)

  # Create and run optimizer
  opt = OptimizerGD$new(obj, x_start = 2, lr = 0.1, print_trace = FALSE)
  opt$optimize(steps = 5L)

  # Add optimization trace
  expect_silent(vis$add_optimization_trace(opt))

  # Plot should work
  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer1DObj input validation works", {
  # 2D objective should fail
  obj_2d = Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  expect_error(
    Visualizer1DObj$new(obj_2d),
    "1-dimensional inputs"
  )
})

test_that("Visualizer1DObj with missing limits works", {
  # Objective without bounds
  obj = Objective$new(
    id = "test",
    fun = function(x) x^2,
    xdim = 1,
    lower = NA,
    upper = NA
  )

  # Should fail without explicit limits
  expect_error(
    Visualizer1DObj$new(obj),
    "Limits could not be extracted"
  )

  # Should work with explicit limits
  vis = Visualizer1DObj$new(obj, xlim = c(-5, 5))
  expect_s3_class(vis, "Visualizer1DObj")
})

test_that("Visualizer1DObj evaluation points are correct", {
  obj = Objective$new(
    id = "linear",
    fun = function(x) 2 * x + 1, # Simple linear function
    xdim = 1,
    lower = 0,
    upper = 10
  )

  vis = Visualizer1DObj$new(obj, n_points = 11L)

  # Check that function values are correct
  expected_x = seq(0, 10, length.out = 11)
  expected_y = 2 * expected_x + 1

  expect_equal(vis$fun_x, expected_x)
  expect_equal(vis$fun_y, expected_y)
})

test_that("Visualizer1DObj bounds are respected", {
  obj = Objective$new(
    id = "test",
    fun = function(x) x^2,
    xdim = 1,
    lower = -1,
    upper = 1
  )

  # Visualizer1DObj doesn't support padding parameter
  vis = Visualizer1DObj$new(obj, n_points = 21L)

  # Just check that it works without padding
  expect_true(min(vis$fun_x) >= -1)
  expect_true(max(vis$fun_x) <= 1)
})
