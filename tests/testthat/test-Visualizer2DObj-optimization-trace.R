library(vistool)

test_that("Visualizer2DObj add_optimization_trace works", {
  # Create a simple 2D objective
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2),
    minimize = TRUE
  )

  # Create visualizer
  vis <- Visualizer2DObj$new(obj, n_points = 20L)

  # Create and run optimizer
  opt <- OptimizerGD$new(obj, x_start = c(1.5, 1.0), lr = 0.1, print_trace = FALSE)
  opt$optimize(steps = 5L)

  # Add optimization trace
  expect_silent(vis$add_optimization_trace(opt))

  # Plot should work
  p <- vis$plot()
  expect_s3_class(p, "ggplot")

  # Check that trace was stored
  expect_length(vis$.__enclos_env__$private$.optimization_traces, 1)
})

test_that("Visualizer2DObj add_optimization_trace validates input", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2),
    minimize = TRUE
  )

  vis <- Visualizer2DObj$new(obj, n_points = 20L)
  opt <- OptimizerGD$new(obj, x_start = c(1, 1), lr = 0.1, print_trace = FALSE)

  # Should fail with empty archive
  expect_error(
    vis$add_optimization_trace(opt),
    "No optimization trace.*Did you forget to call.*optimize"
  )

  # Should fail with wrong optimizer type
  expect_error(
    vis$add_optimization_trace("not_an_optimizer"),
    "Assertion on 'optimizer' failed"
  )
})

test_that("Visualizer2DObj multiple optimization traces work", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-3, -3),
    upper = c(3, 3),
    minimize = TRUE
  )

  vis <- Visualizer2DObj$new(obj, n_points = 20L)

  # Create multiple optimizers
  opt1 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(2, 1), lr = 0.1, print_trace = FALSE)
  opt2 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(-1, 2), lr = 0.15, print_trace = FALSE)

  opt1$optimize(steps = 3L)
  opt2$optimize(steps = 4L)

  # Add both traces
  vis$add_optimization_trace(opt1, line_color = "#ff0000", name = "GD1")
  vis$add_optimization_trace(opt2, line_color = "#0000ff", name = "GD2")

  # Should have 2 traces stored
  expect_length(vis$.__enclos_env__$private$.optimization_traces, 2)

  # Plot should work
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer2DObj optimization trace customization works", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2),
    minimize = TRUE
  )

  vis <- Visualizer2DObj$new(obj, n_points = 20L)
  opt <- OptimizerGD$new(obj, x_start = c(1.5, 1.0), lr = 0.1, print_trace = FALSE)
  opt$optimize(steps = 8L)

  # Test various customization options
  vis$add_optimization_trace(
    opt,
    line_color = "#ff6600",
    line_width = 2.0,
    line_type = "dashed",
    marker_size = 4,
    marker_shape = 17,  # triangle
    marker_color = "#cc0000",
    add_marker_at = c(1, 4, 8),
    show_start_end = TRUE,
    alpha = 0.9,
    name = "Custom Trace"
  )

  # Check stored trace parameters
  trace <- vis$.__enclos_env__$private$.optimization_traces[[1]]
  expect_equal(trace$line_color, "#ff6600")
  expect_equal(trace$line_width, 2.0)
  expect_equal(trace$line_type, "dashed")
  expect_equal(trace$marker_size, 4)
  expect_equal(trace$marker_shape, 17)
  expect_equal(trace$marker_color, "#cc0000")
  expect_equal(trace$add_marker_at, c(1, 4, 8))
  expect_true(trace$show_start_end)
  expect_equal(trace$alpha, 0.9)
  expect_equal(trace$name, "Custom Trace")

  # Plot should work
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer2DObj optimization trace point filtering works", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2),
    minimize = TRUE
  )

  vis <- Visualizer2DObj$new(obj, n_points = 20L)
  opt <- OptimizerGD$new(obj, x_start = c(1.5, 1.0), lr = 0.1, print_trace = FALSE)
  opt$optimize(steps = 20L)

  # Test point filtering
  vis$add_optimization_trace(opt, npoints = 5, npmax = 10)

  trace <- vis$.__enclos_env__$private$.optimization_traces[[1]]
  expect_true(nrow(trace$data) <= 10)  # Should be limited by npmax
  expect_true(nrow(trace$data) <= 5)   # Should be limited by npoints

  # Plot should work
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer2DObj auto-color generation works", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2),
    minimize = TRUE
  )

  vis <- Visualizer2DObj$new(obj, n_points = 20L)

  # Create multiple optimizers without specifying colors
  opts <- list()
  for (i in 1:3) {
    opt <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(runif(1, -1, 1), runif(1, -1, 1)), lr = 0.1, print_trace = FALSE)
    opt$optimize(steps = 3L)
    opts[[i]] <- opt
  }

  # Add traces without specifying colors
  for (opt in opts) {
    vis$add_optimization_trace(opt)
  }

  # Should have 3 traces with different colors
  expect_length(vis$.__enclos_env__$private$.optimization_traces, 3)
  
  colors <- sapply(vis$.__enclos_env__$private$.optimization_traces, function(trace) trace$line_color)
  expect_length(unique(colors), 3)  # All should be different

  # Plot should work
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer2DObj method chaining works", {
  obj <- Objective$new(
    id = "test_2d",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-2, -2),
    upper = c(2, 2),
    minimize = TRUE
  )

  vis <- Visualizer2DObj$new(obj, n_points = 20L)
  opt1 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(1, 1), lr = 0.1, print_trace = FALSE)
  opt2 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(-1, -1), lr = 0.1, print_trace = FALSE)

  opt1$optimize(steps = 3L)
  opt2$optimize(steps = 3L)

  # Method chaining should work
  result <- vis$add_optimization_trace(opt1)$add_optimization_trace(opt2)
  expect_identical(result, vis)
  expect_length(vis$.__enclos_env__$private$.optimization_traces, 2)
})
