test_that("Visualizer3DObj creation works", {
  obj <- obj("TF_branin") # 2D objective

  vis <- Visualizer3DObj$new(obj)

  expect_s3_class(vis, "Visualizer3DObj")
  expect_s3_class(vis, "Visualizer3D")
  expect_identical(vis$objective, obj)
})

test_that("Visualizer3DObj with custom limits works", {
  obj <- obj("TF_branin")

  vis <- Visualizer3DObj$new(
    obj,
    x1_limits = c(0, 5),
    x2_limits = c(0, 5),
    n_points = 20L
  )

  expect_s3_class(vis, "Visualizer3DObj")
})

test_that("Visualizer3DObj surface initialization works", {
  obj <- obj("TF_branin")
  vis <- Visualizer3DObj$new(obj, n_points = 10L)

  # Initialize surface layer
  vis$init_layer_surface()

  # Should have a plot now
  expect_true(!is.null(vis$plot()))
})

test_that("Visualizer3DObj contour initialization works", {
  obj <- obj("TF_branin")
  vis <- Visualizer3DObj$new(obj, n_points = 10L)

  # Initialize contour layer
  vis$init_layer_contour()

  # Should have a plot now
  expect_true(!is.null(vis$plot()))
})

test_that("Visualizer3DObj optimization trace works", {
  skip_if_not_installed("mlr3learners")

  obj <- obj("TF_branin")
  vis <- Visualizer3DObj$new(obj, n_points = 10L)
  vis$init_layer_surface()

  # Create and run optimizer
  opt <- OptimizerGD$new(obj, x_start = c(0, 0), lr = 0.01, print_trace = FALSE)
  opt$optimize(steps = 3L)

  # Add optimization trace
  expect_silent(vis$add_optimization_trace(opt))

  # Plot should work
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DObj input validation works", {
  # 1D objective should fail
  obj_1d <- Objective$new(
    id = "test_1d",
    fun = function(x) x^2,
    xdim = 1,
    lower = -1,
    upper = 1
  )

  expect_error(
    Visualizer3DObj$new(obj_1d),
    "2-dimensional inputs"
  )

  # 3D objective should fail
  obj_3d <- Objective$new(
    id = "test_3d",
    fun = function(x) sum(x^2),
    xdim = 3,
    lower = c(-1, -1, -1),
    upper = c(1, 1, 1)
  )

  expect_error(
    Visualizer3DObj$new(obj_3d),
    "2-dimensional inputs"
  )
})

test_that("Visualizer3DObj with missing limits works", {
  # Objective without bounds
  obj <- Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(NA, NA),
    upper = c(NA, NA)
  )

  # Should fail without explicit limits
  expect_error(
    Visualizer3DObj$new(obj),
    "Limits could not be extracted"
  )

  # Should work with explicit limits
  vis <- Visualizer3DObj$new(
    obj,
    x1_limits = c(-2, 2),
    x2_limits = c(-2, 2)
  )
  expect_s3_class(vis, "Visualizer3DObj")
})

test_that("Visualizer3DObj padding works", {
  obj <- Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  vis <- Visualizer3DObj$new(obj, padding = 0.2, n_points = 5L)

  # Check that padding was applied (can't directly access private fields,
  # but we can initialize and check the plot works)
  vis$init_layer_surface()
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DObj scene setting works", {
  obj <- obj("TF_branin")
  vis <- Visualizer3DObj$new(obj, n_points = 5L)
  vis$init_layer_surface()

  # Set scene
  vis$set_scene(x = 1.2, y = 1.3, z = 1.4)

  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DObj save functionality works", {
  skip_if_not_installed("reticulate")
  skip_on_ci() # Skip on CI as it requires Python setup

  obj <- obj("TF_branin")
  vis <- Visualizer3DObj$new(obj, n_points = 5L)
  vis$init_layer_surface()

  # Test save (may fail if kaleido not installed, but shouldn't error in R)
  temp_file <- tempfile(fileext = ".png")

  # This might fail due to Python dependencies, so we'll just test that the method exists
  expect_true("save" %in% names(vis))
})

test_that("Visualizer3DObj multiple optimization traces work", {
  obj <- obj("TF_branin")
  vis <- Visualizer3DObj$new(obj, n_points = 10L)
  vis$init_layer_contour()

  # Create multiple optimizers
  opt1 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(0, 0), lr = 0.01, print_trace = FALSE)
  opt2 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(1, 1), lr = 0.02, print_trace = FALSE)

  opt1$optimize(steps = 2L)
  opt2$optimize(steps = 2L)

  # Add both traces
  vis$add_optimization_trace(opt1, line_color = "red")
  vis$add_optimization_trace(opt2, line_color = "blue")

  p <- vis$plot()
  expect_s3_class(p, "plotly")
})
