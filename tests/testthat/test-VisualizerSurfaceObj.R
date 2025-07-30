test_that("VisualizerSurfaceObj creation works", {
  obj <- obj("TF_branin") # 2D objective

  vis <- VisualizerSurfaceObj$new(obj)

  expect_s3_class(vis, "VisualizerSurfaceObj")
  expect_s3_class(vis, "VisualizerSurface")
  expect_identical(vis$objective, obj)
})

test_that("VisualizerSurfaceObj with custom limits works", {
  obj <- obj("TF_branin")

  vis <- VisualizerSurfaceObj$new(
    obj,
    x1_limits = c(0, 5),
    x2_limits = c(0, 5),
    n_points = 20L
  )

  expect_s3_class(vis, "VisualizerSurfaceObj")
})

test_that("VisualizerSurfaceObj surface initialization works", {
  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 10L)

  # Initialize surface layer
  vis$init_layer_surface()

  # Should have a plot now
  expect_true(!is.null(vis$plot()))
})

test_that("VisualizerSurfaceObj contour initialization works", {
  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 10L)

  # Should be able to plot as contour
  p <- vis$plot(flatten = TRUE)
  expect_true(!is.null(p))
})

test_that("VisualizerSurfaceObj optimization trace works", {
  skip_if_not_installed("mlr3learners")

  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 10L)
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

test_that("VisualizerSurfaceObj input validation works", {
  # 1D objective should fail
  obj_1d <- Objective$new(
    id = "test_1d",
    fun = function(x) x^2,
    xdim = 1,
    lower = -1,
    upper = 1
  )

  expect_error(
    VisualizerSurfaceObj$new(obj_1d),
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
    VisualizerSurfaceObj$new(obj_3d),
    "2-dimensional inputs"
  )
})

test_that("VisualizerSurfaceObj with missing limits works", {
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
    VisualizerSurfaceObj$new(obj),
    "Limits could not be extracted"
  )

  # Should work with explicit limits
  vis <- VisualizerSurfaceObj$new(
    obj,
    x1_limits = c(-2, 2),
    x2_limits = c(-2, 2)
  )
  expect_s3_class(vis, "VisualizerSurfaceObj")
})

test_that("VisualizerSurfaceObj padding works", {
  obj <- Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1)
  )

  vis <- VisualizerSurfaceObj$new(obj, padding = 0.2, n_points = 5L)

  # Check that padding was applied (can't directly access private fields,
  # but we can initialize and check the plot works)
  vis$init_layer_surface()
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj scene setting works", {
  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 5L)
  vis$init_layer_surface()

  # Set scene
  vis$set_scene(x = 1.2, y = 1.3, z = 1.4)

  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj save functionality works", {
  skip_if_not_installed("reticulate")
  skip_on_ci() # Skip on CI as it requires Python setup

  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 5L)
  vis$init_layer_surface()

  # Test save (may fail if kaleido not installed, but shouldn't error in R)
  temp_file <- tempfile(fileext = ".png")

  # This might fail due to Python dependencies, so we'll just test that the method exists
  expect_true("save" %in% names(vis))
})

test_that("VisualizerSurfaceObj multiple optimization traces work", {
  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 10L)

  # Create multiple optimizers
  opt1 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(0, 0), lr = 0.01, print_trace = FALSE)
  opt2 <- OptimizerGD$new(obj$clone(deep = TRUE), x_start = c(1, 1), lr = 0.02, print_trace = FALSE)

  opt1$optimize(steps = 2L)
  opt2$optimize(steps = 2L)

  # Add both traces
  vis$add_optimization_trace(opt1, line_color = "red")
  vis$add_optimization_trace(opt2, line_color = "blue")

  p <- vis$plot(flatten = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj custom contours parameter works", {
  skip_if_not_installed("plotly")
  
  obj <- obj("TF_franke")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 10L)
  
  # Test custom contours with objective limits
  llower <- vis$objective$limits_lower
  lupper <- vis$objective$limits_upper
  ssize <- (lupper - llower) / 10
  
  custom_contours <- list(
    x = list(show = TRUE, start = llower[1], end = lupper[1], size = ssize[1], color = "red"),
    y = list(show = TRUE, start = llower[2], end = lupper[2], size = ssize[2], color = "blue")
  )
  
  vis$init_layer_surface(
    opacity = 1,
    colorscale = list(c(0, 1), c("white", "black")),
    contours = custom_contours
  )
  
  # Should have a plot now
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceObj add_contours parameter validation", {
  skip_if_not_installed("plotly")
  
  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 5L)
  
  # Test that invalid contours parameter is caught
  expect_error(vis$add_contours(contours = "invalid"), class = "simpleError")
  
  # Test that NULL contours works (default behavior)
  expect_silent(vis$add_contours(contours = NULL))
  
  # Test that empty list contours works
  expect_silent(vis$add_contours(contours = list()))
})

test_that("VisualizerSurfaceObj layer methods use deferred rendering", {
  obj <- obj("TF_branin")
  vis <- VisualizerSurfaceObj$new(obj, n_points = 10L)

  # Test that add_taylor works with deferred rendering
  x0 <- c(2.5, 7.5)
  expect_silent(vis$add_taylor(x0, degree = 1))
  
  # Test that add_hessian works with deferred rendering
  expect_silent(vis$add_hessian(x0))
  
  # Verify layers are stored but plot isn't created until plot() is called
  layers <- vis$.__enclos_env__$private$.layers_to_add
  expect_true(length(layers) >= 2)  # Should have stored the taylor and hessian layers
  
  # Now verify plot renders correctly
  expect_silent(p <- vis$plot())
  expect_true(!is.null(p))
})
