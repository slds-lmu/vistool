test_that("Visualizer2D base class works", {
  # Create grid data
  x1 <- seq(-2, 2, length.out = 10)
  x2 <- seq(-2, 2, length.out = 10)
  grid <- expand.grid(x1 = x1, x2 = x2)
  z_vals <- with(grid, x1^2 + x2^2)
  z_matrix <- matrix(z_vals, nrow = length(x1), ncol = length(x2))

  vis <- Visualizer2D$new(
    fun_x1 = grid$x1,
    fun_x2 = grid$x2,
    fun_y = z_vals,
    title = "Test 2D Function"
  )

  expect_s3_class(vis, "Visualizer2D")
  expect_equal(vis$title, "Test 2D Function")
  expect_equal(length(vis$fun_x1), nrow(grid))
  expect_equal(length(vis$fun_x2), nrow(grid))
  expect_equal(length(vis$fun_y), nrow(grid))

  # Test plotting
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer2D with training points works", {
  # Create simple grid
  x1 <- seq(-1, 1, length.out = 5)
  x2 <- seq(-1, 1, length.out = 5)
  grid <- expand.grid(x1 = x1, x2 = x2)
  z_vals <- with(grid, x1^2 + x2^2)

  vis <- Visualizer2D$new(
    fun_x1 = grid$x1,
    fun_x2 = grid$x2,
    fun_y = z_vals
  )

  # Add training points
  vis$points_x1 <- c(-0.5, 0, 0.5)
  vis$points_x2 <- c(0.5, 0, -0.5)
  vis$points_y <- c(1, 2, 3)

  expect_length(vis$points_x1, 3)
  expect_length(vis$points_x2, 3)
  expect_length(vis$points_y, 3)

  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer2D basic functionality works", {
  # Create simple grid
  x1 <- seq(-1, 1, length.out = 5)
  x2 <- seq(-1, 1, length.out = 5)
  grid <- expand.grid(x1 = x1, x2 = x2)
  z_vals <- with(grid, x1 + x2) # Simple linear function

  vis <- Visualizer2D$new(
    fun_x1 = grid$x1,
    fun_x2 = grid$x2,
    fun_y = z_vals
  )

  # Test that it plots correctly
  p <- vis$plot()
  expect_s3_class(p, "ggplot")

  # Test that it has the right layers (contour_filled)
  expect_true(length(p$layers) >= 1)
})

test_that("Visualizer2D optimization trace and compatibility methods work", {
  # Create simple visualizer
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  grid <- expand.grid(x1 = x1, x2 = x2)
  z_vals <- with(grid, x1^2 + x2^2)

  vis <- Visualizer2D$new(
    fun_x1 = grid$x1,
    fun_x2 = grid$x2,
    fun_y = z_vals
  )

  # set_layout should warn that it's only available for surface visualizers
  expect_warning(
    vis$set_layout(),
    "set_layout\\(\\) is only available for VisualizerSurface"
  )

  # add_optimization_trace should no longer be available on base Visualizer2D
  expect_null(vis$add_optimization_trace)
})
