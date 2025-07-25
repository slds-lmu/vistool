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

  # Add training points using the new unified system
  training_points <- data.frame(
    x = c(-0.5, 0, 0.5),
    y = c(0.5, 0, -0.5)
  )
  vis$add_points(training_points, color = "red")

  # Check that points were added
  expect_length(vis$.__enclos_env__$private$.points_to_add, 1)
  point_set <- vis$.__enclos_env__$private$.points_to_add[[1]]
  expect_equal(nrow(point_set$points), 3)
  expect_equal(point_set$color, "red")

  p <- vis$plot()
  expect_s3_class(p, "ggplot")
  # Should have 3 layers: raster + contour (white) + points (from add_points)
  expect_length(p$layers, 3)
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

  # add_optimization_trace should no longer be available on base Visualizer2D
  expect_null(vis$add_optimization_trace)
  
  # Test that basic plotting works
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})
