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

  # Test that add_optimization_trace now requires an optimizer argument
  expect_error(
    vis$add_optimization_trace(),
    "argument \"optimizer\" is missing"
  )
  
  # set_layout should still be a no-op for compatibility
  expect_silent(vis$set_layout())

  # Test with a valid optimizer
  obj <- Objective$new(
    id = "test",
    fun = function(x) sum(x^2),
    xdim = 2,
    lower = c(-1, -1),
    upper = c(1, 1),
    minimize = TRUE
  )
  opt <- OptimizerGD$new(obj, x_start = c(0.5, 0.5), lr = 0.1, print_trace = FALSE)
  opt$optimize(steps = 2L)
  
  # Should return self for chaining
  result <- vis$add_optimization_trace(opt)
  expect_identical(result, vis)
})
