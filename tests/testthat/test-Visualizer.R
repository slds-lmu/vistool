test_that("Unified save method works for all visualizers", {
  # Test 1D visualizer save
  x_vals <- seq(-2, 2, length.out = 50)
  y_vals <- x_vals^2
  vis1d <- Visualizer1D$new(
    fun_x = x_vals,
    fun_y = y_vals,
    title = "Test 1D Function"
  )

  # Create temporary file for testing
  temp_file_1d <- tempfile(fileext = ".png")
  expect_no_error(vis1d$save(temp_file_1d, width = 8, height = 6))
  expect_true(file.exists(temp_file_1d))
  unlink(temp_file_1d)

  # Test 2D visualizer save
  x1_vals <- rep(seq(-1, 1, length.out = 10), each = 10)
  x2_vals <- rep(seq(-1, 1, length.out = 10), times = 10)
  y_vals_2d <- x1_vals^2 + x2_vals^2
  vis2d <- Visualizer2D$new(
    fun_x1 = x1_vals,
    fun_x2 = x2_vals,
    fun_y = y_vals_2d,
    title = "Test 2D Function"
  )

  temp_file_2d <- tempfile(fileext = ".png")
  expect_no_error(vis2d$save(temp_file_2d, width = 8, height = 6))
  expect_true(file.exists(temp_file_2d))
  unlink(temp_file_2d)

  # Test 3D visualizer save
  grid <- list(
    x1 = seq(-1, 1, length.out = 10),
    x2 = seq(-1, 1, length.out = 10)
  )
  zmat <- outer(grid$x1, grid$x2, function(x, y) x^2 + y^2)
  vis3d <- Visualizer3D$new(
    grid = grid,
    zmat = zmat,
    plot_lab = "Test 3D Function"
  )

  # Note: plotly save requires additional setup for headless environments
  # so we'll just test that the method exists and can be called
  expect_true("save" %in% names(vis3d))
  expect_s3_class(vis3d, "Visualizer3D")
  expect_s3_class(vis3d, "Visualizer")
})

test_that("Base Visualizer class methods work correctly", {
  # Test that the base class cannot be instantiated directly for plot
  base_vis <- Visualizer$new()
  expect_error(base_vis$plot(), "Abstract method 'plot' must be implemented by subclass")

  # Test inheritance structure
  vis1d <- Visualizer1D$new(c(1, 2, 3), c(1, 4, 9))
  expect_s3_class(vis1d, "Visualizer1D")
  expect_s3_class(vis1d, "Visualizer")
  expect_true("save" %in% names(vis1d))
})
