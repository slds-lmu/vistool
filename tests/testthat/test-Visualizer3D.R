test_that("Visualizer3D base class works", {
  skip_if_not_installed("plotly")

  # Create simple grid data
  x1 <- seq(-1, 1, length.out = 5)
  x2 <- seq(-1, 1, length.out = 5)
  grid <- expand.grid(x1 = x1, x2 = x2)
  z_vals <- with(grid, x1^2 + x2^2)
  z_matrix <- matrix(z_vals, nrow = length(x1), ncol = length(x2))

  vis <- Visualizer3D$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix,
    plot_lab = "Test 3D Function"
  )

  expect_s3_class(vis, "Visualizer3D")
  expect_equal(vis$plot_lab, "Test 3D Function")
  expect_true(is.list(vis$grid))
  expect_true(is.matrix(vis$zmat))
})

test_that("Visualizer3D surface initialization works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)

  vis <- Visualizer3D$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Initialize surface
  vis$init_layer_surface()

  # Should have a plot now
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3D contour initialization works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)

  vis <- Visualizer3D$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Initialize contour
  vis$init_layer_contour()

  # Should have a plot now
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3D scene setting works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)

  vis <- Visualizer3D$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  vis$init_layer_surface()

  # Set scene
  vis$set_scene(x = 1.1, y = 1.2, z = 1.3)

  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3D input validation works", {
  skip_if_not_installed("plotly")

  # Test with mismatched dimensions
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 4) # Different length
  z_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  vis <- Visualizer3D$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  expect_s3_class(vis, "Visualizer3D")

  # Should still work even with different grid lengths
  vis$init_layer_surface()
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})
