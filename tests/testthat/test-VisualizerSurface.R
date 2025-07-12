test_that("VisualizerSurface base class works", {
  skip_if_not_installed("plotly")

  # Create simple grid data
  x1 <- seq(-1, 1, length.out = 5)
  x2 <- seq(-1, 1, length.out = 5)
  grid <- expand.grid(x1 = x1, x2 = x2)
  z_vals <- with(grid, x1^2 + x2^2)
  z_matrix <- matrix(z_vals, nrow = length(x1), ncol = length(x2))

  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix,
    plot_lab = "Test Surface Function"
  )

  expect_s3_class(vis, "VisualizerSurface")
  expect_equal(vis$plot_lab, "Test Surface Function")
  expect_true(is.list(vis$grid))
  expect_true(is.matrix(vis$zmat))
})

test_that("VisualizerSurface surface initialization works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)

  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Initialize surface
  vis$init_layer_surface()

  # Should have a plot now
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface contour initialization works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)

  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Initialize contour
  vis$init_layer_contour()

  # Should have a plot now
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface scene setting works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)

  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  vis$init_layer_surface()

  # Set scene
  vis$set_scene(x = 1.1, y = 1.2, z = 1.3)

  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface input validation works", {
  skip_if_not_installed("plotly")

  # Test with mismatched dimensions
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 4) # Different length
  z_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  expect_s3_class(vis, "VisualizerSurface")

  # Should still work even with different grid lengths
  vis$init_layer_surface()
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface custom contours parameter works", {
  skip_if_not_installed("plotly")
  
  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)
  
  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )
  
  # Test custom contours
  custom_contours <- list(
    x = list(show = TRUE, start = -1, end = 1, size = 0.2, color = "red"),
    y = list(show = TRUE, start = -1, end = 1, size = 0.2, color = "blue")
  )
  
  vis$init_layer_surface(contours = custom_contours)
  
  # Should have a plot now
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface custom contours take precedence over show_contours", {
  skip_if_not_installed("plotly")
  
  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)
  
  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )
  
  # Test that custom contours take precedence
  custom_contours <- list(
    z = list(show = TRUE, color = "green")
  )
  
  vis$init_layer_surface(
    show_contours = TRUE,  # Should be ignored
    contours = custom_contours
  )
  
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface backward compatibility with show_contours maintained", {
  skip_if_not_installed("plotly")
  
  # Create simple data
  x1 <- seq(-1, 1, length.out = 3)
  x2 <- seq(-1, 1, length.out = 3)
  z_matrix <- outer(x1, x2, function(x, y) x^2 + y^2)
  
  vis <- VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )
  
  # Test show_contours still works
  vis$init_layer_surface(show_contours = TRUE)
  
  p <- vis$plot()
  expect_s3_class(p, "plotly")
})
