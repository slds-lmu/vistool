test_that("VisualizerSurface base class works", {
  skip_if_not_installed("plotly")

  # Create simple grid data
  x1 = seq(-1, 1, length.out = 5)
  x2 = seq(-1, 1, length.out = 5)
  grid = expand.grid(x1 = x1, x2 = x2)
  z_vals = with(grid, x1^2 + x2^2)
  z_matrix = matrix(z_vals, nrow = length(x1), ncol = length(x2))

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix,
    plot_lab = "Test Surface Function"
  )

  expect_s3_class(vis, "VisualizerSurface")
  expect_equal(vis$plot_lab, "Test Surface Function")
  expect_true(is.list(vis$grid))
  expect_true(is.matrix(vis$zmat))
})

test_that("VisualizerSurface default plot() works directly", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Should work directly without initialization
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface view_as_contour() works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Should be able to plot as contour
  p = vis$plot(flatten = TRUE)
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface explicit surface initialization still works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Explicit initialization should still work
  vis$init_layer_surface()

  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface scene setting works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Set scene (should auto-initialize surface if needed)
  vis$set_scene(x = 1.1, y = 1.2, z = 1.3)

  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface input validation works", {
  skip_if_not_installed("plotly")

  # Test with mismatched dimensions
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 4) # Different length
  z_matrix = matrix(1:12, nrow = 3, ncol = 4)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  expect_s3_class(vis, "VisualizerSurface")

  # Should still work even with different grid lengths
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface custom contours parameter works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Test custom contours
  custom_contours = list(
    x = list(show = TRUE, start = -1, end = 1, size = 0.2, color = "red"),
    y = list(show = TRUE, start = -1, end = 1, size = 0.2, color = "blue")
  )

  vis$init_layer_surface(contours = custom_contours)

  # Should have a plot now
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface custom contours work with add_contours", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Test that custom contours work
  custom_contours = list(
    z = list(show = TRUE, color = "green")
  )

  vis$add_contours(contours = custom_contours)

  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface add_contours method works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Test add_contours method works
  vis$add_contours()

  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface explicit init_layer_surface works", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Explicit initialization should still work
  vis$init_layer_surface()
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurface new workflow - direct plotting like ggplot2", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  # New workflow: direct plotting without init
  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Should work immediately like ggplot2
  p1 = vis$plot()
  expect_s3_class(p1, "plotly")

  # Should be able to plot as contour
  p2 = vis$plot(flatten = TRUE)
  expect_s3_class(p2, "plotly")

  # Test with initial parameters
  vis2 = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Add contours using new method
  vis2$add_contours()

  p3 = vis2$plot()
  expect_s3_class(p3, "plotly")
})

test_that("VisualizerSurface add_contours args are properly applied", {
  skip_if_not_installed("plotly")

  # Create simple data
  x1 = seq(-1, 1, length.out = 3)
  x2 = seq(-1, 1, length.out = 3)
  z_matrix = outer(x1, x2, function(x, y) x^2 + y^2)

  vis = VisualizerSurface$new(
    grid = list(x1 = x1, x2 = x2),
    zmat = z_matrix
  )

  # Test that additional args are properly applied to the surface trace
  vis$add_contours(
    contours = list(z = list(show = TRUE)),
    showscale = TRUE,  # Additional arg that should be applied to trace
    hoverinfo = "skip"  # Another additional arg
  )

  p = vis$plot()
  expect_s3_class(p, "plotly")
  
  # Check that the trace has the additional arguments applied
  # plotly stores trace data in p$x$attrs, find the surface trace
  surface_attrs = NULL
  for (attrs in p$x$attrs) {
    if (!is.null(attrs$type) && attrs$type == "surface") {
      surface_attrs = attrs
      break
    }
  }
  expect_true(!is.null(surface_attrs))
  expect_true(surface_attrs$showscale)
  expect_equal(surface_attrs$hoverinfo, "skip")
})
