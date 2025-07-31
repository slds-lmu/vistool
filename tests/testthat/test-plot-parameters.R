test_that("plot() methods accept text_size and theme parameters", {
  # Test Visualizer1D
  x_vals = seq(-2, 2, length.out = 50)
  y_vals = x_vals^2

  vis1d = Visualizer1D$new(
    fun_x = x_vals,
    fun_y = y_vals,
    title = "Test Function",
    lab_x = "Input",
    lab_y = "Output"
  )

  # Test with different text sizes and themes
  p1 = vis1d$plot(text_size = 8, theme = "bw")
  expect_s3_class(p1, "ggplot")

  p2 = vis1d$plot(text_size = 14, theme = "minimal")
  expect_s3_class(p2, "ggplot")

  # Test Visualizer2D
  x1 = seq(-1, 1, length.out = 5)
  x2 = seq(-1, 1, length.out = 5)
  grid = expand.grid(x1 = x1, x2 = x2)
  z_vals = with(grid, x1 + x2)

  vis2d = Visualizer2D$new(
    fun_x1 = grid$x1,
    fun_x2 = grid$x2,
    fun_y = z_vals
  )

  p3 = vis2d$plot(text_size = 10, theme = "classic")
  expect_s3_class(p3, "ggplot")

  # Test VisualizerLossFuns
  loss1 = lss("l2_se")
  loss2 = lss("l1_ae")

  vis_loss = VisualizerLossFuns$new(list(loss1, loss2))
  p4 = vis_loss$plot(text_size = 12, theme = "bw")
  expect_s3_class(p4, "ggplot")
})

test_that("plot() methods validate parameters correctly", {
  # Create a simple 1D visualizer for testing
  x_vals = seq(-1, 1, length.out = 10)
  y_vals = x_vals^2

  vis = Visualizer1D$new(
    fun_x = x_vals,
    fun_y = y_vals
  )

  # Test invalid text_size
  expect_error(vis$plot(text_size = 0), "Assertion on 'text_size' failed")
  expect_error(vis$plot(text_size = -1), "Assertion on 'text_size' failed")

  # Test invalid theme
  expect_error(vis$plot(theme = "invalid_theme"), "Must be element of set")
})

test_that("plot() methods work without parameters", {
  x_vals = seq(-1, 1, length.out = 10)
  y_vals = x_vals^2

  vis1d = Visualizer1D$new(
    fun_x = x_vals,
    fun_y = y_vals
  )

  # Should work without any parameters (using defaults)
  p1 = vis1d$plot()
  expect_s3_class(p1, "ggplot")

  # Test VisualizerLossFuns backward compatibility
  loss = lss("l2_se")
  vis_loss = VisualizerLossFuns$new(list(loss))
  p2 = vis_loss$plot()
  expect_s3_class(p2, "ggplot")
})
