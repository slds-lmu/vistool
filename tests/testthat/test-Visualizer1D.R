test_that("Visualizer1D base class works", {
  # Create a simple 1D visualizer
  x_vals = seq(-2, 2, length.out = 50)
  y_vals = x_vals^2

  vis = Visualizer1D$new(
    fun_x = x_vals,
    fun_y = y_vals,
    title = "Test Function",
    lab_x = "Input",
    lab_y = "Output"
  )

  expect_s3_class(vis, "Visualizer1D")
  expect_equal(vis$fun_x, x_vals)
  expect_equal(vis$fun_y, y_vals)
  expect_equal(vis$title, "Test Function")
  expect_equal(vis$lab_x, "Input")
  expect_equal(vis$lab_y, "Output")

  # Test plotting
  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("Visualizer1D with points works", {
  x_vals = seq(-1, 1, length.out = 20)
  y_vals = x_vals^2

  vis = Visualizer1D$new(
    fun_x = x_vals,
    fun_y = y_vals
  )
  
  # Add points using the modern API
  vis$add_points(data.frame(x = c(-0.5, 0, 0.5), y = c(0.25, 0, 0.25)))

  # Test plotting with points
  p = vis$plot()
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2) # Line + points
})

test_that("Visualizer1D styling works", {
  x_vals = seq(-1, 1, length.out = 20)
  y_vals = x_vals^2

  vis = Visualizer1D$new(
    fun_x = x_vals,
    fun_y = y_vals
  )

  # Test default styling
  expect_equal(vis$line_col, "auto")  # Now uses unified color system
  expect_equal(vis$line_width, 3)
  expect_equal(vis$line_type, "solid")

  # Test custom styling
  vis$line_col = "blue"
  vis$line_width = 2
  vis$line_type = "dashed"

  expect_equal(vis$line_col, "blue")
  expect_equal(vis$line_width, 2)
  expect_equal(vis$line_type, "dashed")

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})
