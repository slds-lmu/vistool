test_that("VisualizerObj basic functionality works", {
  obj = Objective$new(
    id = "test_basic",
    fun = function(x) x^2,
    xdim = 1,
    lower = -2,
    upper = 2
  )

  vis = VisualizerObj$new(obj)
  expect_s3_class(vis, "VisualizerObj")
  expect_s3_class(vis, "Visualizer")

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerObj handles missing bounds with defaults", {
  obj = Objective$new(
    id = "test_no_bounds",
    fun = function(x) x^2,
    xdim = 1
  )
  vis = VisualizerObj$new(obj)
  expect_message(
    VisualizerObj$new(obj),
    regexp = "Objective bounds not available"
  )
  expect_s3_class(vis, "VisualizerObj")
})
