test_that("plot() methods accept theme override with text_size and theme", {
  # Test VisualizerObj (1D) - using gaussian1 which supports any dimension
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis1d = as_visualizer(obj_1d, type = "1d", x1_limits = c(-3, 3))

  # Test with different text sizes and themes
  p1 = vis1d$plot(theme = list(text_size = 8, theme = "bw"))
  expect_s3_class(p1, "ggplot")

  p2 = vis1d$plot(theme = list(text_size = 14, theme = "minimal"))
  expect_s3_class(p2, "ggplot")

  # Test VisualizerObj (2D)
  obj_2d = obj("TF_branin")
  vis2d = as_visualizer(obj_2d, type = "2d")

  p3 = vis2d$plot(theme = list(text_size = 10, theme = "classic"))
  expect_s3_class(p3, "ggplot")

  # Test VisualizerModel (1D)
  task_1d = mlr3::tsk("mtcars")$select("wt")
  learner = mlr3::lrn("regr.lm")
  vis_model_1d = as_visualizer(task_1d, learner = learner, type = "1d")
  p4 = vis_model_1d$plot(theme = list(text_size = 12, theme = "bw"))
  expect_s3_class(p4, "ggplot")

  # Test VisualizerLossFuns
  loss1 = lss("l2_se")
  loss2 = lss("l1_ae")

  vis_loss = VisualizerLossFuns$new(list(loss1, loss2))
  p5 = vis_loss$plot(theme = list(text_size = 12, theme = "bw"))
  expect_s3_class(p5, "ggplot")
})

test_that("plot() methods validate parameters correctly", {
  # Create a simple visualizer for testing
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis = as_visualizer(obj_1d, type = "1d", x1_limits = c(-3, 3))

  # Test invalid text_size
  expect_error(vis$plot(theme = list(text_size = 0)))
  expect_error(vis$plot(theme = list(text_size = -1)))

  # Test invalid theme
  expect_error(vis$plot(theme = list(theme = "invalid_theme")), "Must be element of set")
})

test_that("plot() methods work without parameters", {
  # Test VisualizerObj
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis1d = as_visualizer(obj_1d, type = "1d", x1_limits = c(-3, 3))

  # Should work without any parameters (using defaults)
  p1 = vis1d$plot()
  expect_s3_class(p1, "ggplot")

  # Test VisualizerModel
  task_1d = mlr3::tsk("mtcars")$select("wt")
  learner = mlr3::lrn("regr.lm")
  vis_model = as_visualizer(task_1d, learner = learner, type = "1d")
  p2 = vis_model$plot()
  expect_s3_class(p2, "ggplot")

  # Test VisualizerLossFuns
  loss = lss("l2_se")
  vis_loss = VisualizerLossFuns$new(list(loss))
  p3 = vis_loss$plot()
  expect_s3_class(p3, "ggplot")
})
