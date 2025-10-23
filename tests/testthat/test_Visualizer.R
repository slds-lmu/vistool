test_that("Unified save method works for all visualizers", {
  skip_if_not_installed("mlr3learners")
  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis1d = as_visualizer(obj_1d, type = "1d", x1_limits = c(-3, 3))

  temp_file_1d = tempfile(fileext = ".png")
  expect_no_error(vis1d$save(temp_file_1d, width = 8, height = 6))
  expect_true(file.exists(temp_file_1d))
  unlink(temp_file_1d)

  obj_2d = obj("TF_branin")
  vis2d = as_visualizer(obj_2d, type = "2d")

  temp_file_2d = tempfile(fileext = ".png")
  expect_no_error(vis2d$save(temp_file_2d, width = 8, height = 6))
  expect_true(file.exists(temp_file_2d))
  unlink(temp_file_2d)

  task_1d = mlr3::tsk("mtcars")$select("wt")
  learner = mlr3::lrn("regr.lm")
  vis_model = as_visualizer(task_1d, learner = learner, type = "1d")

  temp_file_model = tempfile(fileext = ".png")
  expect_no_error(vis_model$save(temp_file_model, width = 8, height = 6))
  expect_true(file.exists(temp_file_model))
  unlink(temp_file_model)

  obj_2d_surface = obj("TF_branin")
  vis3d = as_visualizer(obj_2d_surface, type = "surface")

  expect_true("save" %in% names(vis3d))
  expect_s3_class(vis3d, "VisualizerSurfaceObj")
  expect_s3_class(vis3d, "Visualizer")
})

test_that("Base Visualizer class methods work correctly", {
  skip_if_not_installed("mlr3learners")
  base_vis = Visualizer$new()
  result = base_vis$plot()
  expect_true(inherits(result, "Visualizer")) # Returns self for method chaining
  expect_identical(result, base_vis) # Should return the same object

  obj_1d = obj("TF_gaussian1", xdim = 1)
  vis1d = as_visualizer(obj_1d, type = "1d", x1_limits = c(-3, 3))
  expect_s3_class(vis1d, "VisualizerObj")
  expect_s3_class(vis1d, "Visualizer")
  expect_true("save" %in% names(vis1d))

  task_1d = mlr3::tsk("mtcars")$select("wt")
  learner = mlr3::lrn("regr.lm")
  vis_model = as_visualizer(task_1d, learner = learner, type = "1d")
  expect_s3_class(vis_model, "VisualizerModel")
  expect_s3_class(vis_model, "Visualizer")
  expect_true("save" %in% names(vis_model))
})
