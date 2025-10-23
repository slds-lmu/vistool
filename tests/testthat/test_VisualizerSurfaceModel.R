test_that("VisualizerSurfaceModel creation works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis = VisualizerSurfaceModel$new(task, learner)

  expect_s3_class(vis, "VisualizerSurfaceModel")
  expect_s3_class(vis, "VisualizerSurface")
  expect_identical(vis$task, task)
  expect_identical(vis$learner, learner)
})

test_that("VisualizerSurfaceModel with custom limits works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis = VisualizerSurfaceModel$new(
    task,
    learner,
    x1_limits = c(3, 5),
    x2_limits = c(4, 8),
    n_points = 10L
  )

  expect_s3_class(vis, "VisualizerSurfaceModel")
})

test_that("VisualizerSurfaceModel default plot() works directly", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis = VisualizerSurfaceModel$new(task, learner, n_points = 5L)

  # Should work directly without initialization
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceModel training data works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis = VisualizerSurfaceModel$new(task, learner, n_points = 5L)
  vis$init_layer_surface()

  # Add training data
  vis$add_training_data()

  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceModel with classification works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("spam")
  task$select(c("you", "credit"))
  learner = lrn("classif.svm", predict_type = "prob")

  vis = VisualizerSurfaceModel$new(task, learner, n_points = 5L)

  expect_s3_class(vis, "VisualizerSurfaceModel")

  # Should be able to initialize
  vis$init_layer_surface()
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceModel input validation works", {
  skip_if_not_installed("mlr3learners")

  # Task with wrong number of features
  task_1d = tsk("mtcars")
  task_1d$select("gear")
  learner = lrn("regr.lm")

  expect_error(
    VisualizerSurfaceModel$new(task_1d, learner),
    "exactly 2 features"
  )

  task_3d = tsk("mtcars")
  task_3d$select(c("gear", "cyl", "hp"))

  expect_error(
    VisualizerSurfaceModel$new(task_3d, learner),
    "exactly 2 features"
  )
})

test_that("VisualizerSurfaceModel with different learner types works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  # Test with different regression learners
  learners = list(
    lrn("regr.lm"),
    lrn("regr.featureless")
  )

  for (learner in learners) {
    vis = VisualizerSurfaceModel$new(task, learner, n_points = 3L)
    expect_s3_class(vis, "VisualizerSurfaceModel")

    vis$init_layer_surface()
    p = vis$plot()
    expect_s3_class(p, "plotly")
  }
})

test_that("VisualizerSurfaceModel scene and camera work", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis = VisualizerSurfaceModel$new(task, learner, n_points = 5L)
  vis$init_layer_surface()

  # Set scene
  vis$set_scene(x = 1.1, y = 1.2, z = 1.3)

  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceModel padding works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis = VisualizerSurfaceModel$new(task, learner, padding = 0.1, n_points = 5L)

  # Should work with padding
  vis$init_layer_surface()
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("VisualizerSurfaceModel training data customization works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")

  vis = VisualizerSurfaceModel$new(task, learner, n_points = 5L)
  vis$init_layer_surface()

  # Add training data with custom parameters
  vis$add_training_data(size = 8, color = "red")

  p = vis$plot()
  expect_s3_class(p, "plotly")
})
