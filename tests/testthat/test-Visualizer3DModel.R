test_that("Visualizer3DModel creation works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  
  vis = Visualizer3DModel$new(task, learner)
  
  expect_s3_class(vis, "Visualizer3DModel")
  expect_s3_class(vis, "Visualizer3D")
  expect_identical(vis$task, task)
  expect_identical(vis$learner, learner)
})

test_that("Visualizer3DModel with custom limits works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  
  vis = Visualizer3DModel$new(
    task, 
    learner,
    x1_limits = c(3, 5),
    x2_limits = c(4, 8),
    n_points = 10L
  )
  
  expect_s3_class(vis, "Visualizer3DModel")
})

test_that("Visualizer3DModel surface initialization works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  
  vis = Visualizer3DModel$new(task, learner, n_points = 5L)
  
  # Initialize surface layer
  vis$init_layer_surface()
  
  # Should have a plot now
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DModel training data works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  
  vis = Visualizer3DModel$new(task, learner, n_points = 5L)
  vis$init_layer_surface()
  
  # Add training data
  vis$add_training_data()
  
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DModel with classification works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("spam")
  task$select(c("you", "credit"))
  learner = lrn("classif.svm", predict_type = "prob")
  
  vis = Visualizer3DModel$new(task, learner, n_points = 5L)
  
  expect_s3_class(vis, "Visualizer3DModel")
  
  # Should be able to initialize
  vis$init_layer_surface()
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DModel input validation works", {
  skip_if_not_installed("mlr3learners")
  
  # Task with wrong number of features
  task_1d = tsk("mtcars")
  task_1d$select("gear")
  learner = lrn("regr.lm")
  
  expect_error(
    Visualizer3DModel$new(task_1d, learner),
    "exactly 2 features"
  )
  
  task_3d = tsk("mtcars")
  task_3d$select(c("gear", "cyl", "hp"))
  
  expect_error(
    Visualizer3DModel$new(task_3d, learner),
    "exactly 2 features"
  )
})

test_that("Visualizer3DModel with different learner types works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  
  # Test with different regression learners
  learners = list(
    lrn("regr.lm"),
    lrn("regr.featureless")
  )
  
  for (learner in learners) {
    vis = Visualizer3DModel$new(task, learner, n_points = 3L)
    expect_s3_class(vis, "Visualizer3DModel")
    
    vis$init_layer_surface()
    p = vis$plot()
    expect_s3_class(p, "plotly")
  }
})

test_that("Visualizer3DModel scene and camera work", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  
  vis = Visualizer3DModel$new(task, learner, n_points = 5L)
  vis$init_layer_surface()
  
  # Set scene
  vis$set_scene(x = 1.1, y = 1.2, z = 1.3)
  
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DModel padding works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  
  vis = Visualizer3DModel$new(task, learner, padding = 0.1, n_points = 5L)
  
  # Should work with padding
  vis$init_layer_surface()
  p = vis$plot()
  expect_s3_class(p, "plotly")
})

test_that("Visualizer3DModel training data customization works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  learner = lrn("regr.lm")
  
  vis = Visualizer3DModel$new(task, learner, n_points = 5L)
  vis$init_layer_surface()
  
  # Add training data with custom parameters
  vis$add_training_data(size = 8, color = "red")
  
  p = vis$plot()
  expect_s3_class(p, "plotly")
})
