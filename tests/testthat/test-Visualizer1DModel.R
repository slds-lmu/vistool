test_that("1D Model with regression task works", {
  skip_if_not_installed("mlr3learners")

  task <- tsk("mtcars")
  task$select("gear")

  learner <- lrn("regr.svm")

  vis <- Visualizer1DModel$new(task, learner)

  expect_s3_class(vis, "Visualizer1DModel")
  expect_s3_class(vis, "Visualizer1D")
  expect_identical(vis$task, task)
  expect_identical(vis$learner, learner)
  
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("1D Model with regression task and training data works", {
  skip_if_not_installed("mlr3learners")

  task <- tsk("mtcars")
  task$select("gear")

  learner <- lrn("regr.svm")

  vis <- Visualizer1DModel$new(task, learner, training_points = TRUE)

  expect_s3_class(vis, "Visualizer1DModel")
  expect_true(!is.null(vis$points_x))
  expect_true(!is.null(vis$points_y))
  
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
  
  # Should have training points layer
  expect_true(length(p$layers) >= 2) # function line + training points
})

test_that("1D Model initialization with custom xlim works", {
  skip_if_not_installed("mlr3learners")

  task <- tsk("mtcars")
  task$select("gear")

  learner <- lrn("regr.svm")

  # Test with custom x limits
  vis <- Visualizer1DModel$new(task, learner, xlim = c(2, 5), n_points = 50)

  expect_s3_class(vis, "Visualizer1DModel")
  expect_equal(length(vis$fun_x), 50)
  expect_true(min(vis$fun_x) >= 2)
  expect_true(max(vis$fun_x) <= 5)
  
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("1D Model fails with multi-feature task", {
  skip_if_not_installed("mlr3learners")

  task <- tsk("mtcars")
  # Don't select features - should have multiple features

  learner <- lrn("regr.svm")

  expect_error(
    Visualizer1DModel$new(task, learner),
    "Task must have exactly 1 feature"
  )
})

test_that("1D Model with integer features works", {
  skip_if_not_installed("mlr3learners")

  task <- tsk("mtcars")
  task$select("cyl")  # Use cylinder count - an integer feature

  learner <- lrn("regr.svm")

  vis <- Visualizer1DModel$new(task, learner)

  expect_s3_class(vis, "Visualizer1DModel")
  
  p <- vis$plot()
  expect_s3_class(p, "ggplot")
})
