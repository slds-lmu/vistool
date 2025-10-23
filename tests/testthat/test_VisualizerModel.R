test_that("VisualizerModel with 1D regression task works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select("gear")

  learner = lrn("regr.svm")

  vis = VisualizerModel$new(task, learner)

  expect_s3_class(vis, "VisualizerModel")
  expect_s3_class(vis, "Visualizer")
  expect_identical(vis$task, task)
  expect_identical(vis$learner, learner)

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerModel with 2D regression task works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = lrn("regr.svm")

  vis = VisualizerModel$new(task, learner)

  expect_s3_class(vis, "VisualizerModel")
  expect_s3_class(vis, "Visualizer")
  expect_identical(vis$task, task)
  expect_identical(vis$learner, learner)

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerModel with 1D classification task works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("spam")
  task$select("receive")

  learner = lrn("classif.svm", predict_type = "prob")

  vis = VisualizerModel$new(task, learner)

  expect_s3_class(vis, "VisualizerModel")

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerModel with 2D classification task works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("spam")
  task$select(c("you", "credit"))

  learner = lrn("classif.svm", predict_type = "prob")

  vis = VisualizerModel$new(task, learner)

  expect_s3_class(vis, "VisualizerModel")

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerModel add_training_data works for 1D", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select("gear")

  learner = lrn("regr.svm")

  vis = VisualizerModel$new(task, learner)
  vis$add_training_data()

  p = vis$plot()
  expect_s3_class(p, "ggplot")

  # Should have training points layer
  expect_true(length(p$layers) >= 2) # function line + training points
})

test_that("VisualizerModel add_training_data works for 2D", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = lrn("regr.svm")

  vis = VisualizerModel$new(task, learner)
  vis$add_training_data()

  p = vis$plot()
  expect_s3_class(p, "ggplot")

  # Should have additional layers for training data
  expect_true(length(p$layers) >= 2)
})

test_that("VisualizerModel add_boundary works for 1D", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select("gear")

  learner = lrn("regr.svm")

  vis = VisualizerModel$new(task, learner)
  vis$add_boundary() # Default boundary

  p = vis$plot()
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2) # function line + boundary line
})

test_that("VisualizerModel add_boundary works for 2D", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("spam")
  task$select(c("you", "credit"))

  learner = lrn("classif.svm", predict_type = "prob")

  vis = VisualizerModel$new(task, learner)
  vis$add_boundary() # Should default to 0.5 for probability predictions

  p = vis$plot()
  expect_s3_class(p, "ggplot")
  # Should have raster + contour layers for boundary
  expect_true(length(p$layers) >= 2)
})

test_that("VisualizerModel with custom limits works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  # 1D case with x1_limits
  task = tsk("mtcars")
  task$select("gear")
  learner = lrn("regr.svm")

  vis = VisualizerModel$new(task, learner, x1_limits = c(2, 5), n_points = 50)
  p = vis$plot()
  expect_s3_class(p, "ggplot")

  # 2D case
  task2d = tsk("mtcars")
  task2d$select(c("gear", "cyl"))
  vis2d = VisualizerModel$new(task2d, learner, x1_limits = c(2, 5), x2_limits = c(4, 8))
  p2d = vis2d$plot()
  expect_s3_class(p2d, "ggplot")
})

test_that("VisualizerModel fails with >2D task", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl", "carb")) # 3 features

  learner = lrn("regr.svm")

  expect_error(
    VisualizerModel$new(task, learner),
    "VisualizerModel supports only 1D and 2D tasks"
  )
})

test_that("VisualizerModel classification with custom colors works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("spam")
  task$select("receive")

  learner = lrn("classif.svm", predict_type = "response")

  vis = VisualizerModel$new(task, learner)

  # Custom colors for classification
  vis$add_training_data(color = c("spam" = "red", "nonspam" = "blue"))

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("VisualizerModel chaining methods works", {
  skip_if_not_installed("e1071")
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = lrn("regr.svm")

  # Test method chaining
  vis = VisualizerModel$new(task, learner)$
    add_training_data(color = "red", size = 3)$
    add_boundary(values = c(15, 20), color = "blue")

  p = vis$plot()
  expect_s3_class(p, "ggplot")
})
