test_that("2D Model with training data works", {
  require_namespaces("mlr3learners")

  task <- tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner <- lrn("regr.svm")

  vis <- Visualizer2DModel$new(task, learner)
  vis$add_training_data()

  expect_true(!is.null(vis$points_x1))
  expect_true(!is.null(vis$points_x2))
  expect_true(!is.null(vis$points_y))

  p <- vis$plot()
  expect_s3_class(p, "gg")
})

test_that("2D Model with classification and decision boundary works", {
  require_namespaces("mlr3learners")

  task <- tsk("spam")
  task$select(c("you", "credit"))

  learner <- lrn("classif.svm", predict_type = "prob")

  vis <- Visualizer2DModel$new(task, learner)
  vis$add_training_data()
  vis$add_decision_boundary()

  p <- vis$plot()
  expect_s3_class(p, "gg")

  # Should have 4 layers: contour_filled, contour (white), contour (decision boundary), points
  expect_equal(length(p$layers), 4)
})

test_that("Decision boundary only works with probability predictions", {
  require_namespaces("mlr3learners")

  task <- tsk("spam")
  task$select(c("you", "credit"))

  learner <- lrn("classif.svm", predict_type = "response")

  vis <- Visualizer2DModel$new(task, learner)

  expect_error(vis$add_decision_boundary(), "Decision boundary can only be added for probability predictions")
})
