test_that("LearnerRegrLMFormula works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = LearnerRegrLMFormula$new()
  learner$param_set$values$formula = mpg ~ gear + cyl

  expect_s3_class(learner, "LearnerRegrLMFormula")
  expect_s3_class(learner, "LearnerRegr")

  learner$train(task)
  expect_true(!is.null(learner$model))

  prediction = learner$predict(task)
  expect_s3_class(prediction, "PredictionRegr")
  expect_equal(length(prediction$response), task$nrow)
})

test_that("LearnerRegrLMFormula formula handling works", {
  skip_if_not_installed("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = LearnerRegrLMFormula$new()
  learner$param_set$values$formula = mpg ~ gear + I(gear^2)

  learner$train(task)
  expect_true(!is.null(learner$model))

  prediction = learner$predict(task)
  expect_s3_class(prediction, "PredictionRegr")
})
