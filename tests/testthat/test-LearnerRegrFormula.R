test_that("LearnerRegrLMFormula works", {
  skip_if_not_installed("mlr3learners")
  
  # Create a simple regression task
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  
  # Create learner with required formula
  learner = LearnerRegrLMFormula$new()
  learner$param_set$values$formula = mpg ~ gear + cyl
  
  expect_s3_class(learner, "LearnerRegrLMFormula")
  expect_s3_class(learner, "LearnerRegr")
  
  # Test training
  learner$train(task)
  expect_true(!is.null(learner$model))
  
  # Test prediction
  prediction = learner$predict(task)
  expect_s3_class(prediction, "PredictionRegr")
  expect_equal(length(prediction$response), task$nrow)
})

test_that("LearnerRegrLMFormula formula handling works", {
  skip_if_not_installed("mlr3learners")
  
  task = tsk("mtcars")
  task$select(c("gear", "cyl"))
  
  # Test with custom formula
  learner = LearnerRegrLMFormula$new()
  learner$param_set$values$formula = mpg ~ gear + I(gear^2)
  
  learner$train(task)
  expect_true(!is.null(learner$model))
  
  # Test prediction works
  prediction = learner$predict(task)
  expect_s3_class(prediction, "PredictionRegr")
})
