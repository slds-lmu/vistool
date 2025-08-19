test_that("retrain flag controls (re)training", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("paradox")

  LearnerRegrCount = R6::R6Class("LearnerRegrCount",
    inherit = mlr3::LearnerRegr,
    public = list(
      train_calls = 0L,
      initialize = function() {
        super$initialize(
          id = "regr.count",
          feature_types = c("numeric", "integer"),
          predict_types = c("response"),
          param_set = paradox::ps()
        )
      }
    ),
    private = list(
      .train = function(task) {
        self$train_calls = self$train_calls + 1L
        self$model = list(mean = mean(task$data(cols = task$target_names)[[1]]))
      },
      .predict = function(task) {
        mlr3::PredictionRegr$new(task = task, response = rep(self$model$mean, task$nrow))
      }
    )
  )

  task = mlr3::TaskRegr$new("t", data.frame(x = 1:10, y = 1:10), target = "y")

  # Case A: fresh learner, retrain = TRUE triggers training
  l1 = LearnerRegrCount$new()
  vis1 = as_visualizer(task, learner = l1, retrain = TRUE)
  expect_equal(l1$train_calls, 1L)
  expect_true(!is.null(l1$model))

  # Case B: pre-trained learner, retrain = FALSE reuses model
  l2 = LearnerRegrCount$new()
  l2$train(task)
  expect_equal(l2$train_calls, 1L)
  vis2 = as_visualizer(task, learner = l2, retrain = FALSE)
  expect_equal(l2$train_calls, 1L) # unchanged

  # Case C: untrained learner, retrain = FALSE still trains with warning
  l3 = LearnerRegrCount$new()
  expect_equal(l3$train_calls, 0L)
  expect_warning(as_visualizer(task, learner = l3, retrain = FALSE), "retrain = FALSE ignored")
  expect_equal(l3$train_calls, 1L)
})
