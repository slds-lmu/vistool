test_that("1D Model with regression task works", {
  require_namespaces("mlr3learners")

  task <- tsk("mtcars")
  task$select("gear")

  learner <- lrn("regr.svm")

  vis <- Visualizer1DModel$new(task, learner)

  vis$plot()
})

test_that("1D Model with regression task and trainings data works", {
  require_namespaces("mlr3learners")

  task <- tsk("mtcars")
  task$select("gear")

  learner <- lrn("regr.svm")

  vis <- Visualizer1DModel$new(task, learner, training_points = TRUE)

  vis$plot()
})
