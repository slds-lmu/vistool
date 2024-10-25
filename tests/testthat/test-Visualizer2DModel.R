test_that("multiplication works", {
  require_namespaces("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = lrn("regr.svm")

  vis = Visualizer2DModel$new(task, learner)

  vis$plot()
})

test_that("multiplication works", {
  require_namespaces("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = lrn("regr.svm")

  vis = Visualizer2DModel$new(task, learner)

  vis$plot(training_data = TRUE)
})

test_that("multiplication works", {
  require_namespaces("mlr3learners")

  task = tsk("spam")
  task$select(c("you", "credit"))

  learner = lrn("classif.svm", predict_type = "prob")

  vis = Visualizer2DModel$new(task, learner)

  vis$plot()
  vis$plot(training_data = TRUE)
})
