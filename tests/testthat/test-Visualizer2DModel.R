test_that("2D Model with regression task works", {
  require_namespaces("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = lrn("regr.svm")

  vis = Visualizer2DModel$new(task, learner)

  vis$plot()
})

test_that("2D model with regression task and training points works", {
  require_namespaces("mlr3learners")

  task = tsk("mtcars")
  task$select(c("gear", "cyl"))

  learner = lrn("regr.svm")

  vis = Visualizer2DModel$new(task, learner, training_points = TRUE)

  vis$plot()
})

test_that("2D model with classification task works", {
  require_namespaces("mlr3learners")

  task = tsk("spam")
  task$select(c("you", "credit"))

  learner = lrn("classif.svm", predict_type = "prob")

  vis = Visualizer2DModel$new(task, learner)

  vis$plot()
})


test_that("2D model with classification task and training points works", {
  require_namespaces("mlr3learners")

  task = tsk("spam")
  task$select(c("you", "credit"))

  learner = lrn("classif.svm", predict_type = "prob")

  vis = Visualizer2DModel$new(task, learner, training_points = TRUE)

  vis$plot()
})
