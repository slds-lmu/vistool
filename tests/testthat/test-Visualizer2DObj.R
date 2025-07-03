test_that("2D Objective works", {
  require_namespaces("mlr3learners")

  obj_branin <- obj("TF_branin")

  vis <- as_visualizer(obj_branin, type = "2d")

  vis$plot()
})
