test_that("multiplication works", {
  require_namespaces("mlr3learners")

  obj_branin = obj("TF_branin")

  vis = Visualizer2DObj$new(obj_branin)

  vis$plot()
})
