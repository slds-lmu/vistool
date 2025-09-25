test_that("hypothesis predict works for 1D regr", {
  hyp = hypothesis(function(x) sin(x), type = "regr", predictors = "x", domain = list(x = c(-pi, pi)))
  nd = data.frame(x = seq(-pi, pi, length.out = 5))
  p = hyp$predict(nd)
  expect_equal(length(p), nrow(nd))
  expect_false(any(is.na(p)))
})

test_that("hypothesis predict works for 2D classif with logit", {
  fun = function(x, y) 0.7 * x + 0.2 * y + 4
  hyp = hypothesis(fun, type = "classif", predictors = c("x", "y"), link = "logit", domain = list(x = c(-3, 3), y = c(-3, 3)))
  nd = expand.grid(x = seq(-1, 1, length.out = 4), y = seq(-1, 1, length.out = 4))
  p = hyp$predict(nd)
  expect_true(all(p >= 0 & p <= 1))
  expect_equal(length(p), nrow(nd))
})

test_that("as_visualizer works with hypothesis only (1D)", {
  hyp = hypothesis(function(x) x^2, type = "regr", predictors = "x", domain = list(x = c(-1, 1)))
  vis = as_visualizer(hyp, domain = hyp$domain)
  p = vis$plot()
  expect_s3_class(p, "ggplot")
})

test_that("as_visualizer works with hypothesis only (2D surface)", {
  skip_if_not_installed("plotly")
  fun = function(x, y) x^2 + y^2
  hyp = hypothesis(fun, type = "regr", predictors = c("x", "y"), domain = list(x = c(-1, 1), y = c(-1, 1)))
  vis = as_visualizer(hyp, type = "surface", domain = hyp$domain)
  plt = vis$plot()
  expect_s3_class(plt, "plotly")
})

test_that("error when both learner and hypothesis supplied", {
  skip_if_not_installed("mlr3")
  skip_if_not_installed("rpart")
  task = mlr3::tsk("iris")$filter(1:100)$select(c("Petal.Length", "Petal.Width"))$set_row_roles(NULL)
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
  hyp = hypothesis(function(x, y) plogis(x - y), type = "classif", predictors = c("Petal.Length", "Petal.Width"))
  expect_error(as_visualizer(task, learner = learner, hypothesis = hyp), "Provide exactly one")
})

test_that("error when neither learner nor hypothesis for Task", {
  skip_if_not_installed("mlr3")
  task = mlr3::tsk("iris")$select("Sepal.Length")$set_row_roles(NULL)
  expect_error(as_visualizer(task), "One of 'learner' or 'hypothesis' is required")
})

test_that("error when domain missing for hypothesis-only", {
  hyp = hypothesis(function(x) x, type = "regr", predictors = "x")
  expect_error(as_visualizer(hyp), "domain.*required")
})

test_that("surface type requires 2D hypothesis", {
  hyp = hypothesis(function(x) x, type = "regr", predictors = "x", domain = list(x = c(0, 1)))
  expect_error(as_visualizer(hyp, type = "surface", domain = hyp$domain), "requires a 2D")
})
