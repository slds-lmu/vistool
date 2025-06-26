library(devtools)
devtools::load_all(".")

library(mlr3verse)
library(mlr3oml)

## 2D Model

task = tsk("oml", data_id = 8)
task$select(c("alkphos", "mcv"))
learner = lrn("regr.svm")
vis = as_visualizer(task, learner)
vis$plot()

learner = lrn("regr.ranger")
vis = as_visualizer(task, learner)
vis$plot()

## 2D Training Data

task = tsk("oml", data_id = 8)
task$select(c("alkphos", "mcv"))
learner = lrn("regr.svm")
vis = as_visualizer(task, learner)
vis$add_training_data()
vis$plot()

## 2D Probability

task = tsk("pima")
task = po("imputemean")$train(list(task))[[1]]
task$select(c("insulin", "mass"))
learner = lrn("classif.svm", predict_type = "prob")
vis = as_visualizer(task, learner)
vis$plot()

## 2D Probability Training Data

task = tsk("pima")
task = po("imputemean")$train(list(task))[[1]]
task$select(c("insulin", "mass"))
learner = lrn("classif.svm", predict_type = "prob")
vis = as_visualizer(task, learner)
vis$add_training_data()
vis$plot()

## 2D Probability Decision Boundary

task = tsk("pima")
task = po("imputemean")$train(list(task))[[1]]
task$select(c("insulin", "mass"))
learner = lrn("classif.svm", predict_type = "prob")
vis = as_visualizer(task, learner)
vis$add_decision_boundary()
vis$plot()

## 1D Vis

vis = Visualizer1D$new(x = seq(10), y = seq(10), plot_lab = "test")
vis$plot()

## 1D Model

task = tsk("oml", data_id = 8)
task$select("alkphos")
learner = lrn("regr.svm")
vis = as_visualizer(task, learner)
vis$plot()

## 1D Objective

obj = Objective$new(fun = function(x) sin(x) + sin(10 * x / 3),
  id = "Problem02",
  label = "Problem02",
  xdim = 1,
  limits_lower = 2.7,
  limits_upper = 7.5,
  minimize = TRUE)

vis = as_visualizer(obj)
vis$plot()

## 1D Optimization Trace

opt = OptimizerMomentum$new(obj, x_start = 4, lr = 0.2)
opt$optimize(30)

vis$add_optimization_trace(opt)
vis$plot()


## Regression Loss Functions

loss_function = lss("huber")
vis = as_visualizer(loss_function, y_pred = seq(-10, 10), y_true = 0, delta = 1)
vis$plot()

loss_function = lss("l2_se")
vis = as_visualizer(loss_function, y_pred = seq(-10, 10), y_true = 0)
vis$plot()

loss_function = lss("l1_ae")
vis = as_visualizer(loss_function, y_pred = seq(-10, 10), y_true = 0)
vis$plot()

loss_function = lss("log-cosh")
vis = as_visualizer(loss_function, y_pred = seq(-10, 10), y_true = 0)
vis$plot()

## Combine Regression Loss Functions

loss_function = lss("l2_se")
vis_1 = as_visualizer(loss_function, y_pred = seq(-10, 10), y_true = 0)

loss_function = lss("l1_ae")
vis_2 = as_visualizer(loss_function, y_pred = seq(-10, 10), y_true = 0)

loss_function = lss("log-cosh")
vis_3 = as_visualizer(loss_function, y_pred = seq(-10, 10), y_true = 0)

c(vis_1, vis_2, vis_3)

## Classification Loss Functions

loss_function = lss("cross-entropy")
vis = as_visualizer(loss_function, y_pred = seq(-4, 4), y_true = 1)
vis$plot()

loss_function = lss("hinge")
vis = as_visualizer(loss_function, y_pred = seq(-4, 4), y_true = 1)
vis$plot()


## 2D Model Contour Plot with Decision Boundary

library(mlr3verse)
library(vistools)

task = tsk("spam")
task$select(c("your", "credit"))
learner = lrn("classif.svm", predict_type = "prob")
vis = as_visualizer(task, learner)
vis$init_layer_contour()
vis$add_decision_boundary()
vis$plot()
