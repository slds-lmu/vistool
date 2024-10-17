
library(devtools)
library(ggplot2)
library(ggsci)
load_all()

# print(as.data.table(dict_loss))
# lf1 = lss("hinge")
# lf2 = lss("cross-entropy")
# losses = list(lf1, lf2)
# vis = VisualizerLosses$new(losses)
# vis$line_type = c(hinge="solid", logloss="dashed")
# vis$line_col = c(hinge="pink", logloss="green")
# vis$line_width = c(hinge=9, logloss=2)
# print(vis)
# print(vis$plot())


# task = tsk("boston_housing")
# task$select(c("age"))
# learner = lrn("regr.rpart")
# vis = Visualizer1DModel$new(task = task, learner = learner, plot_data = TRUE)
# vis$line_col = "red"
# vis$line_type = "dashed"
# vis$line_width = 5
# pl = vis$plot()
# print(pl)

# print(as.data.table(dict_objective))
obj = obj("TF_branin")
# obj = Objective$new("f", fun = function(x) sum(x^2), xdim = 1, lower = -1, upper = 2)
vis = Visualizer2DObj$new(obj = obj)
pl = vis$plot()
print(pl)

