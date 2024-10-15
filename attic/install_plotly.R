library(vistool)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
task = tsk("boston_housing")
task$select(c("age", "rm"))
learner = lrn("regr.svm")
vis = as_visualizer(task, learner)
vis$plot()
x = vis$plot()

# fails with missing python dependencies
plotly::save_image(x, "test.png")

# install missing packages
reticulate::py_install(c("kaleido", "plotly"))

# works now
plotly::save_image(x, "test.png")

######################


Sys.setenv("plotly_username" = "xxxx")
Sys.setenv("plotly_api_key" = "xxxx")




library(vistool)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
task = tsk("boston_housing")
task$select(c("age", "rm"))
learner = lrn("regr.svm")
vis = as_visualizer(task, learner)
vis$plot()
x = vis$plot()

plotly::api_create(x, filename = "test.png")
