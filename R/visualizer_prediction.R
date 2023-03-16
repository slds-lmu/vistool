#' @title Prediction Visualizer
#'
#' Visualizer for prediction hypersurfaces.
#'
#' This class is used to create visualizations/animations.
#' The used plotting backends are plotly (https://plotly.com/) and ggplot2
#' (https://ggplot2.tidyverse.org//).
#' @export
VisualizerPrediction = R6::R6Class(
  "VisualizerPrediction",
  inherit = Visualizer,
  public = list(
    #' @field predictor (`LMPredictor`) The objective which was optimized. 
    #' This object is used to generate the surface/contour lines.
    predictor = NULL,
    
    initialize = function(
      predictor,
      x1limits = NULL, 
      x2limits = NULL, 
      padding = 0,
      npoints = 100
    ) {
      self$predictor = checkmate::assertR6(predictor, "LMPredictor")
      if (predictor$with_intercept) {
        xmat = predictor$xmat[, -1]
      } else xmat = predictor$xmat
      xdim = ncol(xmat)
      if (xdim > 2) {
        stop(
          sprintf("1- or 2-dimensional inputs required, but xdim = %s`", xdim)
        )
      }
      checkmate::assertNumber(padding, lower = 0)
      checkmate::assertNumeric(x1limits, len = 2, null.ok = TRUE)
      checkmate::assertNumeric(x2limits, len = 2, null.ok = TRUE)
      
      if (is.null(x1limits)) {
        x1limits = c(min(xmat[, 1]), max(xmat[, 1]))
      }
      if (xdim > 1 & is.null(x2limits)) {
        x2limits = c(min(xmat[, 2]), max(xmat[, 2]))
      }
      x1pad = (max(x1limits) - min(x1limits)) * padding
      
      self$grid = list(
        x1 = unique(
          seq(
            min(x1limits) - x1pad, max(x1limits) + x1pad, length.out = npoints
          )
        )
      )
      
      if (xdim > 1) {
        x2pad = (max(x2limits) - min(x2limits)) * padding
        self$grid = append(
          self$grid,
          list(
            x2 = unique(
              seq(
                min(x2limits) - x2pad, max(x2limits) + x2pad, 
                length.out = npoints
              )
            )
          )
        )
        lm_surface = expand.grid(
          x1 = self$grid$x1, x2 = self$grid$x2, KEEP.OUT.ATTRS = F
        )
        lm_surface$y = rep(0, length(self$grid$x1))
        self$zmat = reshape2::acast(lm_surface, x2 ~ x1, value.var = "y")
        self$zmat = outer(
          self$grid$x1, 
          self$grid$x2, 
          function(x1, x2) {
            xin = cbind(x1, x2)
            if (self$predictor$with_intercept) xin = cbind(1, xin)
            apply(xin, 1, function(x) x %*% self$predictor$coeffs)
            }
          )
      } else self$zmat =  self$grid$x1 %*% self$predictor$coeffs

      return(invisible(self))
      
    },
    
    addLayerScatter = function(
      covariate_1, 
      covariate_2 = NULL, 
      marker = list(size = 3, color = "black"),
      ...
    ) {
      if (private$p_layer_primary == "line") {
        # TODO
      } else if (private$p_layer_primary == "contour") {
        # TODO
      } else if (private$p_layer_primary == "surface") {
        private$p_plot = private$p_plot %>% 
          add_trace(
            x = self$predictor$prediction[[covariate_1]], 
            y = self$predictor$prediction[[covariate_2]],
            z = self$predictor$prediction$pred,
            mode = "markers", 
            type = "scatter3d",
            marker = marker,
            showlegend = FALSE, 
            inherit = FALSE,
            ...
          )
      }
      return(invisible(self))
    },
    
    addLayerResiduals = function() {}

  )
)

n_points <- 10L
set.seed(123)
x_1 <- rnorm(n_points, mean = 1, sd = 1)
x_2 <- rnorm(n_points, mean = 1, sd = 1)
set.seed(456)
y_univ <- 1 + 0.5 * x_1 + rnorm(n_points, sd = 0.5)
y_biv <- 1 + 0.5 * x_1 + 0.5 * x_2 + rnorm(n_points, sd = 0.5)
dt_univ <- data.table(x_1, y = y_univ)
dt_biv <- data.table(x_1, x_2, y = y_biv)

Xmat_univ = model.matrix(~ x_1, dt_univ)
Xmat_biv = model.matrix(~ x_1 + x_2, dt_biv)

obj = rloss_dict$get("RL_l2")
obj$reset_xdim(3L)
obj$reset_fargs(Xmat = Xmat_biv, y = y_biv)

opt = OptimizerGD$new(
  obj, x_start = c(2, 2, 1.6), print_trace = FALSE, lr = 0.0001
)
opt$optimize(steps = 1000)

preddy = LMPredictor$new("test", dt_biv, y ~ x_1 + x_2, opt$x)
head(preddy$predict())

viz = VisualizerPrediction$new(preddy)
viz$initLayerBivariate(type = "surface")
viz$addLayerScatter("x_1", "x_2")
viz$plot()

# foo = VisualizerOptim$new(obj, x1limits = c(-10, 10), x2limits = c(-10, 10))
# foo$initLayerBivariate()
# foo$addLayerOptimizationTrace(opt)
# foo$plot()