#' @title Visualize Model as Interactive Surface
#'
#' @description
#' This class is used to create interactive 3D surface visualizations of learners and tasks
#' for 2D input data using plotly.
#'
#' @template param_x1_limits
#' @template param_x2_limits
#' @template param_padding
#' @template param_n_points
#'
#' @export
VisualizerSurfaceModel <- R6::R6Class("VisualizerSurfaceModel",
  inherit = VisualizerSurface,
  public = list(

    #' @field task (`mlr3::Task`)\cr
    #' Task used to train the model.
    task = NULL,

    #' @field learner (`mlr3::Learner`)\cr
    #' Learner used to train the model.
    learner = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([mlr3::Task])\cr
    #'   The task to train the model on.
    #' @template param_learner
    #' @template param_x1_limits
    #' @template param_x2_limits
    #' @template param_padding
    #' @template param_n_points
    #' @template param_opacity
    #' @template param_colorscale
    #' @template param_opacity
    #' @template param_colorscale
    #' @template param_show_title
    initialize = function(task, learner, x1_limits = NULL, x2_limits = NULL, padding = 0, n_points = 100L,
                          opacity = 0.8, colorscale = list(
                            c(0, "#440154"), c(0.25, "#3b528b"), c(0.5, "#21908c"), 
                            c(0.75, "#5dc863"), c(1, "#fde725")
                          ), show_title = TRUE) {
      self$task <- mlr3::assert_task(task)
      self$learner <- mlr3::assert_learner(learner, task = self$task)
      checkmate::assert_numeric(x1_limits, len = 2, null.ok = TRUE)
      checkmate::assert_numeric(x2_limits, len = 2, null.ok = TRUE)
      checkmate::assert_count(n_points)

      # Validate that task has exactly 2 features
      if (length(self$task$feature_names) != 2) {
        mlr3misc::stopf(
          "3D Model visualization requires a task with exactly 2 features, but got %d",
          length(self$task$feature_names)
        )
      }

      x1 <- self$task$feature_names[1]
      x2 <- self$task$feature_names[2]
      data <- task$data()
      self$learner$train(task)

      x1_limits <- range(data[, x1, with = FALSE])
      x2_limits <- range(data[, x2, with = FALSE])

      grid <- list(
        x1 = seq(x1_limits[1] - padding, x1_limits[2] + padding, length.out = n_points),
        x2 = seq(x2_limits[1] - padding, x2_limits[2] + padding, length.out = n_points)
      )

      newdata <- CJ(grid$x1, grid$x2)
      setnames(newdata, self$task$feature_names)

      original_types <- sapply(self$task$data()[, self$task$feature_names, with = FALSE], class)
      for (col in names(original_types)) {
        if (original_types[col] == "integer") {
          newdata[[col]] <- as.integer(round(newdata[[col]]))
        }
      }
      z <- self$learner$predict_newdata(newdata)[[self$learner$predict_type]]
      if (self$learner$predict_type == "prob") {
        pos_class <- self$task$positive
        z <- z[, pos_class]
      }
      zmat <- matrix(z, nrow = n_points, ncol = n_points, byrow = TRUE)

            super$initialize(
        grid = grid,
        zmat = zmat,
        plot_lab = paste(self$learner$id, "on", self$task$id),
        x1_lab = self$task$feature_names[1],
        x2_lab = self$task$feature_names[2],
        z_lab = self$task$target_names,
        opacity = opacity,
        colorscale = colorscale,
        show_title = show_title
      )

      return(invisible(self))
    },

    #' @description
    #' Adds the training data to the plot.
    #'
    #' @param size (`numeric(1)`)\cr
    #'  Size of the points.
    #' @param color (`character(1)`)\cr
    #' Color of the points.
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)`.
    add_training_data = function(size = 5, color = "grey", ...) {
      if (is.null(private$.plot)) private$.init_default_plot()
      data <- self$task$data()
      x1 <- data[, self$task$feature_names[1], with = FALSE][[1]]
      x2 <- data[, self$task$feature_names[2], with = FALSE][[1]]
      z <- data[, self$task$target_names, with = FALSE][[1]]
      if (self$learner$predict_type == "prob") z <- as.integer(z) - 1


      if (private$.layer_primary == "contour") {
        private$.plot <- private$.plot %>%
          add_trace(
            x = x1,
            y = x2,
            type = "scatter",
            mode = "markers",
            marker = list(
              size = 10,
              color = z,
              cmin = min(self$zmat),
              cmax = max(self$zmat),
              colorscale = list(c(0, 1), c("rgb(176,196,222)", "rgb(160,82,45)")),
              line = list(color = "black", width = 2),
              showscale = FALSE
            ),
            text = ~ paste("x:", x1, "\ny:", x2, " \nz:", z),
            hoverinfo = "text",
            ...
          )
      } else {
        private$.plot <- private$.plot %>%
          add_trace(
            x = x1,
            y = x2,
            z = z,
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 5, color = grey),
            ...
          )
      }



      return(invisible(self))
    },

    #' @description
    #' Adds boundary surface(s) to the plot at specified values.
    #'
    #' @param values (`numeric()`)\cr
    #'   Vector of z-values where to draw boundary surfaces. For classification with probability predictions,
    #'   defaults to 0.5. For regression or response predictions, defaults to quantiles of predictions.
    #' @param color (`character(1)` or `list()`)\cr
    #'   Color specification for boundary surfaces. Default uses a neutral colorscale.
    #' @param ... (`any`)\cr
    #'   Further arguments passed to `add_trace(...)` or `add_surface(...)`.
    add_boundary = function(values = NULL, color = NULL, ...) {
      checkmate::assert_numeric(values, null.ok = TRUE)
      
      if (is.null(private$.plot)) private$.init_default_plot()
      
      # Determine default values based on prediction type
      if (is.null(values)) {
        if (self$learner$predict_type == "prob") {
          values <- 0.5
        } else {
          # For regression or response predictions, use quantiles
          values <- quantile(self$zmat, c(0.25, 0.5, 0.75), na.rm = TRUE)
        }
      }
      
      # Default color scheme
      if (is.null(color)) {
        color <- list(c(0, "rgba(176,196,222,0.5)"), c(1, "rgba(160,82,45,0.5)"))
      }

      for (value in values) {
        z <- matrix(value, nrow = nrow(self$zmat), ncol = ncol(self$zmat), byrow = TRUE)

        if (private$.layer_primary == "contour") {
          llp <- list(x = self$grid$x1, y = self$grid$x2, z = self$zmat)
          private$.plot <- private$.plot %>%
            add_trace(
              name = paste("boundary", value),
              autocontour = FALSE,
              showlegend = FALSE,
              showscale = FALSE,
              x = llp$x,
              y = llp$y,
              z = t(llp$z),
              type = "contour",
              colorscale = list(c(0, 1), c("rgb(0,0,0)", "rgb(0,0,0)")),
              ncontours = 1,
              contours = list(
                start = value,
                end = value,
                coloring = "lines"
              ),
              line = list(
                color = "black",
                width = 3
              ),
              ...
            )
        } else {
          private$.plot <- private$.plot %>%
            add_surface(
              x = self$grid$x1,
              y = self$grid$x2,
              z = z,
              colorscale = color,
              showscale = FALSE,
              name = paste("boundary", value),
              ...
            )
        }
      }
      
      return(invisible(self))
    }
  )
)
