#' @title Hypothesis wrapper for functional models
#'
#' @description
#' Construct a hypothesis object that wraps a user-supplied function with minimal
#' metadata so it can be visualized like a learner.
#'
#' @param fun (`function`)
#'   Function defining the hypothesis. Accepted signatures:
#'   - 1D: `fun(x, ...)` or `fun(data)` where `data` is a data.frame with one predictor column
#'   - 2D: `fun(x, y, ...)` or `fun(data)` where `data` is a data.frame with two predictor columns
#' @param type (`character(1)`) One of "regr" or "classif".
#' @param predictors (`character()`) Names of the predictor columns. Length must be 1 or 2.
#' @param link (`character(1)`) Link for classif outputs: "identity" (default) or "logit".
#'   For regression, only "identity" is used.
#' @param domain (`list`|`NULL`) Named list with limits per predictor, e.g.,
#'   `list(x = c(-3, 3))` for 1D or `list(x = c(-3,3), y = c(-3,3))` for 2D. Used
#'   when no Task is provided.
#' @param levels (`character(2)`|`NULL`) Class labels for binary classification.
#'
#' @return An `R6` Hypothesis object with a `predict(newdata)` method.
#' @rdname Hypothesis
#' @export
hypothesis = function(fun, type, predictors, link = "identity", domain = NULL, levels = NULL) {
  Hypothesis$new(fun = fun, type = type, predictors = predictors, link = link, domain = domain, levels = levels)
}

#' @title Hypothesis R6 class
#'
#' @description
#' R6 class wrapping a user-supplied function plus minimal metadata so it can be
#' visualized like a learner on 1D or 2D inputs.
#'
#' @field fun (`function`) User-provided function implementing the hypothesis.
#' @field type (`character(1)`) Prediction type, one of "regr" or "classif".
#' @field predictors (`character()`) Names of predictor columns (length 1 or 2).
#' @field input_dim (`integer(1)`) Number of predictors, derived from `predictors`.
#' @field link (`character(1)`) Link for classif output: "identity" or "logit".
#' @field domain (`list`|`NULL`) Named list with limits per predictor for plotting without a Task.
#' @field levels (`character(2)`|`NULL`) Class labels for binary classification.
#'
#' @export
Hypothesis = R6::R6Class("Hypothesis",
  public = list(
    fun = NULL,
    type = NULL,
    predictors = NULL,
    input_dim = NULL,
    link = NULL,
    domain = NULL,
    levels = NULL,

    #' @description Create a new Hypothesis instance
    #' @param fun (`function`) See class field `fun`.
    #' @param type (`character(1)`) See class field `type`.
    #' @param predictors (`character()`) See class field `predictors`.
    #' @param link (`character(1)`) See class field `link`.
    #' @param domain (`list`|`NULL`) See class field `domain`.
    #' @param levels (`character(2)`|`NULL`) See class field `levels`.
    initialize = function(fun, type, predictors, link = "identity", domain = NULL, levels = NULL) {
      checkmate::assert_function(fun)
      checkmate::assert_choice(type, c("regr", "classif"))
      checkmate::assert_character(predictors, any.missing = FALSE, min.len = 1, max.len = 2)
      checkmate::assert_choice(link, c("identity", "logit"))
      checkmate::assert_list(domain, types = "numeric", names = "named", null.ok = TRUE)
      if (!is.null(domain)) {
        for (nm in names(domain)) checkmate::assert_numeric(domain[[nm]], len = 2)
      }
      if (!is.null(levels)) checkmate::assert_character(levels, len = 2)

      self$fun = fun
      self$type = type
      self$predictors = predictors
      self$input_dim = length(predictors)
      self$link = link
      self$domain = domain
      self$levels = if (!is.null(levels)) levels else c("negative", "positive")

      # simple dry-run validation on a tiny dummy newdata based on domain if provided
      try(
        {
          dummy = NULL
          if (!is.null(domain)) {
            vals = lapply(predictors, function(p) mean(domain[[p]]))
            dummy = as.data.frame(setNames(vals, predictors))
          }
          # Only validate if we can construct dummy
          if (!is.null(dummy)) {
            .pred = self$predict(dummy)
            invisible(.pred)
          }
        },
        silent = TRUE)
    },

    #' @description Predict on newdata
    #' @param newdata (`data.frame`) with columns matching `predictors` in order
    #' @return numeric vector of predictions (regression) or probabilities in [0,1] (classification)
    predict = function(newdata) {
      checkmate::assert_data_frame(newdata)
      checkmate::assert_true(all(self$predictors %in% colnames(newdata)))
      # Reorder columns to expected order using base data.frame subsetting
      nd = as.data.frame(newdata)
      df = nd[, self$predictors, drop = FALSE]

      # dispatch by function signature
      fmls = names(formals(self$fun))
      out = NULL
      if (length(fmls) >= 1 && (fmls[1] %in% c("data", "newdata"))) {
        out = self$fun(df)
      } else if (self$input_dim == 1) {
        if (length(fmls) >= 1) {
          out = do.call(self$fun, list(df[[1]]))
        } else {
          stop("Hypothesis function signature not recognized for 1D.")
        }
      } else if (self$input_dim == 2) {
        if (length(fmls) >= 2) {
          out = do.call(self$fun, list(df[[1]], df[[2]]))
        } else if (length(fmls) >= 1) {
          # try single-arg vectorized function taking cbind(x,y)
          out = self$fun(df)
        } else {
          stop("Hypothesis function signature not recognized for 2D.")
        }
      }

      # coerce to numeric vector
      out = as.numeric(out)
      checkmate::assert_numeric(out, len = nrow(df), any.missing = FALSE)

      # apply link if needed
      if (self$type == "classif") {
        if (self$link == "logit") out = 1 / (1 + exp(-out))
        # clamp to [0,1] with tolerance
        out = pmin(pmax(out, 0), 1)
      }
      return(out)
    }
  )
)


#' @title Assertions for Hypothesis objects
#'
#' @description
#' Validate a `Hypothesis` object and basic properties required by the
#' visualizer and prediction helpers.
#'
#' @param x (`Hypothesis`) The hypothesis object to validate.
#'
#' @return Invisibly `TRUE` on success, otherwise the function raises an error.
#' @rdname assert_hypothesis
#' @keywords internal
#' @noRd
assert_hypothesis = function(x) {
  if (!inherits(x, "Hypothesis")) stop("Object is not a Hypothesis")
  checkmate::assert_choice(x$type, c("regr", "classif"))
  checkmate::assert_character(x$predictors, min.len = 1, max.len = 2, any.missing = FALSE)
  checkmate::assert_choice(x$link, c("identity", "logit"))
  if (!is.null(x$domain)) {
    checkmate::assert_list(x$domain, names = "named")
    for (nm in names(x$domain)) checkmate::assert_numeric(x$domain[[nm]], len = 2)
  }
  invisible(TRUE)
}

#'
#' Assertion for hypothesis predictions
#'
#' Check that a hypothesis `h` can produce valid predictions on `newdata`.
#'
#' @param h (`Hypothesis`) Hypothesis object created with `Hypothesis$new()` or via `hypothesis()`.
#' @param newdata (`data.frame`) New data to predict on. Must contain the columns named in `h$predictors`.
#'
#' @return Invisibly `TRUE` on success, otherwise an error is raised.
#' @rdname assert_hypothesis
#' @keywords internal
#' @noRd
assert_hypothesis_predict = function(h, newdata) {
  assert_hypothesis(h)
  checkmate::assert_data_frame(newdata)
  checkmate::assert_true(all(h$predictors %in% colnames(newdata)))
  p = h$predict(newdata)
  checkmate::assert_numeric(p, len = nrow(newdata), any.missing = FALSE, all.missing = FALSE)
  if (h$type == "classif") {
    checkmate::assert_number(min(p), lower = 0 - 1e-8)
    checkmate::assert_number(max(p), upper = 1 + 1e-8)
  }
  invisible(TRUE)
}
