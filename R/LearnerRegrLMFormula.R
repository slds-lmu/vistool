#' @title Linear Model Regression Learner with Formula
#'
#' @name mlr_learners_regr.lm_formula
#'
#' @description
#' Ordinary linear regression with formula.
#' Calls [stats::lm()].
#'
#' @export
LearnerRegrLMFormula = R6Class("LearnerRegrLMFormula",
  inherit = mlr3::LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "regr.formula",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "se"),
        param_set = ps(
          df = p_dbl(default = Inf, tags = "predict"),
          interval = p_fct(c("none", "confidence", "prediction"), tags = "predict"),
          level = p_dbl(default = 0.95, tags = "predict"),
          model = p_lgl(default = TRUE, tags = "train"),
          offset = p_lgl(tags = "train"),
          pred.var = p_uty(tags = "predict"),
          qr = p_lgl(default = TRUE, tags = "train"),
          scale = p_dbl(default = NULL, special_vals = list(NULL), tags = "predict"),
          singular.ok = p_lgl(default = TRUE, tags = "train"),
          x = p_lgl(default = FALSE, tags = "train"),
          y = p_lgl(default = FALSE, tags = "train"),
          rankdeficient = p_fct(c("warnif", "simple", "non-estim", "NA", "NAwarn"), tags = "predict"),
          tol = p_dbl(default = 1e-07, tags = "predict"),
          verbose = p_lgl(default = FALSE, tags = "predict"),
          formula = p_uty(tags = c("train", "predict", "required"))
        ),
        man = "mlr3::mlr_learners_regr.lm_formula",
        label = "Linear Model with Formula"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }

      invoke(stats::lm, data = task$data(), .args = pv, .opts = list(contrasts = c("contr.treatment", "contr.poly")))
    },
    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      newdata = ordered_features(task, self)
      se_fit = self$predict_type == "se"
      prediction = invoke(predict, object = self$model, newdata = newdata, se.fit = se_fit, .args = pv)

      if (se_fit) {
        list(response = unname(prediction$fit), se = unname(prediction$se.fit))
      } else {
        list(response = unname(prediction))
      }
    }
  )
)

ordered_features = function(task, learner) {
  cols = if (is.null(names(learner$state$data_prototype))) learner$state$feature_names else names(learner$state$data_prototype)
  task$data(cols = intersect(cols, task$feature_names))
}
