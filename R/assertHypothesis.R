#' @title Assertions for Hypothesis objects
#' @export
assertHypothesis = function(x) {
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

#' @export
assertHypothesisPredict = function(h, newdata) {
  assertHypothesis(h)
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
