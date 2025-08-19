## Internal mlr3 helpers (not exported)
## Optionally train a learner depending on `retrain` flag.
internal_maybe_train = function(learner, task, retrain) { # nocov start
  if (is.null(learner)) return(invisible(FALSE))
  checkmate::assert_flag(retrain)
  if (retrain) {
    learner$train(task)
    return(invisible(TRUE))
  }
  if (is.null(learner$model)) {
    warning("Learner not trained; retrain = FALSE ignored - training now.")
    learner$train(task)
    return(invisible(TRUE))
  }
  invisible(FALSE)
} # nocov end
