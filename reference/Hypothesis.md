# Hypothesis wrapper for functional models

Construct a hypothesis object that wraps a user-supplied function with
minimal metadata so it can be visualized like a learner.

R6 class wrapping a user-supplied function plus minimal metadata so it
can be visualized like a learner on 1D or 2D inputs.

## Usage

``` r
hypothesis(
  fun,
  type,
  predictors,
  link = "identity",
  domain = NULL,
  levels = NULL
)
```

## Arguments

- fun:

  (`function`) Function defining the hypothesis. Accepted signatures:

  - 1D: `fun(x, ...)` or `fun(data)` where `data` is a data.frame with
    one predictor column

  - 2D: `fun(x, y, ...)` or `fun(data)` where `data` is a data.frame
    with two predictor columns

- type:

  (`character(1)`) One of "regr" or "classif".

- predictors:

  ([`character()`](https://rdrr.io/r/base/character.html)) Names of the
  predictor columns. Length must be 1 or 2.

- link:

  (`character(1)`) Link for classif outputs: "identity" (default) or
  "logit". For regression, only "identity" is used.

- domain:

  (`list`\|`NULL`) Named list with limits per predictor, e.g.,
  `list(x = c(-3, 3))` for 1D or `list(x = c(-3,3), y = c(-3,3))` for
  2D. Used when no Task is provided.

- levels:

  (`character(2)`\|`NULL`) Class labels for binary classification.

## Value

An `R6` Hypothesis object with a `predict(newdata)` method.

## Public fields

- `fun`:

  (`function`) User-provided function implementing the hypothesis.

- `type`:

  (`character(1)`) Prediction type, one of "regr" or "classif".

- `predictors`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Names of
  predictor columns (length 1 or 2).

- `input_dim`:

  (`integer(1)`) Number of predictors, derived from `predictors`.

- `link`:

  (`character(1)`) Link for classif output: "identity" or "logit".

- `domain`:

  (`list`\|`NULL`) Named list with limits per predictor for plotting
  without a Task.

- `levels`:

  (`character(2)`\|`NULL`) Class labels for binary classification.

## Methods

### Public methods

- [`Hypothesis$new()`](#method-Hypothesis-new)

- [`Hypothesis$predict()`](#method-Hypothesis-predict)

- [`Hypothesis$clone()`](#method-Hypothesis-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new Hypothesis instance

#### Usage

    Hypothesis$new(
      fun,
      type,
      predictors,
      link = "identity",
      domain = NULL,
      levels = NULL
    )

#### Arguments

- `fun`:

  (`function`) See class field `fun`.

- `type`:

  (`character(1)`) See class field `type`.

- `predictors`:

  ([`character()`](https://rdrr.io/r/base/character.html)) See class
  field `predictors`.

- `link`:

  (`character(1)`) See class field `link`.

- `domain`:

  (`list`\|`NULL`) See class field `domain`.

- `levels`:

  (`character(2)`\|`NULL`) See class field `levels`.

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Predict on newdata

#### Usage

    Hypothesis$predict(newdata)

#### Arguments

- `newdata`:

  (`data.frame`) with columns matching `predictors` in order

#### Returns

numeric vector of predictions (regression) or probabilities in \\\[0,
1\]\\ (classification)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Hypothesis$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
