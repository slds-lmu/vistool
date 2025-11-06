# Loss function

Represents a loss function for regression or classification tasks.

## Details

Loss functions are expected to be vectorized and to accept a single
numeric argument representing the input on the chosen scale:

- `"score"`: residuals `y_true - y_pred` for regression, or signed
  margins `y_true * f(x)` for binary classification.

- `"probability"`: predicted probabilities for the positive class.
  Visualizers obtain the loss for `y = 0` by evaluating the function at
  `1 - p`.

The returned value must be a numeric vector of losses with the same
length as the input. Additional optional parameters can be encoded via
closures and are configurable through
[`lss()`](https://slds-lmu.github.io/vistool/reference/lss.md), which
wraps the stored functions with the supplied arguments.

## Public fields

- `id`:

  (`character(1)`)  
  Identifier for the loss function.

- `label`:

  (`character(1)`)  
  Label for the loss function.

- `task_type`:

  (`character(1)`)  
  Task type: `"regr"` or `"classif"`.

- `input_default`:

  (`character(1)`)  
  The natural input scale for the loss: `"score"` or `"probability"`.

- `input_supported`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  All input scales this loss can be expressed on (subset of
  `c("score", "probability")`).

- `fun`:

  (`function` or `list`)  
  The loss function itself. Provide either a vectorized closure matching
  the active input scale or a named list of such closures keyed by
  `"score"` and / or `"probability"`.

## Methods

### Public methods

- [`LossFunction$new()`](#method-LossFunction-new)

- [`LossFunction$get_fun()`](#method-LossFunction-get_fun)

- [`LossFunction$clone()`](#method-LossFunction-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LossFunction$new(
      id,
      label,
      task_type,
      fun,
      input_default = "score",
      input_supported = c("score")
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the loss function.

- `label`:

  (`character(1)`)  
  Label for the loss function.

- `task_type`:

  (`character(1)`)  
  Task type: `"regr"` or `"classif"`.

- `fun`:

  (`function` or `list`)  
  The loss function. Supply a vectorized closure matching the active
  input scale or a named list of closures keyed by `"score"` /
  `"probability"`.

- `input_default`:

  (`character(1)`)  
  Default input scale (`"score"` or `"probability"`).

- `input_supported`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Character vector of supported input scales. Must contain
  `input_default`.

------------------------------------------------------------------------

### Method `get_fun()`

Get the loss function for a specific input type.

#### Usage

    LossFunction$get_fun(input_type = self$input_default)

#### Arguments

- `input_type`:

  (`character(1)`)  
  The input type: `"score"` or `"probability"`.

#### Returns

A function for the specified input type.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LossFunction$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
