# Objective function

This class defines the objective that is used for optimization.

## Value

Returns self invisibly.

## Public fields

- `id`:

  (`character(1)`)  
  The id of the object.

- `label`:

  (`character(1)` The label of the objective, i.e. a.

- `lower`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The lower limits
  for each dimension.

- `upper`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The upper limits
  for each dimension.

- `minimize`:

  (`logical(1)`) Is the problem a minimization problem?

## Active bindings

- `archive`:

  ([`data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  Archive of all calls to `$eval_store`.

- `log_funs`:

  ([`list()`](https://rdrr.io/r/base/list.html)) A list containing
  logging functions. Each function must have argument.

- `xdim`:

  (`integer(1)`) Input dimension of `f`.

- `transform_id`:

  (`character(1)`) Identifier of the active transform.

## Methods

### Public methods

- [`Objective$new()`](#method-Objective-new)

- [`Objective$eval()`](#method-Objective-eval)

- [`Objective$eval_store()`](#method-Objective-eval_store)

- [`Objective$assert_x()`](#method-Objective-assert_x)

- [`Objective$set_transform()`](#method-Objective-set_transform)

- [`Objective$get_transform()`](#method-Objective-get_transform)

- [`Objective$grad()`](#method-Objective-grad)

- [`Objective$hess()`](#method-Objective-hess)

- [`Objective$add_log_fun()`](#method-Objective-add_log_fun)

- [`Objective$clear_archive()`](#method-Objective-clear_archive)

- [`Objective$clone()`](#method-Objective-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Objective$new(
      id,
      fun,
      label = "f",
      xdim,
      lower = NA,
      upper = NA,
      xtest = NULL,
      minimize = FALSE,
      transform = objective_transform_identity(),
      label_base = NULL,
      ...
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Id of the object.

- `fun`:

  (`function` The objective function. The first argument must be a
  numerical input of length `xdim`.

- `label`:

  (`character(1)` The label of the objective, i.e. a.

- `xdim`:

  (`integer(1)`) The input dimension of `fun`. Use `xdim = NA` for an
  arbitrary input dimension.

- `lower`:

  (`numeric(xdim)`) The lower boundaries for inputs to `fun`. Must

- `upper`:

  (`numeric(xdim)`) The upper boundaries for inputs to `fun`. Must be of
  length `xdim`.

- `xtest`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Test value for
  `fun` during initialization. If not defined,
  `xtest = rep(0, ifelse(is.na(xdim), 2, xdim))` is used.

- `minimize`:

  (`logical(1)`) Is the problem a minimization problem? Default is no
  (`FALSE`).

- `transform`:

  ([`objective_transform()`](https://slds-lmu.github.io/vistool/reference/objective_transform.md))
  Optional scalar transformation applied to the objective value.
  Defaults to
  [`objective_transform_identity()`](https://slds-lmu.github.io/vistool/reference/objective_transform.md).

- `label_base`:

  (`character(1)`) Optional base label used when appending transform
  identifiers. Defaults to `label`.

- `...`:

    
  Additional arguments passed to `fun`.

------------------------------------------------------------------------

### Method [`eval()`](https://rdrr.io/r/base/eval.html)

Evaluate the objective function.

#### Usage

    Objective$eval(x)

#### Arguments

- `x`:

  (`numeric`) The numerical input of `fun`.

#### Returns

The result of `fun(x)`.

------------------------------------------------------------------------

### Method `eval_store()`

Evaluate the objective function and log into the archive. Each call logs
the input vector `x`, result of fun `fval`, the gradient `grad`, the
norm of the gradient `gnorm`, and additional logs that were added by
`$add_log_fun`.

#### Usage

    Objective$eval_store(x)

#### Arguments

- `x`:

  (`numeric`) The numerical input of `fun`.

#### Returns

Invisible list of logs that are added to the archive.

------------------------------------------------------------------------

### Method `assert_x()`

Assert a numeric input if it is suitable or not.

#### Usage

    Objective$assert_x(x, ...)

#### Arguments

- `x`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Input value for
  `fun`.

- `...`:

  Additional arguments passed to `assertNumeric(...)`.

------------------------------------------------------------------------

### Method `set_transform()`

Update the active transformation for the objective.

#### Usage

    Objective$set_transform(transform, test = TRUE)

#### Arguments

- `transform`:

  ([`objective_transform()`](https://slds-lmu.github.io/vistool/reference/objective_transform.md))
  Transformation descriptor created via
  [`objective_transform()`](https://slds-lmu.github.io/vistool/reference/objective_transform.md).

- `test`:

  (`logical(1)`) If `TRUE`, validates the transform by evaluating the
  objective at the stored test point.

#### Returns

The Objective invisibly.

------------------------------------------------------------------------

### Method `get_transform()`

Retrieve the active transformation descriptor.

#### Usage

    Objective$get_transform()

#### Returns

An object created by
[`objective_transform()`](https://slds-lmu.github.io/vistool/reference/objective_transform.md).

------------------------------------------------------------------------

### Method `grad()`

Evaluate the gradient of the objective function at x.

#### Usage

    Objective$grad(x)

#### Arguments

- `x`:

  (`numeric`) The numerical input of `fun`.

------------------------------------------------------------------------

### Method `hess()`

Evaluate the hessian of the objective function at x.

#### Usage

    Objective$hess(x)

#### Arguments

- `x`:

  (`numeric`) The numerical input of `fun`.

------------------------------------------------------------------------

### Method `add_log_fun()`

Method to add custom logger to the objective.

#### Usage

    Objective$add_log_fun(l, label)

#### Arguments

- `l`:

  (`function`) Function that returns a single numerical value or a
  string. The arguments of `l` must be `x`, `fval` and `grad`.

- `label`:

  (`character(1)`) The name of the logger.

------------------------------------------------------------------------

### Method `clear_archive()`

Delete the archive.

#### Usage

    Objective$clear_archive()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Objective$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
