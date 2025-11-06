# Optimizer class

This class defines the optimization technique.

## Public fields

- `id`:

  (`character(1)`)  
  The id of the object.

- `print_trace`:

  (`logical(1)` Indicator whether to print the status of `$optimize()`.

## Active bindings

- `lr`:

  (`numeric(1`) Step size of the algorithm.

- `archive`:

  ([`data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  Archive of all calls to `$eval_store`.

- `objective`:

  (`Objective`) The objective function.

- `x`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The numerical
  input vector used as starting point by `$optimize()`.

## Methods

### Public methods

- [`Optimizer$new()`](#method-Optimizer-new)

- [`Optimizer$prepare_update_for_archive()`](#method-Optimizer-prepare_update_for_archive)

- [`Optimizer$update_archive()`](#method-Optimizer-update_archive)

- [`Optimizer$set_x()`](#method-Optimizer-set_x)

- [`Optimizer$clone()`](#method-Optimizer-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Optimizer$new(objective, x_start, id = NULL, print_trace = TRUE)

#### Arguments

- `objective`:

  (`Objective`)  
  The objective to optimize.

- `x_start`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Start value of the optimization. Note, after the first call of
  `$optimize()` the last value is used to continue optimization. Get
  this value with `$x`.

- `id`:

  (`character(1)`)  
  Id of the object.

- `print_trace`:

  (`logical(1)`)  
  Indicator whether to print the status of `$optimize()`.

------------------------------------------------------------------------

### Method `prepare_update_for_archive()`

Prepare updates for adding them to the archive.

#### Usage

    Optimizer$prepare_update_for_archive(
      x_out,
      x_in,
      update,
      fval_out,
      fval_in,
      lr,
      step_size,
      objective,
      step,
      ...
    )

#### Arguments

- `x_out`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The new proposed
  point by the optimizer.

- `x_in`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The old input
  value which is updated to `x_out`.

- `update`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) The update from
  `x_in` to `x_out`.

- `fval_out`:

  (`numeric(1)`) The objective value `objetive$eval(x_out)`.

- `fval_in`:

  (`numeric(1)`) The objective value `objetive$eval(x_in)`.

- `lr`:

  (`numeric(1)`) The learning rate used to multiply `update` with.

- `step_size`:

  (`numeric(1)`) The step_size used to multiply `lr * update` with.

- `objective`:

  (`Objective`) The objective used by `$optimize()`.

- `step`:

  (`integer(1)`) The step or iteration.

- `...`:

  Additional objects added to the archive (e.g. `momentum`).

#### Returns

[`data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
of the input arguments.

------------------------------------------------------------------------

### Method `update_archive()`

Add points to the archive.

#### Usage

    Optimizer$update_archive(ain)

#### Arguments

- `ain`:

  [`data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with names "x_out", "x_in", "update", "fval_out", "fval_in", "lr",
  "objective", and "step".

------------------------------------------------------------------------

### Method `set_x()`

Set the current input vector used as start point of `$optimize()`.

#### Usage

    Optimizer$set_x(x)

#### Arguments

- `x`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)) Input vector.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Optimizer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
