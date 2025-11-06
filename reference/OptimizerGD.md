# Gradient descent optimizer

This class defines gradient descent

## Super classes

[`vistool::Optimizer`](https://slds-lmu.github.io/vistool/reference/Optimizer.md)
-\>
[`vistool::OptimizerMomentum`](https://slds-lmu.github.io/vistool/reference/OptimizerMomentum.md)
-\> `OptimizerGD`

## Active bindings

- `momentum`:

  (`numeric(1)`) Momentum of the algorithm.

## Methods

### Public methods

- [`OptimizerGD$new()`](#method-OptimizerGD-new)

- [`OptimizerGD$clone()`](#method-OptimizerGD-clone)

Inherited methods

- [`vistool::Optimizer$prepare_update_for_archive()`](https://slds-lmu.github.io/vistool/reference/Optimizer.html#method-prepare_update_for_archive)
- [`vistool::Optimizer$set_x()`](https://slds-lmu.github.io/vistool/reference/Optimizer.html#method-set_x)
- [`vistool::Optimizer$update_archive()`](https://slds-lmu.github.io/vistool/reference/Optimizer.html#method-update_archive)
- [`vistool::OptimizerMomentum$optimize()`](https://slds-lmu.github.io/vistool/reference/OptimizerMomentum.html#method-optimize)
- [`vistool::OptimizerMomentum$update()`](https://slds-lmu.github.io/vistool/reference/OptimizerMomentum.html#method-update)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OptimizerGD$new(
      objective,
      x_start,
      lr = 0.01,
      id = "Gradient Descent",
      print_trace = TRUE
    )

#### Arguments

- `objective`:

  (`Objective`)  
  The objective to optimize.

- `x_start`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Start value of the optimization. Note, after the first call of
  `$optimize()` the last value is used to continue optimization. Get
  this value with `$x`.

- `lr`:

  (`numeric(1)`)  
  Step size with which the update is multiplied.

- `id`:

  (`character(1)`)  
  Id of the object.

- `print_trace`:

  (`logical(1)`)  
  Indicator whether to print the status of `$optimize()`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OptimizerGD$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
