# Retrieve loss function

Retrieve a loss function from the dictionary. If additional parameters
are provided, they will be used to customize the loss function (e.g.,
quantile for pinball loss).

## Usage

``` r
lss(.key, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve the object.

- ...:

  (`any`)  
  Additional arguments.
