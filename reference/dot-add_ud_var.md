# Add user-defined variable to a SA model

Add user-defined variable to a SA model

## Usage

``` r
.add_ud_var(x, jx, userdefined = NULL, out_class = NULL, result = FALSE)
```

## Arguments

- x:

  The model of SA

- jx:

  Reference to a Java object

- userdefined:

  vector containing the names of the object to extract.

- out_class:

  Java class of the result object

- result:

  Boolean. Does `jx` contains the results? Default to FALSE.

## Value

A new model with same class as `x`
