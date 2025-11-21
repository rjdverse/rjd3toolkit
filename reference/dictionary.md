# Get Dictionary and Result

Extract dictionary of a `"JD3_ProcResults"` object (`dictionary()`) and
extract a specific value (`result()`) or a list of values
(`user_defined()`).

## Usage

``` r
dictionary(object)

result(object, id)

user_defined(object, userdefined = NULL)
```

## Arguments

- object:

  the java object.

- id:

  the name of the object to extract.

- userdefined:

  vector containing the names of the object to extract.

## Value

the function `dictionary()` returns a character vector with the items
that can be extracted from `object`. The `result()` function extract an
item from the object. The `user_defined()` function do the same thing as
`result()` but can also extract several element at once and encapsulate
the items in a `user_defined` class object.
