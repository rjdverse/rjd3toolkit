# Display names and items from a java (X13) estimation result object

rjd3x13 has to be loaded, `dictionary()` displays the names of all items
contained in a `"JD3_ProcResults"` object, (`result()`) displays the
contents of one item, (`user_defined()`) displays the contents of
several items at once

## Usage

``` r
dictionary(object)

result(object, id)

user_defined(object, userdefined = NULL)
```

## Arguments

- object:

  java object.

- id:

  name of the object to extract.

- userdefined:

  vector containing the names of the objects to extract.

## Value

`dictionary()` returns a character vector with the names of the items
that can be extracted from `object`. `result()` returns a numeric or
character or a ts object (series), `user_defined()` returns an object of
class "user_defined" (list)

## Examples

``` r
# library(rjd3x13)
# dictionary(jsa_x13_estimation)
# tail(result(jsa_x13_estimation, "decomposition.d8" ))
# user_defined
# user_defined(jsa_x13_estimation, userdefined=c("quality.summary","arima.p", "arima.d", "arima.q"))
```
