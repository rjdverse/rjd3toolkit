# Creates an UCARIMA model, which is composed of ARIMA models with independent innovations.

Creates an UCARIMA model, which is composed of ARIMA models with
independent innovations.

## Usage

``` r
ucarima_model(model = NULL, components, complements = NULL, checkmodel = FALSE)
```

## Arguments

- model:

  The reduced model. Usually not provided.

- components:

  The ARIMA models representing the components

- complements:

  Complements of (some) components. Usually not provided

- checkmodel:

  When the model is provided and *checkmodel* is TRUE, we check that it
  indeed corresponds to the reduced form of the components; similar
  controls are applied on complements. Currently not implemented

## Value

A list with the reduced model, the components and their complements

## Examples

``` r
mod1 <- arima_model("trend", delta = c(1, -2, 1))
mod2 <- arima_model("noise", var = 1600)
hp <- ucarima_model(components = list(mod1, mod2))
print(hp$model)
#> arima 
#> 
#> DIF: 1 -2 1 
#> MA: 1 -1.777091 0.7994438 
#> var:  2001.392 
#> 
```
