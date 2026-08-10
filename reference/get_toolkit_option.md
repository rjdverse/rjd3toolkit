# Get teh value of an option for toolkit

Get teh value of an option for toolkit

## Usage

``` r
get_toolkit_option(name)
```

## Arguments

- name:

  Name of the option

## Value

The requested option or NULL if it doesn't exist

## Examples

``` r
toolkit_option("test", "DUMMY")
get_toolkit_option("test")
#> [1] "DUMMY"
```
