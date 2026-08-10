# Complete a Modelling Context with Additional Regressors

Adds a new regressor variable to an existing modelling context, either
creating a new group or adding to an existing group.

## Usage

``` r
complete_modelling_context(
  modelling_context,
  y,
  group = "r",
  name = "",
  overwrite = FALSE
)
```

## Arguments

- modelling_context:

  A modelling context object created by
  [`modelling_context`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.md).
  It's a list of calendars and variables.

- y:

  A regressor. An object to format (`mts`, `ts`, `JD3_TS`,
  `JD3_DYNAMICTS`, `JD3_TSCOLLECTION`, `list`, or other).

- group:

  Character. Name of the group to which the variable should be added. By
  default, the group is "r".

- name:

  Character. Name to assign to the new regressor variable.

- overwrite:

  a Boolean to indicate whether a variable already present should be
  replaced

## Value

The updated modelling context with the new variable added.

## Details

If a variable with the same name already exists in the group and
overwrite is `FALSE`, returns the original modelling context with a
message.

## Examples

``` r
# Assuming we have a modelling context and a new regressor
my_context <- modelling_context()

# Add the new regressor to the context
my_context <- complete_modelling_context(
    modelling_context = my_context,
    y = AirPassengers,
    group = "my_group",
    name = "my_regressor"
)

# Add another regressor to the same group with same name
my_context <- complete_modelling_context(
    modelling_context = my_context,
    y = AirPassengers,
    group = "my_group",
    name = "my_regressor"
)
#> There is already a variable with the same name in the same group.Please change the name of the variable or the name of the group or set `overwrite` to `TRUE`.

# Add another regressor to the same group
my_context <- complete_modelling_context(
    modelling_context = my_context,
    y = ABS,
    group = "my_group",
    name = "another_regressor"
)
#> Replaced forbidden character(s) in 22 name(s).
```
