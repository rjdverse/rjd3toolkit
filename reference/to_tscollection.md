# Creates a collection of time series

Creates a collection of time series

## Usage

``` r
to_tscollection(source, id, type = "All")
```

## Arguments

- source:

  Source of the collection of time series

- id:

  Identifier of the collection of time series (source-dependent)

- type:

  Type of the requested information (Data, Metadata...). All by default.

## Value

An object of type "JD3_TSCOLLECTION". List containing the identifiers,
the metadata and all the series (data).

## Examples

``` r
path_data <- system.file("extdata", "IPI_nace4.csv",
                         package = "rjd3workspace")
id <- paste(
    "demetra://tsprovider/Txt/20111201?datePattern=dd%2FMM%2Fyyyy",
    "delimiter=SEMICOLON",
    paste0(
        "file=",
        path_data |>
            normalizePath() |>
            URLencode(reserved = TRUE)
    ),
    sep = "&"
)
#> Warning: path[1]="": No such file or directory
source <- "Txt"
if (nzchar(path_data)) {
    my_collection <- to_tscollection(source, id)
}
```
