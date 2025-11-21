# Creates a time series object

Creates a time series object

## Usage

``` r
to_ts(source, id, type = "All")
```

## Arguments

- source:

  Source of the time series

- id:

  Identifier of the time series (source-dependent)

- type:

  Type of the requested information (Data, Metadata...). All by default.

## Value

An object of type "JD3_TS". List containing the identifiers, the data
and the metadata

## Examples

``` r
source <- "Txt"
# id is split due to length restrictions
id1 <- "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&"
id2 <- "file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
id <- paste0(id1, id2)

to_ts(source, id)
#> $name
#> [1] ""
#> 
#> $moniker
#> $source
#> [1] "Txt"
#> 
#> $id
#> [1] "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
#> 
#> attr(,"class")
#> [1] "JD3_TSMONIKER"
#> 
#> $metadata
#> NULL
#> 
#> $data
#> NULL
#> 
#> attr(,"class")
#> [1] "JD3_TS"
```
