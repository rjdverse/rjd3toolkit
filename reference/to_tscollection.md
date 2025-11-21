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
# id is split due to length restrictions
id1 <- "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&"
id2 <- "file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
id <- paste0(id1, id2)
source <- "Txt"
#my_collection <- to_tscollection(source, id)
```
