# Create a Moniker

Create a Moniker

## Usage

``` r
.tsmoniker(source, id)
```

## Arguments

- source:

  Source of the time series.

- id:

  Id of the time series.

## Value

Returns a java object of class JD3_TSMONIKER.

## Examples

``` r
source <- "Txt"
# id is split due to length restrictions
id1 <- "demetra://tsprovider/Txt/20111201/SERIES?datePattern=dd%2FMM%2Fyyyy&delimiter=SEMICOLON&"
id2 <- "file=C%3A%5CDocuments%5CIPI%5CData%5CIPI_nace4.csv#seriesIndex=0"
id <- paste0(id1, id2)
moniker <- .tsmoniker(source, id)
```
