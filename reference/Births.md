# Number of births registered in France from 1968 to 2024

Daily number of births recorded in France (metropolitan + DOM), covering
the period from January 1, 1968 to December 31, 2024.

## Usage

``` r
Births
```

## Format

A data frame with 20,820 rows and 2 variables:

- date: Date of the value (from 1968-01-01 to 2024-12-31)

- births: Number of daily births (1254–2830)

## Source

INSEE, Statistiques de l'état civil –
<https://www.insee.fr/fr/statistiques/8582123?sommaire=8582147>

## Details

The dataset corresponds to the INSEE series **T79jnais**. The raw data
can be downloaded as a CSV file here:
<https://www.insee.fr/fr/statistiques/fichier/8582123/T79jnais.csv>

## Examples

``` r
data(Births)
plot(Births$date, Births$births,
     type = "l",
     main = "Daily births in France",
     ylab = "Number of births",
     xlab = "date")

```
