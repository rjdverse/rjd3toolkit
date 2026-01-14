# Generating trigonometric variables

Generates trigonometric variables at different frequencies.

## Usage

``` r
trigonometric_variables(frequency, start, length, s, seasonal_frequency = NULL)
```

## Arguments

- frequency:

  Frequency of the series, number of periods per year (12, 4, 3, 2...)

- start, length:

  First date (array with the first year and the first period, for
  instance `c(1980, 1)`) and number of periods of the output variables.
  Can also be provided with the `s` argument

- s:

  time series used to get the dates for the trading days variables. If
  supplied the parameters `frequency`, `start` and `length` are ignored.

- seasonal_frequency:

  the seasonal frequencies. By default the fundamental seasonal
  frequency and all the harmonics are used.

## Value

a mts object with 2 columns

## Details

Denote by \\P\\ the value of `frequency` (= the period) and \\f_1\\,
..., \\f_n\\ the frequencies provides by `seasonal_frequency` (if
`seasonal_frequency = NULL` then \\n=\lfloor P/2\rfloor\\ and
\\f_i\\=i).

`trigonometric_variables` returns a matrix of size \\length\times(2n)\\.

For each date \\t\\ associated to the period \\m\\ (\\m\in\[1,P\]\\),
the columns \\2i\\ and \\2i-1\\ are equal to: \$\$ \cos \left( \frac{2
\pi}{P} \times m \times f_i \right) \text{ and } \sin \left( \frac{2
\pi}{P} \times m \times f_i \right) \$\$ Take for example the case when
the first date (`date`) is a January, `frequency = 12` (monthly time
series), `length = 12` and `seasonal_frequency = NULL`. The first
frequency, \\\lambda_1 = 2\pi /12\\ represents the fundamental seasonal
frequency and the other frequencies (\\\lambda_2 = 2\pi /12 \times 2\\,
..., \\\lambda_6 = 2\pi /12 \times 6\\) are the five harmonics. The
output matrix will be equal to: \$\$ \begin{pmatrix} \cos(\lambda_1) &
\sin (\lambda_1) & \cdots & \cos(\lambda_6) & \sin (\lambda_6) \\
\cos(\lambda_1\times 2) & \sin (\lambda_1\times 2) & \cdots &
\cos(\lambda_6\times 2) & \sin (\lambda_6\times 2)\\ \vdots & \vdots &
\cdots & \vdots & \vdots \\ \cos(\lambda_1\times 12) & \sin
(\lambda_1\times 12) & \cdots & \cos(\lambda_6\times 12) & \sin
(\lambda_6\times 12) \end{pmatrix} \$\$

## Examples

``` r
trigonometric_variables(
    frequency = 12,
    length = 60,
    start = c(2020, 1),
    seasonal_frequency = 12
)
#>          Series 1      Series 2
#> Jan 2020        1 -2.038010e-13
#> Feb 2020        1  2.011429e-15
#> Mar 2020        1 -2.469235e-13
#> Apr 2020        1 -4.111099e-14
#> May 2020        1 -2.900459e-13
#> Jun 2020        1 -8.423342e-14
#> Jul 2020        1 -3.331683e-13
#> Aug 2020        1 -1.273558e-13
#> Sep 2020        1 -3.762907e-13
#> Oct 2020        1 -1.704783e-13
#> Nov 2020        1  3.533420e-14
#> Dec 2020        1 -2.136007e-13
#> Jan 2021        1 -7.788221e-15
#> Feb 2021        1 -2.567231e-13
#> Mar 2021        1 -5.091064e-14
#> Apr 2021        1 -2.998455e-13
#> May 2021        1 -9.403307e-14
#> Jun 2021        1 -3.429680e-13
#> Jul 2021        1 -1.371555e-13
#> Aug 2021        1  6.865697e-14
#> Sep 2021        1 -1.802779e-13
#> Oct 2021        1  2.553455e-14
#> Nov 2021        1 -2.234003e-13
#> Dec 2021        1 -1.758787e-14
#> Jan 2022        1 -2.665228e-13
#> Feb 2022        1 -6.071029e-14
#> Mar 2022        1 -3.096452e-13
#> Apr 2022        1 -1.038327e-13
#> May 2022        1 -3.527676e-13
#> Jun 2022        1 -1.469551e-13
#> Jul 2022        1  5.885732e-14
#> Aug 2022        1 -1.900776e-13
#> Sep 2022        1  1.573490e-14
#> Oct 2022        1 -2.332000e-13
#> Nov 2022        1 -2.738752e-14
#> Dec 2022        1 -2.763224e-13
#> Jan 2023        1 -7.050994e-14
#> Feb 2023        1 -3.194448e-13
#> Mar 2023        1 -1.136324e-13
#> Apr 2023        1 -3.625673e-13
#> May 2023        1 -1.567548e-13
#> Jun 2023        1  4.905767e-14
#> Jul 2023        1 -1.998772e-13
#> Aug 2023        1  5.935251e-15
#> Sep 2023        1 -2.429996e-13
#> Oct 2023        1 -3.718717e-14
#> Nov 2023        1 -2.861221e-13
#> Dec 2023        1 -8.030959e-14
#> Jan 2024        1 -3.292445e-13
#> Feb 2024        1 -1.234320e-13
#> Mar 2024        1 -3.723669e-13
#> Apr 2024        1 -1.665544e-13
#> May 2024        1 -4.154893e-13
#> Jun 2024        1  2.450705e-13
#> Jul 2024        1 -3.864399e-15
#> Aug 2024        1 -2.527993e-13
#> Sep 2024        1 -5.017342e-13
#> Oct 2024        1  1.588256e-13
#> Nov 2024        1 -9.010925e-14
#> Dec 2024        1 -3.390441e-13
```
