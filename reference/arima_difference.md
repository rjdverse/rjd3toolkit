# Remove an arima model from an existing one.

More exactly, m_diff = m_left - m_right iff m_left = m_right + m_diff.

## Usage

``` r
arima_difference(left, right, simplify = TRUE)
```

## Arguments

- left:

  Left operand (JD3_ARIMA object)

- right:

  Right operand (JD3_ARIMA object)

- simplify:

  Simplify the results if possible (common roots in the auto-regressive
  and in the moving average polynomials, including unit roots)

## Value

a `"JD3_ARIMA"` model.

## Examples

``` r
mod1 <- arima_model(delta = c(1, -2, 1))
mod2 <- arima_model(variance = .01)
diff <- arima_difference(mod1, mod2)
sum <- arima_sum(diff, mod2)
# sum should be equal to mod1
```
